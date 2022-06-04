module Main where

import Control.Monad.Extra
import Data.Bits
import Data.List
import Data.Maybe
import Data.Time.Format
import Data.Time.LocalTime
import Data.Tuple.Extra
import Foreign.C.Types
import Graphics.X11.Types
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window
import Numeric
import System.IO
import System.Process

import Shlurp

logMsgLns :: [String] -> IO ()
logMsgLns = logMsg . intercalate "\n"

-- | Log a message with a timestamp
logMsg :: String -> IO ()
logMsg msg = do
    zt <- getZonedTime
    let d = formatTime defaultTimeLocale "%Y-%m-%d %X" zt
    putStrLn $ "----------\n" ++ d ++ " " ++ msg

{- | Information we determine a single time at startup
and then carry around for the rest of the program.
-}
data WmReadOnly = WmReadOnly
    { roDisplay :: Display
    , roRoot :: Window
    , roFocusedColour :: Pixel
    , roUnfocusedColour :: Pixel
    }

{- | State we use for keeping track of what we're doing with X
and how it might impact how we translate events.
-}
data XState = XState
    { xsDragState :: XDragState
    , xsNakedMod :: Bool
    }

-- | Stages of drag.
data XDragState
    = -- | there is no drag state at all
      NoDrag
    | -- | the mouse button is pressed at some x & y (not yet released),
      -- this might become a drag,
      -- but it might just be a click
      NascentDrag WinId Integer Integer
    | -- | mouse button is down and mouse is moving around, drag is active!
      DragInProgress

xInitState :: XState
xInitState =
    XState
        { xsDragState = NoDrag
        , xsNakedMod = False
        }

-- | Mask to use for all WM bindings.
modMask :: KeyMask
modMask = mod4Mask

{- | Key sym for the specified mod-mask.
Must match the mask!
-}
modKeyL, modKeyR :: KeySym
modKeyL = xK_Super_L
modKeyR = xK_Super_R

-- | Different actions which can be bound.
data BindAction
    = -- | trigger an event which doesn't care about the source window
      BindActWm Ev
    | -- | trigger an event using the event's source window
      BindActWin (WinId -> Ev)
    | -- | run an arbitrary IO action
      BindActIO (IO ())

keyBinds :: [(KeySym, KeyMask, BindAction)]
keyBinds =
    [ (xK_Tab, modMask, BindActWm EvCmdFocusNext)
    , (xK_space, modMask, BindActWm EvCmdFocusNext)
    , (xK_grave, modMask, BindActWm EvCmdFocusPrev)
    , (xK_q, modMask, BindActWin EvCmdClose)
    , (xK_m, modMask, BindActWin EvCmdMaximize)
    , (xK_f, modMask, BindActWm EvCmdFullscreen)
    , (xK_Escape, modMask, BindActWm EvCmdLower)
    , (xK_p, modMask, BindActIO $ void (spawnProcess "dmenu_run" []))
    , (xK_Return, modMask, BindActIO $ void (spawnProcess "st" []))
    , (xK_e, modMask, BindActIO $ void (spawnProcess "e" []))
    ]

config :: WmConfig
config =
    wcDefault
        { wcSnapDist = 20
        , wcSnapGap = 3
        , wcBorderWidth = 1
        }

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    setDefaultErrorHandler

    d <- openDisplay ""

    let root = defaultRootWindow d

    selectInput
        d
        root
        (substructureRedirectMask .|. substructureNotifyMask)

    grabButton
        d
        anyButton
        modMask
        root
        False
        (buttonPressMask .|. buttonReleaseMask .|. button1MotionMask)
        grabModeAsync
        grabModeAsync
        none
        currentTime

    let grab sym mask = do
            kc <- keysymToKeycode d sym
            grabKey d kc mask root False grabModeAsync grabModeAsync
        grab1 (sym, mask, _) = grab sym mask

    mapM_ grab1 keyBinds
    grab modKeyL 0
    grab modKeyR 0

    let cm = defaultColormap d (defaultScreen d)
        getColour hex = color_pixel . fst <$> allocNamedColor d cm hex
    focusedColour <- getColour "#94d2bd"
    unfocusedColour <- getColour "#005f73"

    let ro =
            WmReadOnly
                { roDisplay = d
                , roRoot = root
                , roFocusedColour = focusedColour
                , roUnfocusedColour = unfocusedColour
                }

    (_, _, existingWindows) <- queryTree d root
    mapM_ (manageNewWindow config ro) existingWindows
    ws <- fmap catMaybes (mapM (newWindow d) existingWindows)

    screenBounds <- map rect2bounds <$> getScreenInfo d

    let wm0 =
            wmBlankState
                { wmWindows = ws
                , wmScreenBounds = screenBounds
                }
    _ <- handleEventsForever config ro xInitState wm0

    closeDisplay d

rect2bounds :: Rectangle -> Bounds
rect2bounds rect =
    let l = fromIntegral (rect_x rect)
        t = fromIntegral (rect_y rect)
        r = l + fromIntegral (rect_width rect) - 1
        b = t + fromIntegral (rect_height rect) - 1
     in Bounds l r t b

{- | Convert X's picture of window position to our idea of bounds.

The window's width & height as reported by X do not include the border width.
But X's x & y coordinates specify the top left position of the start of the border.
So on-screen, the window occupies a rectangle starting from the reported x & y
and extending to the width + 2 * the border width.
-}
transformBounds :: CInt -> CInt -> CInt -> CInt -> CInt -> Bounds
transformBounds x y w h bw =
    Bounds
        { boundsL = fromIntegral x
        , boundsR = fromIntegral $ x + w + 2 * bw - 1
        , boundsT = fromIntegral y
        , boundsB = fromIntegral $ y + h + 2 * bw - 1
        }

{- | Make some queries and build a new window.
If the window is an "override redirect" window, return nothing.
-}
newWindow :: Display -> WinId -> IO (Maybe Win)
newWindow d w = do
    attr <- getWindowAttributes d w
    if wa_override_redirect attr
        then return Nothing
        else do
            let bounds =
                    transformBounds
                        (wa_x attr)
                        (wa_y attr)
                        (wa_width attr)
                        (wa_height attr)
                        (wa_border_width attr)
                isMapped = wa_map_state attr == 1
            return $
                Just $
                    Win
                        { winId = w
                        , winBounds = bounds
                        , winMapped = isMapped
                        }

mag :: (Integer, Integer) -> (Integer, Integer) -> Float
mag (x0, y0) (x1, y1) =
    let x = x1 - x0
        y = y1 - y0
     in sqrt $ fromIntegral $ x * x + y * y

justIf :: Bool -> a -> Maybe a
justIf True x = Just x
justIf False _ = Nothing

-- | Determines if a mask value contains all of the specified mask bits.
hasBits :: [Mask] -> CULong -> Bool
hasBits ms v = fromIntegral v .&. (foldl (.|.) 0 ms) /= 0

{- | Converts an X event to our internal event type.
Also performs any X wrangling to achieve this, eg:
starting and stopping grabs.
-}
convertEvent :: WmConfig -> WmReadOnly -> XState -> Event -> IO ([Ev], XState)
convertEvent _ ro xstate MapRequestEvent{ev_window = w} = do
    win <- newWindow (roDisplay ro) w
    return (maybeToList (EvWantsMap <$> win), xstate)
convertEvent _ _ xstate MapNotifyEvent{ev_window = w} =
    return ([EvWasMapped w], xstate)
-- todo configurereqeust will happen before map!
-- should we create on create and not maprequest?
-- yes.
-- all other ways of doing it are hacky and crap.
-- eg: if we receive an event for a window we don't have,
-- could emit a 'request create window with full details' request,
-- but then you have to queue up the action you were initially attempting.

convertEvent
    conf
    _
    xstate
    ConfigureRequestEvent
        { ev_window = win
        , ev_x = x
        , ev_y = y
        , ev_width = w
        , ev_height = h
        , ev_value_mask = vm
        } =
        let es =
                [ justIf (hasBits [cWX, cWY] vm) $
                    EvWantsMove win (fromIntegral x) (fromIntegral y)
                , justIf (hasBits [cWWidth, cWHeight] vm) $
                    let t d = fromIntegral d + 2 * wcBorderWidth conf - 1
                     in EvWantsResize win (t w) (t h)
                ]
         in return (catMaybes es, xstate)
convertEvent _ _ xstate DestroyWindowEvent{ev_window = w} =
    return ([EvWasDestroyed w], xstate)
convertEvent _ _ xstate CrossingEvent{ev_window = w} =
    return ([EvMouseEntered w], xstate)
convertEvent
    conf
    _
    xstate@XState{xsDragState = dragState}
    MotionEvent{ev_x = ex, ev_y = ey} = do
        let x = fromIntegral ex
            y = fromIntegral ey
        return $ case dragState of
            NascentDrag win x0 y0 ->
                if mag (x0, y0) (x, y) > (fromIntegral $ wcDragThreshold conf)
                    then
                        ( [EvDragStart win x0 y0, EvDragMove x y]
                        , xstate{xsDragState = DragInProgress}
                        )
                    else ([EvDragMove x y], xstate)
            DragInProgress -> ([EvDragMove x y], xstate)
            _ -> ([], xstate)
convertEvent
    _
    _
    xstate
    ConfigureEvent
        { ev_window = win
        , ev_x = x
        , ev_y = y
        , ev_width = w
        , ev_height = h
        , ev_border_width = bw
        } =
        return ([EvWasResized win $ transformBounds x y w h bw], xstate)
convertEvent
    _
    WmReadOnly{roDisplay = d}
    xstate0
    KeyEvent{ev_subwindow = w, ev_keycode = kc, ev_event_type = et} = do
        ks <- keycodeToKeysym d kc 0
        let xstateF = xstate0{xsNakedMod = False}
            xstateT = xstate0{xsNakedMod = True}
            down
                | ks == modKeyL || ks == modKeyR = return ([], xstateT)
                | otherwise =
                    let mba = (\(_, _, a) -> a) <$> find (\(sym, _, _) -> sym == ks) keyBinds
                     in case mba of
                            Just (BindActWm ev) -> return ([ev], xstateF)
                            Just (BindActWin f) -> return ([f w], xstateF)
                            Just (BindActIO io) -> io >> return ([], xstateF)
                            Nothing -> return ([], xstateF)
            up
                | ks == modKeyL || ks == modKeyR = do
                    if xsNakedMod xstate0
                        then do
                            return ([EvCmdFocusNext, EvCmdFocusFinished], xstateF)
                        else do
                            return ([EvCmdFocusFinished], xstateF)
                | otherwise = do
                    return ([], xstate0)
        if et == keyPress then down else up
convertEvent
    _
    WmReadOnly{roDisplay = d, roRoot = r}
    xstate@XState{xsDragState = dragState}
    ButtonEvent
        { ev_subwindow = w
        , ev_event_type = et
        , ev_x_root = x
        , ev_y_root = y
        , ev_button = but
        }
        | et == buttonPress && but == button1 = do
            let m = pointerMotionMask .|. buttonPressMask .|. buttonReleaseMask
            _ <- grabPointer d r False m grabModeAsync grabModeAsync none none currentTime
            return
                ( []
                , xstate
                    { xsDragState = NascentDrag w (fromIntegral x) (fromIntegral y)
                    , xsNakedMod = False
                    }
                )
        | et == buttonRelease && but == button1 = do
            ungrabPointer d currentTime
            case dragState of
                DragInProgress ->
                    return
                        ( [EvDragFinish]
                        , xstate
                            { xsDragState = NoDrag
                            , xsNakedMod = False
                            }
                        )
                _ -> do
                    return ([EvMouseClicked w 1], xstate{xsDragState = NoDrag, xsNakedMod = False})
        | et == buttonRelease && but == button3 = do
            return ([EvMouseClicked w 3], xstate)
        | otherwise = do
            return ([], xstate{xsNakedMod = False})
convertEvent _ _ xstate FocusChangeEvent{ev_event_type = et, ev_window = w, ev_mode = m} =
    let result
            | m `elem` [notifyGrab, notifyUngrab] = ([], xstate)
            | et == focusIn = ([EvFocusIn w], xstate)
            | et == focusOut = ([EvFocusOut w], xstate)
            | otherwise = ([], xstate)
     in return result
convertEvent _ _ xstate _ = do
    return ([], xstate)

manageNewWindow :: WmConfig -> WmReadOnly -> WinId -> IO ()
manageNewWindow wc ro wid = do
    let d = roDisplay ro
    setWindowBorderWidth d wid (fromIntegral $ wcBorderWidth wc)
    setWindowBorder d wid (roUnfocusedColour ro)
    selectInput d wid $ enterWindowMask .|. focusChangeMask

performReqs :: WmConfig -> WmReadOnly -> [Request] -> IO ()
performReqs wc ro = mapM_ go
  where
    d = roDisplay ro
    go (ReqMap wid) = mapWindow d wid
    go (ReqManage wid) = manageNewWindow wc ro wid
    go (ReqFocus wid) = setInputFocus d wid revertToParent currentTime
    go (ReqStyleFocused wid) = setWindowBorder d wid (roFocusedColour ro)
    go (ReqStyleUnfocused wid) = setWindowBorder d wid (roUnfocusedColour ro)
    go (ReqRaise wid) = raiseWindow d wid
    go (ReqLower wid) = lowerWindow d wid
    go (ReqClose wid) = destroyWindow d wid
    go (ReqMoveResize wid (Bounds l r t b)) =
        let bw = wcBorderWidth wc
            li = fromIntegral l
            ti = fromIntegral t
            wi = fromIntegral $ r - l - 2 * bw + 1
            hi = fromIntegral $ b - t - 2 * bw + 1
         in moveResizeWindow d wid li ti wi hi
    go (ReqMove wid l t) =
        let li = fromIntegral l
            ti = fromIntegral t
         in moveWindow d wid li ti
    go (ReqResize wid w h) =
        let bw = wcBorderWidth wc
            wi = fromIntegral $ w - 2 * bw + 1
            hi = fromIntegral $ h - 2 * bw + 1
         in resizeWindow d wid wi hi

handleOneEvent :: WmConfig -> WmReadOnly -> XState -> Event -> WmState -> IO (WmState, XState)
handleOneEvent conf ro xstate0 event wm0 = do
    (es, xstate1) <- convertEvent conf ro xstate0 event
    let estr = if null es then ["<nothing>"] else (map show es)
    logMsgLns $ ["converted event:", show event, "to:"] ++ estr
    -- one event can expand to multiple requests,
    -- accumulate them all before actually handling them
    let go (wm, acc) ev = second (acc ++) (handleEvent conf ev wm)
        (wm1, reqs) = foldl go (wm0, []) es
    performReqs conf ro reqs
    return (wm1, xstate1)

handleEventsForever :: WmConfig -> WmReadOnly -> XState -> WmState -> IO (WmState, XState)
handleEventsForever conf ro xstate0 wm0 = do
    let d = roDisplay ro
    let getNextEvent ep = do
            e0 <- nextEvent d ep >> getEvent ep
            -- read off any queued motion notifications so we don't bother with stale ones
            if ev_event_type e0 == motionNotify
                then whileM (checkTypedEvent d motionNotify ep) >> getEvent ep
                else return e0
    ev <- allocaXEvent getNextEvent
    (wm1, xstate1) <- handleOneEvent conf ro xstate0 ev wm0
    printDebug wm1
    handleEventsForever conf ro xstate1 wm1

showWindowBounds :: WmState -> IO ()
showWindowBounds wm =
    let wins = wmWindows wm
        f win = unwords [showHex (winId win) "", show (winBounds win)]
     in logMsgLns $ "windows and bounds" : (if null wins then ["<none>"] else map f wins)

printDebug :: WmState -> IO ()
printDebug wm = do
    showWindowBounds wm
    logMsg $
        unwords
            [ "focus history"
            , show $ map (`showHex` "") (wmFocusHistory wm)
            ]
