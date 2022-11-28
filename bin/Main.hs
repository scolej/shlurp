module Main where

import Control.Monad.Extra
import Data.Bits
import Data.List
import Data.Maybe
import Data.Time.Format
import Data.Time.LocalTime
import Data.Tuple.Extra
import Debug.Trace
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
import Shlurp
import System.IO
import System.Posix.Process
import System.Process

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
    = -- | trigger an event from a key press
      BindActKey (Event -> Ev)
    | -- | trigger an event which doesn't care about the source window
      BindActWm Ev
    | -- | run an arbitrary IO action
      BindActIO (IO ())
    | -- | binds an action which does IO to produce an event
      BindActEvIO (WmReadOnly -> Event -> IO Ev)

-- todo ^^^
--
-- this is some nasty shit

-- todo
--
-- could have another action type which does IO to create the event,
-- starting to switch could accept the window stacking order as in input,
-- which can then be restored afterwards

config :: WmConfig
config =
    wcDefault
        { wcSnapDist = 20
        , wcSnapGap = 1
        , wcBorderWidth = 2
        }

-- events specialized with config... perhaps a smell todo
evDragStart' = evDragStart config
evDragMove' = evDragMove config
evCmdFullscreen' = evCmdFullscreen config

makeFocusNext :: WmReadOnly -> Event -> IO Ev
makeFocusNext
    WmReadOnly{roDisplay = dsp, roRoot = root}
    KeyEvent{ev_x_root = x, ev_y_root = y} = do
        (_, _, stackOrder) <- queryTree dsp root
        let wids = reverse $ map WinId stackOrder
        logMsg $ "stashed stack order: " ++ show wids
        return $ evCmdFocusNext (Just wids)

bindFocusNext :: BindAction
bindFocusNext = BindActEvIO makeFocusNext

bindFocusNextUnderMouse1 :: BindAction
bindFocusNextUnderMouse1 =
    BindActEvIO $ \WmReadOnly{roDisplay = dsp, roRoot = root} KeyEvent{ev_x_root = x, ev_y_root = y} -> do
        (_, _, stackOrder) <- queryTree dsp root
        let wids = reverse $ map WinId stackOrder
        logMsg $ "stashed stack order: " ++ show wids
        return $ evCmdFocusNextUnderMouse (fromIntegral x, fromIntegral y) (Just wids)

keyBinds :: [(KeySym, KeyMask, BindAction)]
keyBinds =
    [ (xK_Tab, modMask, bindFocusNext)
    , (xK_space, modMask, bindFocusNext)
    , (xK_period, modMask, bindFocusNextUnderMouse1)
    , (xK_grave, modMask, BindActWm evCmdFocusPrev)
    , (xK_comma, modMask, BindActWm evCmdFocusPrev)
    , (xK_q, modMask, BindActWm evCmdClose)
    , (xK_m, modMask, BindActWm evCmdMaximize)
    , (xK_f, modMask, BindActWm evCmdFullscreen')
    , (xK_bracketleft, modMask, BindActWm (evCmdScreenProportionalResize config (0, 0.5, 0, 1)))
    , (xK_bracketright, modMask, BindActWm (evCmdScreenProportionalResize config (0.5, 1.0, 0, 1)))
    , (xK_Escape, modMask, BindActWm evCmdLower)
    , (xK_p, modMask, BindActIO $ void (spawnProcess "dmenu_run" []))
    , (xK_Return, modMask, BindActIO $ void (spawnProcess "alacritty" []))
    , (xK_e, modMask, BindActIO $ void (spawnProcess "e" []))
    , (xK_v, modMask, BindActIO $ void (spawnProcess "pavucontrol" []))
    , (xK_q, modMask .|. shiftMask, BindActIO $ void (executeFile "xshlurp" True [] Nothing))
    ]

grabKeys :: WmReadOnly -> IO ()
grabKeys (WmReadOnly{roDisplay = d, roRoot = root}) = do
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

    grabKeys ro

    (_, _, existingWindows) <- queryTree d root
    mapM_ (manageNewWindow ro . WinId) existingWindows
    ws <- fmap catMaybes (mapM (newWindow d . WinId) existingWindows)

    screenBounds <- map rect2bounds <$> getScreenInfo d

    let wm0 =
            wmBlankState
                { wmWindows = ws
                , wmScreenBounds = screenBounds
                }
    _ <- handleEventsForever ro xInitState wm0

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
    attr <- getWindowAttributes d (wid64 w)
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
convertEvent :: WmReadOnly -> XState -> Event -> IO ([Ev], XState)
convertEvent ro xstate MapRequestEvent{ev_window = w} = do
    win <- newWindow (roDisplay ro) (WinId w)
    return (maybeToList (evWantsMap <$> win), xstate)
convertEvent _ xstate MapNotifyEvent{ev_window = w} =
    return ([evWasMapped (WinId w)], xstate)
convertEvent
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
                    evWantsMove (WinId win) (fromIntegral x) (fromIntegral y)
                , justIf (hasBits [cWWidth, cWHeight] vm) $
                    let t d = fromIntegral d + 2 * wcBorderWidth config - 1
                     in evWantsResize (WinId win) (t w) (t h)
                ]
         in return (catMaybes es, xstate)
convertEvent _ xstate DestroyWindowEvent{ev_window = w} =
    return ([evWasDestroyed (WinId w)], xstate)
convertEvent _ xstate CrossingEvent{ev_window = w} =
    return ([evMouseEntered (WinId w)], xstate)
convertEvent
    _
    xstate@XState{xsDragState = dragState}
    MotionEvent{ev_x = ex, ev_y = ey} = do
        let x = fromIntegral ex
            y = fromIntegral ey
        return $ case dragState of
            NascentDrag win x0 y0 ->
                if mag (x0, y0) (x, y) > (fromIntegral $ wcDragThreshold config)
                    then
                        ( [evDragStart' win x0 y0, evDragMove' x y]
                        , xstate{xsDragState = DragInProgress}
                        )
                    else ([evDragMove' x y], xstate)
            DragInProgress -> ([evDragMove' x y], xstate)
            _ -> ([], xstate)
convertEvent
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
        return ([evWasResized (WinId win) $ transformBounds x y w h bw], xstate)
convertEvent
    ro@WmReadOnly{roDisplay = d}
    xstate0
    event@KeyEvent
        { ev_subwindow = w
        , ev_keycode = kc
        , ev_event_type = et
        , ev_state = km
        } = do
        ks <- keycodeToKeysym d kc 0
        let xstateF = xstate0{xsNakedMod = False}
            xstateT = xstate0{xsNakedMod = True}
            down
                | ks == modKeyL || ks == modKeyR = return ([], xstateT)
                | otherwise =
                    let mba = (\(_, _, a) -> a) <$> find (\(sym, mod, _) -> sym == ks && mod == km) keyBinds
                     in case mba of
                            Nothing -> return ([], xstateF)
                            Just binding -> case binding of
                                BindActKey f -> return ([f event], xstateF)
                                BindActWm ev -> return ([ev], xstateF)
                                BindActIO io -> io >> return ([], xstateF)
                                BindActEvIO io -> io ro event >>= \ev -> return ([ev], xstateF)
            up
                | ks == modKeyL || ks == modKeyR = do
                    if xsNakedMod xstate0
                        then do
                            ev <- makeFocusNext ro event
                            return ([ev, evCmdFocusFinished], xstateF)
                        else do
                            return ([evCmdFocusFinished], xstateF)
                | otherwise = do
                    return ([], xstate0)
        if et == keyPress then down else up
convertEvent
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
                    { xsDragState = NascentDrag (WinId w) (fromIntegral x) (fromIntegral y)
                    , xsNakedMod = False
                    }
                )
        | et == buttonRelease && but == button1 = do
            ungrabPointer d currentTime
            case dragState of
                DragInProgress ->
                    return
                        ( [evDragFinish]
                        , xstate
                            { xsDragState = NoDrag
                            , xsNakedMod = False
                            }
                        )
                _ -> do
                    return ([evMouseClicked (WinId w) 1], xstate{xsDragState = NoDrag, xsNakedMod = False})
        | et == buttonRelease && but == button3 = do
            return ([evMouseClicked (WinId w) 3], xstate)
        | otherwise = do
            return ([], xstate{xsNakedMod = False})
convertEvent _ xstate FocusChangeEvent{ev_event_type = et, ev_window = w, ev_mode = m} =
    let result
            | m `elem` [notifyGrab, notifyUngrab] = ([], xstate)
            | et == focusIn = ([evFocusIn (WinId w)], xstate)
            | et == focusOut = ([evFocusOut (WinId w)], xstate)
            | otherwise = ([], xstate)
     in return result
convertEvent _ xstate _ = do
    return ([], xstate)

manageNewWindow :: WmReadOnly -> WinId -> IO ()
manageNewWindow ro wid = do
    let d = roDisplay ro
    setWindowBorderWidth d (wid64 wid) (fromIntegral $ wcBorderWidth config)
    setWindowBorder d (wid64 wid) (roUnfocusedColour ro)
    selectInput d (wid64 wid) $ enterWindowMask .|. focusChangeMask

performReqs :: WmReadOnly -> [Request] -> IO ()
performReqs ro = mapM_ go
  where
    d = roDisplay ro
    go (ReqMap wid) = mapWindow d (wid64 wid)
    go (ReqManage wid) = manageNewWindow ro wid
    go (ReqFocus wid) = setInputFocus d (wid64 wid) revertToParent currentTime
    go (ReqStyleFocused wid) = setWindowBorder d (wid64 wid) (roFocusedColour ro)
    go (ReqStyleUnfocused wid) = setWindowBorder d (wid64 wid) (roUnfocusedColour ro)
    go (ReqRaise wid) = raiseWindow d (wid64 wid)
    go (ReqLower wid) = lowerWindow d (wid64 wid)
    go (ReqClose wid) = destroyWindow d (wid64 wid)
    go (ReqMoveResize wid (Bounds l r t b)) =
        let bw = wcBorderWidth config
            li = fromIntegral l
            ti = fromIntegral t
            wi = fromIntegral $ r - l - 2 * bw + 1
            hi = fromIntegral $ b - t - 2 * bw + 1
         in moveResizeWindow d (wid64 wid) li ti wi hi
    go (ReqMove wid l t) =
        let li = fromIntegral l
            ti = fromIntegral t
         in moveWindow d (wid64 wid) li ti
    go (ReqResize wid w h) =
        let bw = wcBorderWidth config
            wi = fromIntegral $ w - 2 * bw + 1
            hi = fromIntegral $ h - 2 * bw + 1
         in resizeWindow d (wid64 wid) wi hi
    go (ReqRestack wids) = do
        logMsg $ "restacking: " ++ show wids
        restackWindows d (map wid64 wids)

handleOneEvent :: WmReadOnly -> XState -> Event -> WmState -> IO (WmState, XState)
handleOneEvent ro xstate0 event wm0 = do
    (es, xstate1) <- convertEvent ro xstate0 event
    logMsgLns $ ["event:", show event]
    -- one event can expand to multiple requests,
    -- accumulate them all before actually handling them
    let go (wm, acc) ev = second (acc ++) (ev wm)
        (wm1, reqs) = foldl go (wm0, []) es
    performReqs ro reqs
    return (wm1, xstate1)

handleEventsForever :: WmReadOnly -> XState -> WmState -> IO (WmState, XState)
handleEventsForever ro xstate0 wm0 = do
    let d = roDisplay ro
    let getNextEvent ep = do
            e0 <- nextEvent d ep >> getEvent ep
            -- read off any queued motion notifications so we don't bother with stale ones
            if ev_event_type e0 == motionNotify
                then whileM (checkTypedEvent d motionNotify ep) >> getEvent ep
                else return e0
    ev <- allocaXEvent getNextEvent
    (wm1, xstate1) <- handleOneEvent ro xstate0 ev wm0
    printDebug wm1
    handleEventsForever ro xstate1 wm1

showWindowBounds :: WmState -> IO ()
showWindowBounds wm =
    let wins = wmWindows wm
        f win = unwords [show (winId win), show (winBounds win)]
     in logMsgLns $ "windows and bounds" : (if null wins then ["<none>"] else map f wins)

printDebug :: WmState -> IO ()
printDebug wm = do
    showWindowBounds wm
    logMsg $
        unlines
            [ unwords
                [ "focus history"
                , show $ wmFocusHistory wm
                ]
            , unwords
                [ "focus ring"
                , show $ wmFocusRing wm
                ]
            ]
