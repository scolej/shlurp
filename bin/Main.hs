module Main where

import Control.Monad
import Data.Bits
import Data.Maybe
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

import Shlurp

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

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    setDefaultErrorHandler

    d <- openDisplay ""
    let root = defaultRootWindow d

    selectInput
        d
        root
        ( substructureRedirectMask
            .|. substructureNotifyMask
        )

    ( grabButton
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
        )

    -- todo paramterize mask
    let grab sym mask = do
            kc <- keysymToKeycode d sym
            grabKey d kc mask root False grabModeAsync grabModeAsync
    grab xK_q modMask
    grab xK_Tab modMask
    grab xK_space modMask
    grab xK_Super_L 0
    grab xK_Super_R 0

    let cm = defaultColormap d (defaultScreen d)
    focusedColour <- color_pixel . fst <$> allocNamedColor d cm "#4ac6e8"
    unfocusedColour <- color_pixel . fst <$> allocNamedColor d cm "#2c6f82"

    let ro =
            WmReadOnly
                { roDisplay = d
                , roRoot = root
                , roFocusedColour = focusedColour
                , roUnfocusedColour = unfocusedColour
                }
        conf =
            wcDefault
                { wcSnapDist = 20
                , wcSnapGap = 1
                , wcBorderWidth = 2
                }

    (_, _, existingWindows) <- queryTree d root
    mapM_ (manageNewWindow conf ro) existingWindows
    ws <- fmap catMaybes (mapM (newWindow d) existingWindows)

    screenBounds <- map rect2bounds <$> getScreenInfo d

    let wm0 =
            wmBlankState
                { wmConf = conf
                , wmWindows = ws
                , wmScreenBounds = screenBounds
                }

    printDebug wm0

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

{- | Converts an X event to our internal event type.
Also performs any X wrangling to achieve this, eg:
starting and stopping grabs.
-}
convertEvent :: WmReadOnly -> XState -> Event -> IO ([Ev], XState)
convertEvent ro xstate MapRequestEvent{ev_window = w} = do
    win <- newWindow (roDisplay ro) w
    return (maybeToList (fmap EvWantsMap win), xstate)
convertEvent _ xstate MapNotifyEvent{ev_window = w} =
    return ([EvWasMapped w], xstate)
-- todo configurereqeust will happen before map!
-- should we create on create and not maprequest?
-- yes.
-- all other ways of doing it are hacky and crap.
-- eg: if we receive an event for a window we don't have,
-- could emit a 'request create window with full details' request,
-- but then you have to queue up the action you were initially attempting.
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
        } = do
        let move =
                if (fromIntegral vm .&. (cWX .|. cWY)) /= 0
                    then [EvWantsMove win (fromIntegral x) (fromIntegral y)]
                    else []
        -- todo extract a "hasMaskBits" function
        let resize =
                if (fromIntegral vm .&. (cWWidth .|. cWHeight)) /= 0
                    then [EvWantsResize win (fromIntegral w) (fromIntegral h)]
                    else []
        return (move ++ resize, xstate)
convertEvent _ xstate DestroyWindowEvent{ev_window = w} =
    return ([EvWasDestroyed w], xstate)
convertEvent _ xstate CrossingEvent{ev_window = w} =
    return ([EvMouseEntered w], xstate)
convertEvent
    WmReadOnly{roDisplay = d}
    xstate@XState{xsDragState = dragState}
    e0@MotionEvent{} = do
        ev <-
            allocaXEvent
                ( \ep ->
                    let go e = do
                            r <- checkTypedEvent d motionNotify ep
                            putStrLn $ "read another with " ++ show r
                            if r then getEvent ep >>= go else return e
                     in go e0
                )
        let MotionEvent{ev_x = ex, ev_y = ey} = ev
        let x = fromIntegral ex
            y = fromIntegral ey
        return $ case dragState of
            NascentDrag win x0 y0 ->
                if mag (x0, y0) (x, y) > 5 -- todo configurable drag dist threshold
                    then
                        (
                            [ EvDragStart win x0 y0
                            , EvDragMove x y
                            ]
                        , xstate{xsDragState = DragInProgress}
                        )
                    else ([EvDragMove x y], xstate)
            DragInProgress -> ([EvDragMove x y], xstate)
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
        return ([EvWasResized win $ transformBounds x y w h bw], xstate)
convertEvent
    WmReadOnly{roDisplay = d}
    xstate0
    KeyEvent{ev_subwindow = w, ev_keycode = kc, ev_event_type = et} = do
        ks <- keycodeToKeysym d kc 0
        let xstateF = xstate0{xsNakedMod = False}
        let xstateT = xstate0{xsNakedMod = True}
        let down
                | ks == xK_q = return ([EvCmdClose w], xstateF)
                | ks == xK_Tab || ks == xK_space = return ([EvCmdFocusNext], xstateF)
                | ks == xK_grave = return ([EvCmdFocusPrev], xstateF)
                | ks == xK_Super_L || ks == xK_Super_R = do
                    -- todo match modMask
                    return ([], xstateT)
                | otherwise = do
                    return ([], xstateF)
        let up
                | ks == xK_Super_L || ks == xK_Super_R = do
                    -- todo match modMask
                    if xsNakedMod xstate0
                        then do
                            return ([EvCmdFocusNext, EvCmdFocusFinished], xstateF)
                        else do
                            return ([EvCmdFocusFinished], xstateF)
                | otherwise = do
                    return ([], xstate0)
        if et == keyPress
            then down
            else up
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
convertEvent _ xstate FocusChangeEvent{ev_event_type = et, ev_window = w, ev_mode = m} =
    return $
        if m `elem` [notifyGrab, notifyUngrab]
            then ([], xstate)
            else
                if et == focusIn
                    then ([EvFocusIn w], xstate)
                    else
                        if et == focusOut
                            then ([EvFocusOut w], xstate)
                            else ([], xstate)
convertEvent _ xstate ev = do
    putStrLn $ unwords ["converted", show ev, "to nothing"]
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

handleOneEvent :: WmReadOnly -> XState -> Event -> WmState -> IO (WmState, XState)
handleOneEvent ro xstate0 event wm0 = do
    (es, xstate1) <- convertEvent ro xstate0 event
    -- todo extract io?
    let go wm ev = do
            let (wm1, reqs) = handleEvent ev wm
            performReqs (wmConf wm) ro reqs
            return wm1
    wm1 <- foldM go wm0 es
    return (wm1, xstate1)

handleEventsForever :: WmReadOnly -> XState -> WmState -> IO (WmState, XState)
handleEventsForever ro xstate0 wm0 = do
    let d = roDisplay ro
    ev <- allocaXEvent (\ep -> nextEvent d ep >> getEvent ep)
    (wm1, xstate1) <- handleOneEvent ro xstate0 ev wm0
    printDebug wm1
    handleEventsForever ro xstate1 wm1

showWindowBounds :: WmState -> IO ()
showWindowBounds wm =
    let f win =
            let Bounds l r t b = winBounds win
             in unwords [showHex (winId win) "", show [l, t, r - l, b - t]]
     in putStrLn $ unlines $ map f (wmWindows wm)

printDebug :: WmState -> IO ()
printDebug wm = do
    showWindowBounds wm
    putStrLn $
        unwords
            [ "focus history"
            , show $ map (`showHex` "") (wmFocusHistory wm)
            ]
