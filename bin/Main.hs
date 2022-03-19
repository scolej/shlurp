module Main where

import Data.Bits
import Control.Monad
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
import System.IO

import Shlurp

-- | Information we determine a single time at startup and then carry
-- around for the rest of the program.
data WmReadOnly =
  WmReadOnly
  { roDisplay :: Display
  , roRoot :: Window
  , roFocusedColour :: Pixel
  , roUnfocusedColour :: Pixel
  }

-- | State we use for keeping track of what we're doing with X and how it
-- might impact how we translate events for Shlurp.
data XState = XState { xsDragState :: XDragState }

-- | Stages of drag.
data XDragState
  = NoDrag -- ^ there is no drag state at all
  | NascentDrag WinId Integer Integer -- ^ the mouse button is pressed at some x & y (not yet released), this might become a drag, but it might just be a click
  | DragInProgress -- ^ mouse button is down and mouse is moving around, drag is active!

xInitState :: XState
xInitState =
  XState { xsDragState = NoDrag }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting..."

  setDefaultErrorHandler

  d <- openDisplay "" -- todo do we need to use env var?

  let root = defaultRootWindow d

  selectInput d root
    ( substructureRedirectMask
      .|. substructureNotifyMask )

  (grabButton d anyButton mod1Mask root False
    (buttonPressMask .|. buttonReleaseMask .|. button1MotionMask)
    grabModeAsync grabModeAsync none currentTime)

  -- todo paramterize mask
  kc <- keysymToKeycode d xK_q
  grabKey d kc mod1Mask root False grabModeAsync grabModeAsync

  let cm = defaultColormap d (defaultScreen d)
  red <- color_pixel . fst <$> allocNamedColor d cm "#ff0000"
  grey <- color_pixel . fst <$> allocNamedColor d cm "#333333"

  let ro = WmReadOnly
           { roDisplay = d
           , roRoot = root
           , roFocusedColour = red
           , roUnfocusedColour = grey
           }
      conf = wcDefault
             { wcSnapDist = 10
             , wcSnapGap = 1
             , wcBorderWidth = 1
             }

  (_, _, existingWindows) <- queryTree d root
  mapM_ (manageNewWindow conf ro) existingWindows
  ws <- mapM (newWindow d) existingWindows

  screenBounds <- map rect2bounds <$> getScreenInfo d

  let wm0 = wmBlankState
            { wmConf = conf
            , wmWindows = ws
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

-- | Convert X's picture of window position to our idea of bounds.
-- X's window width/height does not include the border width.
transformBounds :: CInt -> CInt -> CInt -> CInt -> CInt -> Bounds
transformBounds x y w h bw =
        Bounds { boundsL = fromIntegral x
               , boundsR = fromIntegral $ x + w + 2 * bw - 1
               , boundsT = fromIntegral y
               , boundsB = fromIntegral $ y + h + 2 * bw - 1
               }

newWindow :: Display -> WinId -> IO Win
newWindow d w = do
  attr <- getWindowAttributes d w
  let bounds = transformBounds (wa_x attr) (wa_y attr) (wa_width attr) (wa_height attr) (wa_border_width attr)
      isMapped = wa_map_state attr == 1
  return $ Win { winId = w
               , winName = "?"
               , winBounds = bounds
               , winMapped = isMapped
               }

mag :: (Integer, Integer) -> (Integer, Integer) -> Float
mag (x0, y0) (x1, y1) =
  let x = x1 - x0
      y = y1 - y0
  in sqrt $ fromIntegral $ x * x + y * y

-- | Converts an X event to our internal event type.
--
-- todo smelly:
--
-- method is called convert but it does grabs
--
-- also, I'm pretty unsure about 1:many event mapping, seems weird
convertEvent :: WmReadOnly -> XState -> Event -> IO ([Ev], XState)

convertEvent ro xstate MapRequestEvent {ev_window = w} = do
  win <- newWindow (roDisplay ro) w
  return ([EvWantsMap win], xstate)

convertEvent _ xstate MapNotifyEvent {ev_window = w} =
  return ([EvWasMapped w], xstate)

convertEvent _ xstate DestroyWindowEvent {ev_window = w} =
  return ([EvWasDestroyed w], xstate)

convertEvent _ xstate CrossingEvent { ev_window = w } =
  return ([EvMouseEntered w], xstate)

convertEvent
  _ xstate@XState { xsDragState = dragState }
  MotionEvent { ev_x = ex, ev_y = ey} = do
  let x = fromIntegral ex
      y = fromIntegral ey
  return $ case dragState of
    NascentDrag win x0 y0 ->
      if mag (x0, y0) (x, y) > 5 -- todo configurable drag dist threshold
      then ( [ EvDragStart win x0 y0
             , EvDragMove x y
             ]
           , xstate { xsDragState = DragInProgress })
      else ([EvDragMove x y], xstate)
    DragInProgress -> ([EvDragMove x y], xstate)
    _ -> ([], xstate)

convertEvent _ xstate
  ConfigureEvent { ev_window = win,
                   ev_x = x, ev_y = y,
                   ev_width = w, ev_height = h,
                   ev_border_width = bw } =
  return ([EvWasResized win $ transformBounds x y w h bw], xstate)

convertEvent
  WmReadOnly { roDisplay = d } xstate
  KeyEvent { ev_subwindow = w , ev_keycode = kc } = do
  ks <- keycodeToKeysym d kc 0
  let r | ks == xK_q = return ([EvCmdClose w] , xstate)
        | otherwise = do
            putStrLn $ unwords ["no binding for", show ks]
            return ([] , xstate)
  r

convertEvent
  WmReadOnly { roDisplay = d , roRoot = r }
  xstate@XState { xsDragState = dragState }
  ButtonEvent { ev_subwindow = w, ev_event_type = et,
                ev_x_root = x, ev_y_root = y,
                ev_button = but }
  | et == buttonPress && but == button1 = do
      let m = pointerMotionMask .|. buttonPressMask .|. buttonReleaseMask
      _ <- grabPointer d r False m grabModeAsync grabModeAsync none none currentTime
      return ( []
             , xstate { xsDragState = NascentDrag w (fromIntegral x) (fromIntegral y) }
             )
  | et == buttonRelease && but == button1 = do
      ungrabPointer d currentTime
      case dragState of
        DragInProgress ->
          return ( [EvDragFinish]
                 , xstate { xsDragState = NoDrag }
                 )
        _ -> do
          return ([EvMouseClicked w 1], xstate { xsDragState = NoDrag })
  | et == buttonRelease && but == button3 = do
      return ([EvMouseClicked w 3], xstate)
  | otherwise = do
      putStrLn "nothing for this click"
      return ([], xstate)

convertEvent _ xstate AnyEvent {ev_event_type = et, ev_window = w} = do
  if et == focusIn
    then return ([EvFocusIn w], xstate)
    else return ([], xstate)

convertEvent _ xstate ev = do
  putStrLn $ unwords ["converted", show ev, "to nothing"]
  return ([], xstate)

-- todo why does it seem that this sometimes fails???
manageNewWindow :: WmConfig -> WmReadOnly -> WinId -> IO ()
manageNewWindow wc ro wid = do
  let d = roDisplay ro
  setWindowBorderWidth d wid (fromIntegral $ wcBorderWidth wc)
  setWindowBorder d wid (roUnfocusedColour ro)
  selectInput d wid $ enterWindowMask .|. focusChangeMask

performReqs :: WmConfig -> WmReadOnly -> [Request] -> IO ()
performReqs wc ro = mapM_ go
  where d = roDisplay ro
        go (ReqMap wid) = mapWindow d wid
        go (ReqManage wid) = manageNewWindow wc ro wid
        go (ReqFocus wid) = setInputFocus d wid revertToParent currentTime
        go (ReqStyleFocused wid) = setWindowBorder d wid (roFocusedColour ro)
        go (ReqStyleUnfocused wid) = setWindowBorder d wid (roUnfocusedColour ro)
        go (ReqRaise wid) = raiseWindow d wid
        go (ReqLower wid) = lowerWindow d wid
        go (ReqClose wid) = destroyWindow d wid -- todo this does not seem to kill the process?
        go (ReqResize wid (Bounds l r t b)) =
          let bw = wcBorderWidth wc
              li = fromIntegral l
              ti = fromIntegral t
              wi = fromIntegral $ r - l - 2 * bw + 1
              hi = fromIntegral $ b - t - 2 * bw + 1
          in moveResizeWindow d wid li ti wi hi

handleOneEvent :: WmReadOnly -> XState -> Event -> WmState -> IO (WmState, XState)
handleOneEvent ro xstate0 event wm0 = do
  (es, xstate1) <- convertEvent ro xstate0 event
  -- todo extract io?
  let go wm0 ev = do
      let (wm1, reqs) = handleEvent ev wm0
      performReqs (wmConf wm0) ro reqs
      return wm1
  wm1 <- foldM go wm0 es
  return (wm1, xstate1)

handleEventsForever :: WmReadOnly -> XState -> WmState -> IO (WmState, XState)
handleEventsForever ro xstate0 wm0 = do
  let d = roDisplay ro
  ev <- allocaXEvent (\ep -> nextEvent d ep >> getEvent ep)
  -- putStrLn $ unwords ["got event", show ev]
  (wm1, xstate1) <- handleOneEvent ro xstate0 ev wm0
  handleEventsForever ro xstate1 wm1

showWindowBounds :: WmState -> IO ()
showWindowBounds wm =
  let f win = let Bounds l r t b = winBounds win
              in unwords [show $ winId win, show [l, t, r - l, b - t]]
  in putStrLn $ unlines $ map f (wmWindows wm)
