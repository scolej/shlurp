module Main where

import Data.Bits
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

data WmReadOnly =
  WmReadOnly { roDisplay :: Display
             , roRoot :: Window
             , roFocusedColour :: Pixel
             , roUnfocusedColour :: Pixel
             }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting..."

  d <- openDisplay "" -- todo use env

  let root = defaultRootWindow d

  selectInput d root
    ( substructureRedirectMask
      .|. substructureNotifyMask )
  xSetErrorHandler

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

  _ <- handleEventsForever ro wm0
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

convertEvent :: WmReadOnly -> Event -> IO (Maybe Ev)
convertEvent ro MapRequestEvent {ev_window = w} = do
  win <- newWindow (roDisplay ro) w
  return $ Just $ EvWantsMap win

convertEvent _ MapNotifyEvent {ev_window = w} =
  return $ Just (EvWasMapped w)

convertEvent _ DestroyWindowEvent {ev_window = w} =
  return $ Just (EvWasDestroyed w)

convertEvent _ CrossingEvent { ev_window = w } =
  return $ Just (EvMouseEntered w)

convertEvent _ MotionEvent { ev_x = x, ev_y = y} =
  return $ Just (EvDragMove (fromIntegral x) (fromIntegral y))

convertEvent _ ConfigureEvent { ev_window = win, ev_x = x, ev_y = y, ev_width = w, ev_height = h, ev_border_width = bw } =
  return $ Just $ EvWasResized win $ transformBounds x y w h bw

-- todo destroy window

-- todo smelly; method is called convert but it does grabs

-- todo click and no drag should raise
-- click and drag should not raise

convertEvent
  WmReadOnly { roDisplay = d , roRoot = r }
  ButtonEvent { ev_window = w, ev_event_type = et, ev_x_root = x, ev_y_root = y }
  | et == buttonPress = do
    let m = pointerMotionMask .|. buttonPressMask .|. buttonReleaseMask
    _ <- grabPointer d r False m grabModeAsync grabModeAsync none none currentTime
    return $ Just (EvDragStart w (fromIntegral x) (fromIntegral y))
  | et == buttonRelease = do
    ungrabPointer d currentTime
    return $ Just EvDragFinish
  | otherwise = return Nothing

convertEvent _ AnyEvent {ev_event_type = et, ev_window = w} = do
  if et == focusIn
    then return $ Just (EvFocusIn w)
    else return Nothing

convertEvent _ _ = return Nothing

manageNewWindow :: WmConfig -> WmReadOnly -> WinId -> IO ()
manageNewWindow wc ro wid = do
  let d = roDisplay ro
  selectInput d wid $ enterWindowMask .|. focusChangeMask
  setWindowBorderWidth d wid (fromIntegral $ wcBorderWidth wc)
  setWindowBorder d wid (roUnfocusedColour ro)
  grabButton d button1 mod1Mask wid False buttonPressMask grabModeAsync grabModeAsync none currentTime

performReqs :: WmConfig -> WmReadOnly -> [Request] -> IO ()
performReqs wc ro = mapM_ go
  where d = roDisplay ro
        go (ReqMap wid) = do
          putStrLn "requesting window map"
          mapWindow d wid
        go (ReqManage wid) = do
          putStrLn "managing window"
          manageNewWindow wc ro wid
        go (ReqFocus wid) = do
          putStrLn "requesting focus"
          setInputFocus d wid revertToParent currentTime
        go (ReqStyleFocused wid) = do
          putStrLn "styling as focused"
          setWindowBorder d wid (roFocusedColour ro)
        go (ReqStyleUnfocused wid) = do
          putStrLn "styling as un-focused"
          setWindowBorder d wid (roUnfocusedColour ro)
        go (ReqResize wid (Bounds l r t b)) = do
          let bw = wcBorderWidth wc
              li = fromIntegral l
              ti = fromIntegral t
              wi = fromIntegral $ r - l - 2 * bw + 1
              hi = fromIntegral $ b - t - 2 * bw + 1
          moveResizeWindow d wid li ti wi hi
        go r = do
          putStrLn $ "unhandled request " ++ show r

handleOneEvent :: WmReadOnly -> Event -> WmState -> IO WmState
handleOneEvent ro event wm0 = do
  mev <- convertEvent ro event
  case mev of
    Just ev -> do
      putStrLn $ "event " ++ show event ++ " converted to " ++ show ev
      let (wm1, reqs) = handleEvent ev wm0
      performReqs (wmConf wm0) ro reqs
      showWindowBounds wm1
      print (wmDragResize wm1)
      return wm1
    Nothing -> do
      putStrLn $ "unhandled event " ++ show event
      return wm0

handleEventsForever :: WmReadOnly -> WmState -> IO WmState
handleEventsForever ro wm0 = do
  let d = roDisplay ro
  ev <- allocaXEvent (\ep -> nextEvent d ep >> getEvent ep)
  wm1 <- handleOneEvent ro ev wm0
  handleEventsForever ro wm1

showWindowBounds :: WmState -> IO ()
showWindowBounds wm =
  let f win = let Bounds l r t b = winBounds win
              in unwords [show $ winId win, show [l, t, r - l, b - t]]
  in putStrLn $ unlines $ map f (wmWindows wm)
