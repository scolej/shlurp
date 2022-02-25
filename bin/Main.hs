module Main where

import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Cursor
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Screen
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Window
import Data.Bits
import Data.Time.Clock.System

import Shlurp

data WmReadOnly =
  WmReadOnly { roDisplay :: Display
             , roRoot :: Window
             , roFocusedColour :: Pixel
             , roUnfocusedColour :: Pixel
             }

main :: IO ()
main = do
  putStrLn "starting..."
  d <- openDisplay "" -- todo use env
  let root = defaultRootWindow d
  selectInput d root
    ( substructureRedirectMask
      .|. substructureNotifyMask )
  xSetErrorHandler

  let cm = defaultColormap d (defaultScreen d)
  red <- color_pixel . fst <$> allocNamedColor d cm "#ff0000"
  grey <- color_pixel . fst <$> allocNamedColor d cm "#eeeeee"

  let ro = WmReadOnly
           { roDisplay = d
           , roRoot = root
           , roFocusedColour = red
           , roUnfocusedColour = grey
           }
      wm0 = wmBlankState
  handleEventsForever ro wm0
  closeDisplay d

convertEvent :: WmReadOnly -> Event -> IO (Maybe Ev)
convertEvent ro (MapRequestEvent {ev_window = w}) = do
  let d = roDisplay ro
  attr <- getWindowAttributes d w
  let bounds =
        Bounds { boundsL = fromIntegral $ wa_x attr
               , boundsR = fromIntegral $ wa_x attr + wa_width attr
               , boundsT = fromIntegral $ wa_y attr
               , boundsB = fromIntegral $ wa_y attr + wa_height attr
               }
  return $ Just $
    EvWantsMap Win { winId = w
                   , winLastFocus = MkSystemTime 0 0
                   , winName = "?"
                   , winBounds = bounds
                   , winMapped = False
                   }

convertEvent ro (MapNotifyEvent {ev_window = w}) = do
  return $ Just (EvWasMapped w)

convertEvent ro (CrossingEvent {ev_window = w}) = do
  return $ Just (EvMouseEntered w)

convertEvent ro (AnyEvent {ev_event_type = et, ev_window = w}) = do
  if et == focusIn
    then return $ Just (EvFocusIn w)
    else return Nothing

convertEvent _ _ = return Nothing

performReqs :: WmReadOnly -> [Request] -> IO ()
performReqs ro reqs = mapM_ go reqs
  where d = roDisplay ro
        go (ReqMap wid) = do
          putStrLn "requesting window map"
          mapWindow d wid
        go (ReqManage wid) = do
          putStrLn "managing window"
          selectInput d wid $ enterWindowMask .|. focusChangeMask
          setWindowBorderWidth d wid 2
          setWindowBorder d wid (roUnfocusedColour ro)
        go (ReqFocus wid) = do
          putStrLn "requesting focus"
          setInputFocus d wid revertToParent currentTime
        go (ReqStyleFocused wid) = do
          putStrLn "styling as focused"
          setWindowBorder d wid (roFocusedColour ro)
        go (ReqStyleUnfocused wid) = do
          putStrLn "styling as un-focused"
          setWindowBorder d wid (roUnfocusedColour ro)
        go r = do
          putStrLn $ "unhandled request " ++ show r

handleOneEvent :: WmReadOnly -> Event -> WmState -> IO (WmState)
handleOneEvent ro event wm0 = do
  ev <- convertEvent ro event
  case ev of
    Just ev -> do
      putStrLn $ "event " ++ show event ++ " converted to " ++ show ev
      let (wm1, reqs) = handleEvent ev wm0
      performReqs ro reqs
      return wm1
    Nothing -> do
      putStrLn $ "unhandled event " ++ show event
      return wm0

handleEventsForever :: WmReadOnly -> WmState -> IO (WmState)
handleEventsForever ro wm0 = do
  let d = (roDisplay ro)
  ev <- allocaXEvent (\ep -> nextEvent d ep >> getEvent ep)
  wm1 <- handleOneEvent ro ev wm0
  handleEventsForever ro wm1
