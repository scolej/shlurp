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

data WmReadOnly =
  WmReadOnly { roDisplay :: Display
             , roRoot :: Window
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
  let ro = WmReadOnly { roDisplay = d
                      , roRoot = root
                      }
      wm0 = wmBlankState
  handleEventsForever wm0 ro
  closeDisplay d

handleOneEvent :: WmReadOnly -> WmState -> IO (WmState)
handleOneEvent ro wm0 =


handleEventsForever :: WmState -> WmReadOnly -> IO ()
handleEventsForever wm0 ro = do
  let d = (roDisplay ro)
  e <- allocaXEvent (\ep -> nextEvent d ep >> getEvent ep)

  return ()
