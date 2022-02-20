module Shlurp where

import Debug.Trace

import Data.Word
import Data.Time.Clock.System

data Win = Win { winId :: Word64
               , winLastFocus :: SystemTime
               , winName :: String
               , winBounds :: Bounds
               , winMapped :: Bool
               }
  deriving (Eq, Show)

data Bounds = Bounds { boundsL :: Integer
                     , boundsR :: Integer
                     , boundsT :: Integer
                     , boundsB :: Integer
                     }
  deriving (Eq, Show)

data Event = EvWasMapped Win
           | EvWasUnmapped Win
           | EvWantsMap Win
           | EvWantsResize Win Bounds
           | EvWasResized Win Bounds
           | EvFocusIn Win
           -- | EvFocusNext
           -- | EvFocusPrev
           -- | EvFocusNextMRU
           -- | EvFocusLastMRU
           -- | EvFocusConfirm
           -- | EvFocusCancel

data Request = ReqFocus Win
             | ReqUnmap Win
             | ReqMap Win
             | ReqLower Win
             | ReqRaise Win
             | ReqRezie Win Bounds
  deriving (Eq, Show)

-- todo there's "state tracking" and "commands"

data WmState =
  WmState { wmWindows :: [Win]
          , wmFocused :: Maybe Win
          }

wmBlankState :: WmState
wmBlankState =
  WmState { wmWindows = []
          , wmFocused = Nothing
          }

wmMappedWindows :: WmState -> [Win]
wmMappedWindows wm0 = filter winMapped (wmWindows wm0)

-- | Handle an event. Produces the new window state and any requests which
-- should be forwarded.
handleEvent :: Event -> WmState -> (WmState, [Request])
handleEvent (EvWantsMap w) wm0 = (addWindow w wm0, [ReqMap w])
handleEvent (EvWasMapped w) wm0 = (setMapped (winId w) wm0, [])
handleEvent _ _ = undefined

-- todo lens?

-- todo where to use Win and where only win-id?

-- | Add a window to the window list.
addWindow :: Win -> WmState -> WmState
addWindow win wm0 =
  let wins0 = wmWindows wm0
      wins1 = win : wins0
  in wm0 { wmWindows = wins1 }

-- | Update a window's map-state to mapped.
setMapped :: Word64 -> WmState -> WmState
setMapped wid wm0 =
  let wins0 = wmWindows wm0
      wins1 = map f wins0
      f w = if winId w == wid
            then w { winMapped = True }
            else w
  in wm0 { wmWindows = wins1 }
