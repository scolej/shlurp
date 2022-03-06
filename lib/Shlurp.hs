module Shlurp where

import Safe
import Data.List
import Data.Maybe
import Data.Word

type WinId = Word64

data Win
  = Win { winId :: WinId
        , winName :: String
        , winBounds :: Bounds
        , winMapped :: Bool
        }
  deriving (Eq, Show)

data Bounds
  = Bounds { boundsL :: Integer
           , boundsR :: Integer
           , boundsT :: Integer
           , boundsB :: Integer
           }
  deriving (Eq, Show)

boundsAdd :: Bounds -> (Integer, Integer) -> Bounds
boundsAdd (Bounds l r t b) (x, y) =
  Bounds (l + x) (r + x) (t + y) (b + y)
-- todo DANGER ordering

data Ev
  = EvWasMapped WinId
  | EvWasUnmapped WinId
  | EvWantsMap Win
  | EvWasResized WinId Bounds
  | EvFocusIn WinId
  | EvMouseEntered WinId
  | EvDragStart WinId Integer Integer
  | EvDragMove Integer Integer
  | EvDragFinish
  deriving (Eq, Show)

data Request
  = ReqFocus WinId
  | ReqUnmap WinId
  | ReqMap WinId
  | ReqManage WinId -- ^ do whatever's necessary to begin managing this window
  | ReqLower WinId
  | ReqRaise WinId
  | ReqResize WinId Bounds
  | ReqStyleFocused WinId
  | ReqStyleUnfocused WinId
  deriving (Eq, Show)

-- todo there's "state tracking" and "commands"

data DragResize
  = DragResize WinId Integer Integer Bounds
  | ResizeNone

data WmState =
  WmState
  { wmWindows :: [Win] -- ^ All windows
  , wmFocused :: Maybe WinId -- ^ The ID of the focused window
  , wmDragResize :: DragResize -- ^ The current drag-resize state
  , wmConf :: WmConfig -- ^ Configuration
  }

data WmConfig =
  WmConfig
  { wcSnapDist :: Integer -- ^ Distance below which snapping occurs.
  , wcSnapGap :: Integer -- ^ Pixels left in between window borders when snapping.
  , wcBorderWidth :: Integer -- ^ Width of window borders.
  , wcHandleFrac :: Float -- ^ Fraction of window width/height dedicated to resize handles.
  }

wcDefault :: WmConfig
wcDefault =
  WmConfig
  { wcSnapDist = 30
  , wcSnapGap = 2
  , wcBorderWidth = 4
  , wcHandleFrac = 0.2
  }

wmBlankState :: WmState
wmBlankState =
  WmState
  { wmWindows = []
  , wmFocused = Nothing
  , wmDragResize = ResizeNone
  , wmConf = wcDefault
  }

findWindow :: WmState -> WinId -> Maybe Win
findWindow wm wid = find (\w -> winId w == wid) (wmWindows wm)

wmMappedWindows :: WmState -> [WinId]
wmMappedWindows wm0 = map winId $ filter winMapped (wmWindows wm0)

-- | Handle an event. Produces the new window state and any requests which
-- should be forwarded.
handleEvent :: Ev -> WmState -> (WmState, [Request])

handleEvent (EvWantsMap w) wm0 =
  let wid = winId w
  in (addWindow w wm0, [ReqManage wid, ReqMap wid])

handleEvent (EvWasMapped wid) wm0 = (setMapped wid wm0, [])

handleEvent (EvMouseEntered wid) wm0 =
  (wm0, if wmFocused wm0 == Just wid then [] else [ReqFocus wid])

handleEvent (EvFocusIn wid) wm0 =
  let maybePrevWid = wmFocused wm0
      wm1 = wm0 { wmFocused = Just wid }
      reqs = [ReqStyleFocused wid]
             ++ case maybePrevWid
                of Nothing -> []
                   Just prevWid -> [ReqStyleUnfocused prevWid]
  in (wm1, reqs)

handleEvent (EvDragStart wid x y) wm0 =
  let mw = findWindow wm0 wid
      ds = case mw of Nothing -> ResizeNone
                      Just w -> DragResize wid x y (winBounds w)
  in (wm0 { wmDragResize = ds }, [])

handleEvent (EvDragMove x y) wm0 =
  let ds = wmDragResize wm0
  -- todo if we just maybe for drag resize, can do better here
  in case ds of
    DragResize wid x0 y0 origBounds ->
      let delta = (x - x0, y - y0)
          newBounds = origBounds `boundsAdd` delta
          otherWins = filter (\w -> winId w /= wid) $ wmWindows wm0
          otherBounds = map winBounds otherWins -- todo add screen bounds
          snappedBounds = snapBounds (wmConf wm0) otherBounds newBounds
      in (wm0, [ReqResize wid snappedBounds])
    ResizeNone -> (wm0, [])

handleEvent EvDragFinish wm0 = (wm0 { wmDragResize = ResizeNone }, [])

handleEvent (EvWasResized wid bounds) wm0 =
  let ws0 = wmWindows wm0
      u w = if winId w == wid
            then w { winBounds = bounds }
            else w
      wm1 = wm0 { wmWindows = map u ws0 }
  in (wm1, [])

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
setMapped :: WinId -> WmState -> WmState
setMapped wid wm0 =
  let wins0 = wmWindows wm0
      wins1 = map f wins0
      f w = if winId w == wid
            then w { winMapped = True }
            else w
  in wm0 { wmWindows = wins1 }

snapBs :: Integer -> Integer -> Bounds -> Bounds -> (Integer, Integer)
snapBs d g w0 w1 =
  let s = snap2 d g
      x = s (boundsL w0) (boundsR w0) (boundsL w1) (boundsR w1)
      y = s (boundsT w0) (boundsB w0) (boundsT w1) (boundsB w1)
  in (x, y)

-- todo it doesn't quite work: it means you can only snap to a single
-- window at a time; but in reality, you might be snapping to two edges
-- from two different windows.
snap2
  :: Integer -> Integer
  -> Integer -> Integer
  -> Integer -> Integer
  -> Integer
snap2 d g a1 a2 b1 b2 =
  let g1 = g + 1
      -- maybe: if you snap with sign, it only goes in one direction. but i
      -- think it's not what you want.
      --
      --
      -- pos a = if a >= 0 && a <= d then Just a else Nothing
      -- neg a = if a <= 0 && a >= (-d) then Just a else Nothing
      fil a = if abs a < d then Just a else Nothing
  in headDef 0 $ catMaybes [ fil $ a1 - g1 - b2 -- right edge moves right to meet a left edge
                           , fil $ a2 + g1 - b1 -- left edge moves left to meet a right edge
                           , fil $ a1 - b1 -- left edge moves left to overlap a left edge
                           , fil $ a2 - b2 -- right edge moves right to overlap a right edge
                           ]

snapBounds :: WmConfig -> [Bounds] -> Bounds -> Bounds
snapBounds wc otherBs bs =
  let d = wcSnapDist wc
      g = wcSnapGap wc
      m (x, y) = x * x + y * y
      mc a b = m b `compare` m a
      ss = map (\o -> snapBs d g o bs) otherBs
      sso = sortBy mc ss
      offset = headDef (0, 0) sso
  in boundsAdd bs offset
