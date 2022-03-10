module Shlurp where

import Safe
import Data.List
import Data.Word
import Debug.Trace

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

boundsAdd4 :: Bounds -> (Integer, Integer, Integer, Integer) -> Bounds
boundsAdd4 (Bounds l r t b) (dl, dr, dt, db) =
  Bounds (l + dl) (r + dr) (t + dt) (b + db)

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
  = DragResize
  { drWin :: WinId -- ^ window id being dragged
  , drClickX :: Integer -- ^ x coordinate of where drag started (absolute)
  , drClickY :: Integer -- ^ y coordinate of where drag started (absolute)
  , drHandleClicked :: ResizeHandle -- ^ which part of the window handle was clicked
  , drInitBounds :: Bounds -- ^ initial window bounds
  }

data WmState =
  WmState
  { wmWindows :: [Win] -- ^ all windows
  , wmFocused :: Maybe WinId -- ^ id of the focused window
  , wmDragResize :: Maybe DragResize -- ^ current drag-resize state
  , wmConf :: WmConfig -- ^ configuration
  }

data WmConfig =
  WmConfig
  { wcSnapDist :: Integer -- ^ distance below which snapping occurs
  , wcSnapGap :: Integer -- ^ pixels left in between window borders when snapping
  , wcBorderWidth :: Integer -- ^ width of window borders
  , wcHandleFrac :: Rational -- ^ fraction of window width/height dedicated to resize handles
  }

wcDefault :: WmConfig
wcDefault =
  WmConfig
  { wcSnapDist = 30
  , wcSnapGap = 2
  , wcBorderWidth = 4
  , wcHandleFrac = 0.1
  }

wmBlankState :: WmState
wmBlankState =
  WmState
  { wmWindows = []
  , wmFocused = Nothing
  , wmDragResize = Nothing
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
      hf = wcHandleFrac $ wmConf wm0
      ds = do
        win <- mw
        let bs = winBounds win
            hand = grabWindowHandle hf bs (x, y)
        return $ DragResize wid x y hand bs
  in (wm0 { wmDragResize = ds }, [])

handleEvent (EvDragMove x y) wm0 =
  let ds = wmDragResize wm0
  -- todo if we just maybe for drag resize, can do better here
  in case ds of
    Just (DragResize wid x0 y0 hand origBounds) ->
      let dx = x - x0
          dy = y - y0
          (sl, sr, st, sb) =
            case hand of
              ResizeHandle HL HL -> (1, 0, 1, 0)
              ResizeHandle HL HM -> (1, 0, 0, 0)
              ResizeHandle HL HH -> (1, 0, 0, 1)
              ResizeHandle HM HL -> (0, 0, 1, 0)
              ResizeHandle HM HM -> (1, 1, 1, 1)
              ResizeHandle HM HH -> (0, 0, 0, 1)
              ResizeHandle HH HL -> (0, 1, 1, 0)
              ResizeHandle HH HM -> (0, 1, 0, 0)
              ResizeHandle HH HH -> (0, 1, 0, 1)
          delta4 = (sl * dx, sr * dx, st * dy, sb * dy)
          newBounds = origBounds `boundsAdd4` delta4
          otherWins = filter (\w -> winId w /= wid) $ wmWindows wm0
          otherBounds = map winBounds otherWins -- todo add screen bounds
          snappedBounds = snapBounds (wmConf wm0) otherBounds newBounds
      in (wm0, [ReqResize wid snappedBounds])
    Nothing -> (wm0, [])

handleEvent EvDragFinish wm0 = (wm0 { wmDragResize = Nothing }, [])

handleEvent (EvWasResized wid bounds) wm0 =
  let ws0 = wmWindows wm0
      u w = if winId w == wid
            then w { winBounds = bounds }
            else w
      wm1 = wm0 { wmWindows = map u ws0 }
  in (wm1, [])

handleEvent _ _ = undefined

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

-- todo revise author

-- | Handles: low, middle and high.
data CoHandle = HL | HM | HH

-- | Two co-handles specify one of 9 resize handles.
data ResizeHandle = ResizeHandle CoHandle CoHandle

-- | Decide which part of the window has been gripped based on its bounds,
-- the handle ratio, and the group position.
grabWindowHandle :: Rational -> Bounds -> (Integer, Integer) -> ResizeHandle
grabWindowHandle ratio (Bounds l r t b) (x, y) =
  let w = r - l
      h = b - t
      ratio' = 1 - ratio
      fi = fromIntegral
      x1 = fi l + fi w * ratio
      x2 = fi l + fi w * ratio'
      y1 = fi t + fi h * ratio
      y2 = fi t + fi h * ratio'
      xh | fi x < x1 = HL
         | fi x < x2 = HM
         | otherwise = HH
      yh | fi y < y1 = HL
         | fi y < y2 = HM
         | otherwise = HH
  in ResizeHandle xh yh

absMag :: Integer -> Integer -> Ordering
absMag a b = abs a `compare` abs b

-- | Finds the smallest offset which moves a given value to one of a set of
-- snap values with a limit. If there is no such offset within the limit,
-- then nothing is returned.
maybeSnap
  :: Integer
  -- ^ snap distance
  -> Integer
  -- ^ value to be snapped
  -> [Integer]
  -- ^ snap stops: those values onto which we might snap
  -> Maybe Integer
  -- ^ an offset which might snap the value onto a snap-stop
maybeSnap d val =
  headMay . sortBy absMag . filter (\x -> abs x <= d) . map (\s -> s - val)

-- | Finds the set of snap stops, in x and y, which the "low" edge may snap
-- to, where "low" is left or top.
snapStopsL :: Integer -> [Bounds] -> ([Integer], [Integer])
snapStopsL g bs = (stopsX, stopsY)
  where stopsX = concatMap (\(Bounds l r _ _) -> [l, r + g + 1]) bs
        stopsY = concatMap (\(Bounds _ _ t b) -> [t, b + g + 1]) bs

-- | Finds the set of snap stops, in x and y, which the "high" edge may snap
-- to, where "high" is right or bottom.
snapStopsH :: Integer -> [Bounds] -> ([Integer], [Integer])
snapStopsH g bs = (stopsX, stopsY)
  where stopsX = concatMap (\(Bounds l r _ _) -> [r, l - g - 1]) bs
        stopsY = concatMap (\(Bounds _ _ t b) -> [b, t - g - 1]) bs

-- | Snaps a window's bounds to other bounds or screen edges.
snapBounds
  :: WmConfig
  -- ^ window manager configuration
  -> [Bounds]
  -- ^ bounds to which we might snap
  -> Bounds
  -- ^ bounds to snap
  -> Bounds
  -- ^ potentially snapped bounds or the original bounds unchanged
snapBounds wc otherBounds bs@(Bounds l r t b) =
  let d = wcSnapDist wc
      g = wcSnapGap wc
      (sxl, syl) = snapStopsL g otherBounds
      (sxh, syh) = snapStopsH g otherBounds
      ox = smallestPresent (maybeSnap d l sxl) (maybeSnap d r sxh)
      oy = smallestPresent (maybeSnap d t syl) (maybeSnap d b syh)
      offset = traceShowId (ox, oy)
  in boundsAdd bs offset

-- todo return 4 ints, use add4

-- | Finds the smallest of two values.
smallestPresent :: Maybe Integer -> Maybe Integer -> Integer
smallestPresent Nothing (Just x) = x
smallestPresent (Just x) Nothing = x
smallestPresent (Just a) (Just b) = min a b
smallestPresent _ _ = 0
