module Shlurp where

import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import Safe

type WinId = Word64

data Win = Win
    { winId :: WinId
    , winName :: String
    , winBounds :: Bounds
    , winMapped :: Bool
    }
    deriving (Eq, Show)

data Bounds = Bounds
    { boundsL :: Integer
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

mul4 :: (Integer, Integer) -> (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
mul4 (x, y) (l, r, t, b) = (x * l, x * r, y * t, y * b)

data Ev
    = EvWasMapped WinId
    | EvWasUnmapped WinId
    | EvWantsMap Win
    | EvWasResized WinId Bounds
    | EvWasDestroyed WinId
    | EvFocusIn WinId
    | EvMouseEntered WinId
    | --
      EvDragStart WinId Integer Integer
    | EvDragMove Integer Integer
    | EvDragFinish
    | -- | mouse button clicked on window
      EvMouseClicked WinId Int
    | EvCmdClose WinId
    | --

      -- | switch to the next most recently used window
      EvCmdFocusNext
    | -- | indicate that we've finished switching focus: raise current focused win to top of focus stack
      EvCmdFocusFinished
    deriving (Eq, Show)

--
-- EvKeyTyped Char Int -- ^ character and modifier mask
-- EvMouseClick WinId Int Int Int -- ^ mouse button was pressed and released at absolute x and y

data Request
    = ReqFocus WinId
    | ReqMap WinId
    | -- | do whatever's necessary to begin managing this window
      ReqManage WinId
    | ReqLower WinId
    | ReqRaise WinId
    | ReqResize WinId Bounds
    | -- | todo maybe should collapse to just "style"?
      ReqStyleFocused WinId
    | ReqStyleUnfocused WinId
    | ReqClose WinId
    deriving (Eq, Show)

-- todo there's "state tracking" and "commands"

data DragResize = DragResize
    { -- | window id being dragged
      drWin :: WinId
    , -- | x coordinate of where drag started (absolute)
      drClickX :: Integer
    , -- | y coordinate of where drag started (absolute)
      drClickY :: Integer
    , -- | which part of the window handle was clicked
      drHandleClicked :: ResizeHandle
    , -- | initial window bounds
      drInitBounds :: Bounds
    }
    deriving (Show)

data FocusCycleState = FocusCycleState
    { -- | infinite repetition of original ordering which we drop from as we go
      fcsRing :: [WinId]
    , -- | the original focus list (not infinite) to reinstate when we're done
      fcsOrig :: [WinId]
    }

data WmState = WmState
    { -- | all windows
      wmWindows :: [Win]
    , -- | windows in order of focus history
      wmFocusHistory :: [WinId]
    , -- | if we're currently cycling window focus, this contains the focus ring
      wmFocusRing :: Maybe FocusCycleState
    , -- | current drag-resize state
      wmDragResize :: Maybe DragResize
    , -- | configuration
      wmConf :: WmConfig
    , -- | screen bounds
      wmScreenBounds :: [Bounds]
    }

-- | Finds the currently focused window.
wmFocused :: WmState -> Maybe WinId
wmFocused wm0 = headMay $ wmFocusHistory wm0

data WmConfig = WmConfig
    { -- | distance below which snapping occurs
      wcSnapDist :: Integer
    , -- | pixels left in between window borders when snapping
      wcSnapGap :: Integer
    , -- | width of window borders
      wcBorderWidth :: Integer
    , -- | fraction of window width/height dedicated to resize handles
      wcHandleFrac :: Rational
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
        , wmFocusHistory = []
        , wmFocusRing = Nothing
        , wmDragResize = Nothing
        , wmConf = wcDefault
        , wmScreenBounds = []
        }

findWindow :: WmState -> WinId -> Maybe Win
findWindow wm wid = find (\w -> winId w == wid) (wmWindows wm)

wmMappedWindows :: WmState -> [WinId]
wmMappedWindows wm0 = map winId $ filter winMapped (wmWindows wm0)

{- | Builds a new focus history given a list of windows to put at the front
 of the history. TODO this also needs to remove windows from input which
 don't exist in base history.
-}
focusHistoryNew ::
    -- | existing state
    WmState ->
    -- | front of the new focus history
    [WinId] ->
    -- | new state
    [WinId]
focusHistoryNew wm0 wids =
    let history0 = wmFocusHistory wm0
        rest = map winId (wmWindows wm0)
     in nub $ wids ++ history0 ++ rest

{- | Handle an event. Produces the new window state and any requests which
 should be forwarded.
-}
handleEvent :: Ev -> WmState -> (WmState, [Request])
handleEvent (EvWantsMap w) wm0 =
    let wid = winId w
     in (addWindow w wm0, [ReqManage wid, ReqMap wid])
handleEvent (EvWasMapped wid) wm0 = (setMapped wid wm0, [])
handleEvent (EvWasDestroyed wid) wm0 =
    -- todo remove from ring
    let ws0 = wmWindows wm0
        ws1 = filter (\w -> winId w /= wid) ws0
     in (wm0{wmWindows = ws1}, [])
-- todo it would be nice not to have to do this is focus is already on the
-- window which was entered. but if the window has focus when we start up
-- but haven't got that into our state yet it all breaks...
handleEvent (EvMouseEntered wid) wm0 =
    (wm0, [ReqFocus wid])
handleEvent (EvFocusIn wid) wm0 =
    let maybePrevWid = headMay $ wmFocusHistory wm0
        newHistory = focusHistoryNew wm0 [wid]
        reqs =
            [ReqStyleFocused wid]
                ++ case maybePrevWid of
                    Nothing -> []
                    Just prevWid -> [ReqStyleUnfocused prevWid]
     in (wm0{wmFocusHistory = newHistory}, reqs)
handleEvent (EvDragStart wid x y) wm0 =
    let mw = findWindow wm0 wid
        hf = wcHandleFrac $ wmConf wm0
        ds = do
            win <- mw
            let bs = winBounds win
                hand = grabWindowHandle hf bs (x, y)
            return $ DragResize wid x y hand bs
     in (wm0{wmDragResize = ds}, [])
handleEvent (EvDragMove x y) wm0 =
    let ds = wmDragResize wm0
     in -- todo if we just maybe for drag resize, can do better here
        case ds of
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
                    otherBounds =
                        map winBounds otherWins ++ wmScreenBounds wm0
                    snappedBounds = snapBounds (wmConf wm0) otherBounds (hand == ResizeHandle HM HM) newBounds
                 in (wm0, [ReqResize wid snappedBounds])
            Nothing -> (wm0, [])
handleEvent EvDragFinish wm0 = (wm0{wmDragResize = Nothing}, [])
handleEvent (EvWasResized wid bounds) wm0 =
    let ws0 = wmWindows wm0
        u w =
            if winId w == wid
                then w{winBounds = bounds}
                else w
        wm1 = wm0{wmWindows = map u ws0}
     in (wm1, [])
-- todo this event -> action mapping does not belong here
handleEvent (EvMouseClicked wid button) wm0
    | button == 1 = (wm0, [ReqRaise wid])
    | button == 3 = (wm0, [ReqLower wid])
    | otherwise = (wm0, [])
handleEvent (EvCmdClose wid) wm0 = (wm0, [ReqClose wid])
handleEvent EvCmdFocusNext wm0 =
    let (mnext, wm1) = rotateRing wm0
     in ( wm1
        , case mnext of
            Just next -> [ReqFocus next, ReqRaise next]
            Nothing -> []
        )
handleEvent EvCmdFocusFinished wm0 = (finishFocusChange wm0, [])
handleEvent _ _ = undefined

{- | Finish cycling focus. Recall the focus-history when we started but
 prepend, the newly focused window.
-}
finishFocusChange :: WmState -> WmState
finishFocusChange wm0 =
    let mfr = wmFocusRing wm0
        cur = wmFocusHistory wm0
        reinstatedHistory = case mfr of
            Just fr -> focusHistoryNew wm0 (maybeToList (headMay $ fcsRing fr) ++ fcsOrig fr)
            Nothing -> cur
     in wm0
            { wmFocusRing = Nothing
            , wmFocusHistory = reinstatedHistory
            }

{- | Rotate the focus ring or create it if it doesn't exist yet. Returns
 the new state and the window at the front of the ring.
-}
rotateRing :: WmState -> (Maybe WinId, WmState)
rotateRing wm0 =
    let fr0 =
            fromMaybe
                ( FocusCycleState
                    { fcsRing = cycle $ wmFocusHistory wm0
                    , fcsOrig = wmFocusHistory wm0
                    }
                )
                (wmFocusRing wm0)
        fr = fr0{fcsRing = tail $ fcsRing fr0}
     in (headMay $ fcsRing fr, wm0{wmFocusRing = Just fr})

-- | Add a window to the window list.
addWindow :: Win -> WmState -> WmState
addWindow win wm0 =
    let wins0 = wmWindows wm0
        wins1 = win : wins0
     in wm0{wmWindows = wins1}

-- | Update a window's map-state to mapped.
setMapped :: WinId -> WmState -> WmState
setMapped wid wm0 =
    let wins0 = wmWindows wm0
        wins1 = map f wins0
        f w =
            if winId w == wid
                then w{winMapped = True}
                else w
     in wm0{wmWindows = wins1}

-- todo revise author

-- | Handles: low, middle and high.
data CoHandle = HL | HM | HH
    deriving (Eq, Show)

-- | Two co-handles specify one of 9 resize handles.
data ResizeHandle = ResizeHandle CoHandle CoHandle
    deriving (Eq, Show)

{- | Decide which part of the window has been gripped based on its bounds,
 the handle ratio, and the group position.
-}
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
        xh
            | fi x < x1 = HL
            | fi x < x2 = HM
            | otherwise = HH
        yh
            | fi y < y1 = HL
            | fi y < y2 = HM
            | otherwise = HH
     in ResizeHandle xh yh

absMag :: Integer -> Integer -> Ordering
absMag a b = abs a `compare` abs b

{- | Finds the smallest offset which moves a given value to one of a set of
 snap values with a limit. If there is no such offset within the limit,
 then nothing is returned.
-}
maybeSnap ::
    -- | snap distance
    Integer ->
    -- | value to be snapped
    Integer ->
    -- | snap stops: those values onto which we might snap
    [Integer] ->
    -- | an offset which might snap the value onto a snap-stop
    Maybe Integer
maybeSnap d val =
    headMay . sortBy absMag . filter (\x -> abs x <= d) . map (\s -> s - val)

{- | Finds the set of snap stops, in x and y, which the "low" edge may snap
 to, where "low" is left or top.
-}
snapStopsL :: Integer -> [Bounds] -> ([Integer], [Integer])
snapStopsL g bs = (stopsX, stopsY)
  where
    stopsX = concatMap (\(Bounds l r _ _) -> [l, r + g + 1]) bs
    stopsY = concatMap (\(Bounds _ _ t b) -> [t, b + g + 1]) bs

{- | Finds the set of snap stops, in x and y, which the "high" edge may snap
 to, where "high" is right or bottom.
-}
snapStopsH :: Integer -> [Bounds] -> ([Integer], [Integer])
snapStopsH g bs = (stopsX, stopsY)
  where
    stopsX = concatMap (\(Bounds l r _ _) -> [r, l - g - 1]) bs
    stopsY = concatMap (\(Bounds _ _ t b) -> [b, t - g - 1]) bs

-- | Snaps a window's bounds to other bounds or screen edges.
snapBounds ::
    -- | window manager configuration
    WmConfig ->
    -- | bounds to which we might snap
    [Bounds] ->
    -- | true to maintain bounds width & height
    Bool ->
    -- | bounds to snap
    Bounds ->
    -- | potentially snapped bounds or the original bounds unchanged
    Bounds
snapBounds wc otherBounds fixSize bs@(Bounds l r t b) =
    let d = wcSnapDist wc
        g = wcSnapGap wc
        (sxl, syl) = snapStopsL g otherBounds
        (sxh, syh) = snapStopsH g otherBounds
        snapl = maybeSnap d l sxl
        snapr = maybeSnap d r sxh
        snapt = maybeSnap d t syl
        snapb = maybeSnap d b syh
        offset =
            if fixSize
                then
                    let x = smallestPresent snapl snapr
                        y = smallestPresent snapt snapb
                     in (x, x, y, y)
                else
                    ( fromMaybe 0 snapl
                    , fromMaybe 0 snapr
                    , fromMaybe 0 snapt
                    , fromMaybe 0 snapb
                    )
     in boundsAdd4 bs offset

-- | Finds the smallest of two values.
smallestPresent :: Maybe Integer -> Maybe Integer -> Integer
smallestPresent Nothing (Just x) = x
smallestPresent (Just x) Nothing = x
smallestPresent (Just a) (Just b) = min a b
smallestPresent _ _ = 0
