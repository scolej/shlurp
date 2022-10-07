module Shlurp (
    Bounds (..),
    WinId (..),
    wid64,
    Win (..),
    Ev (..),
    Request (..),
    WmState (..),
    WmConfig (..),
    wcDefault,
    wmBlankState,
    handleEvent,
    wmMappedWindows,
    findWindow,
) where

import Data.List
import Data.Maybe
import Data.Word
import Numeric
import Safe hiding (at)

import Bounds
import Ring

data WinId = WinId Word64
    deriving (Eq)

instance Show WinId where
    show (WinId w) = showHex w ""

wid64 :: WinId -> Word64
wid64 (WinId w) = w

data Win = Win
    { winId :: WinId
    , winBounds :: Bounds
    , winMapped :: Bool
    }
    deriving (Eq, Show)

-- | An event that happened out in the world that we need to respond to.
data Ev
    = EvWasMapped WinId
    | EvWantsMap Win
    | EvWasResized WinId Bounds
    | EvWantsResize WinId Integer Integer
    | EvWantsMove WinId Integer Integer
    | EvWasDestroyed WinId
    | EvFocusIn WinId
    | EvFocusOut WinId
    | EvMouseEntered WinId
    | EvDragStart WinId Integer Integer
    | EvDragMove Integer Integer
    | EvDragFinish
    | -- | mouse button clicked on window
      EvMouseClicked WinId Int
    | EvCmdClose
    | -- | switch to the next most recently used window
      EvCmdFocusNext
    | -- | switch to the previous most recently used window
      EvCmdFocusPrev
    | -- | indicate that we've finished switching focus: raise current focused win to top of focus stack
      EvCmdFocusFinished
    | EvCmdMaximize
    | EvCmdFullscreen
    | EvCmdLower
    deriving (Eq, Show)

{- | A request from window-manager-land to the outside world:
window-manager wants something to happen.
-}
data Request
    = ReqFocus WinId
    | ReqMap WinId
    | -- | do whatever's necessary to begin managing this window
      ReqManage WinId
    | ReqLower WinId
    | ReqRaise WinId
    | ReqMove WinId Integer Integer
    | ReqResize WinId Integer Integer
    | ReqMoveResize WinId Bounds
    | -- | todo maybe should collapse to just "style"?
      ReqStyleFocused WinId
    | ReqStyleUnfocused WinId
    | ReqClose WinId
    deriving (Eq, Show)

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
    { -- | ring of ordering which we can rotate through
      fcsRing :: Ring WinId
    , -- | the original focus list (not infinite) to reinstate when we're done
      fcsOrig :: [WinId]
    }
    deriving (Show)

data WmState = WmState
    { wmFocused :: Maybe WinId
    , wmWindows :: [Win]
    , -- | windows in order of last focus
      wmFocusHistory :: [WinId]
    , -- | if we're currently cycling window focus, this contains the focus ring
      wmFocusRing :: Maybe FocusCycleState
    , -- | current drag-resize state
      wmDragResize :: Maybe DragResize
    , -- | screen bounds
      wmScreenBounds :: [Bounds]
    }

data WmConfig = WmConfig
    { -- | distance below which snapping occurs
      wcSnapDist :: Integer
    , -- | pixels left in between window borders when snapping
      wcSnapGap :: Integer
    , -- | width of window borders
      wcBorderWidth :: Integer
    , -- | fraction of window width/height dedicated to resize handles
      wcHandleFrac :: Rational
    , -- | how many pixels of movement with mouse pressed before a drag starts
      wcDragThreshold :: Integer
    }

wcDefault :: WmConfig
wcDefault =
    WmConfig
        { wcSnapDist = 30
        , wcSnapGap = 2
        , wcBorderWidth = 4
        , wcHandleFrac = 0.1
        , wcDragThreshold = 5
        }

wmBlankState :: WmState
wmBlankState =
    WmState
        { wmFocused = Nothing
        , wmWindows = []
        , wmFocusHistory = []
        , wmFocusRing = Nothing
        , wmDragResize = Nothing
        , wmScreenBounds = []
        }

findWindow :: WmState -> WinId -> Maybe Win
findWindow wm wid =
    find (\w -> winId w == wid) (wmWindows wm)

wmMappedWindows :: WmState -> [WinId]
wmMappedWindows wm0 = map winId $ filter winMapped (wmWindows wm0)

-- | Remove any reference to the given window.
wmForgetWindow :: WinId -> WmState -> WmState
wmForgetWindow wid wm =
    wm
        { wmWindows = filter (\w -> winId w /= wid) (wmWindows wm)
        , wmFocused = (wmFocused wm) >>= (\f -> if f == wid then Nothing else Just f)
        , wmFocusHistory = filter (/= wid) (wmFocusHistory wm)
        , wmFocusRing =
            ( \fr ->
                fr
                    { fcsRing = ringFilter (/= wid) (fcsRing fr)
                    , fcsOrig = filter (/= wid) (fcsOrig fr)
                    }
            )
                <$> (wmFocusRing wm)
        }

{- | Map the window state using the focused window or do nothing if
there is no focused window.
-}
wmWithFocused ::
    (WmState -> WinId -> (WmState, [Request])) ->
    WmState ->
    (WmState, [Request])
wmWithFocused f wm =
    case wmFocused wm of
        Just wid -> f wm wid
        Nothing -> (wm, [])

{- | Builds a new focus history given a list of windows to put at the
front of the history.
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
        allWids = map winId (wmWindows wm0)
        first = filter (`elem` allWids) (wids ++ history0) -- ensure we don't re-instate destroyed windows
     in nub (first ++ allWids)

-- | Ensure a width/height don't become too small.
minSize :: Integer -> Integer -> (Integer, Integer)
minSize w h =
    (max w 20, max h 20) -- todo probably makes sense for it to be configurable

-- | Ensure some bounds don't become too small.
minBounds :: Bounds -> Bounds
minBounds (Bounds l r t b) =
    let (w, h) = minSize (r - l) (b - t)
     in Bounds l (l + w) t (t + h)

-- | Clip any of the seconds bounds' edges to be inside the first bounds' edges.
clipBounds :: Bounds -> Bounds -> Bounds
clipBounds (Bounds al ar at ab) (Bounds bl br bt bb) =
    Bounds
        (if bl < al then al else bl)
        (if br > ar then ar else br)
        (if bt < at then at else bt)
        (if bb > ab then ab else bb)

{- | Handle an event.
Produces the new window state and any requests which should be forwarded.
-}
handleEvent :: WmConfig -> Ev -> WmState -> (WmState, [Request])
handleEvent _ (EvWantsMap win) wm0 =
    let wid = winId win
        wb = winBounds win
        maybeResize = do
            homeScreen <- homeBounds (wmScreenBounds wm0) wb
            if boundsContainsBounds wb homeScreen
                then Nothing
                else Just $ ReqMoveResize wid (clipBounds homeScreen wb)
     in ( addWindow win wm0
        , catMaybes
            [ Just (ReqManage wid)
            , maybeResize
            , Just (ReqMap wid)
            ]
        )
handleEvent _ (EvWasMapped wid) wm0 =
    (setMapped wid wm0, [])
handleEvent _ (EvWasDestroyed wid) wm0 =
    (wmForgetWindow wid wm0, [])
handleEvent _ (EvMouseEntered wid) wm0 =
    (wm0, [ReqFocus wid])
handleEvent _ (EvFocusIn wid) wm0 =
    ( wm0
        { wmFocused = Just wid
        , wmFocusHistory = focusHistoryNew wm0 [wid]
        }
    , [ReqStyleFocused wid]
    )
handleEvent _ (EvFocusOut wid) wm0 =
    (wm0, [ReqStyleUnfocused wid])
handleEvent conf (EvDragStart wid x y) wm0 =
    let mw = findWindow wm0 wid
        hf = wcHandleFrac $ conf
        ds = do
            win <- mw
            let bs = winBounds win
                hand = grabWindowHandle hf bs (x, y)
            return $ DragResize wid x y hand bs
     in (wm0{wmDragResize = ds}, [])
handleEvent conf (EvDragMove x y) wm0 =
    let reqs = maybeToList $ do
            ds <- wmDragResize wm0
            let (DragResize wid x0 y0 hand origBounds) = ds
                dx = x - x0
                dy = y - y0
                (sl, sr, st, sb) = case hand of
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
                snappedBounds =
                    snapWindowBounds
                        conf
                        wm0
                        wid
                        (hand == ResizeHandle HM HM)
                        newBounds
            return $ ReqMoveResize wid (minBounds snappedBounds)
     in (wm0, reqs)
handleEvent _ EvDragFinish wm0 =
    (wm0{wmDragResize = Nothing}, [])
handleEvent _ (EvWasResized wid bounds) wm0 =
    let ws0 = wmWindows wm0
        u w = if winId w == wid then w{winBounds = bounds} else w
        wm1 = wm0{wmWindows = map u ws0}
     in (wm1, [])
handleEvent _ (EvWantsMove wid x y) wm0 =
    let reqs
            | isJust (findWindow wm0 wid) = [] -- if we know about the window, it's been mapped, refuse the move
            | otherwise = [ReqMove wid x y]
     in (wm0, reqs)
handleEvent _ (EvWantsResize wid w h) wm0 =
    let reqs
            | isJust (findWindow wm0 wid) = [] -- if we know about the window, it's been mapped, refuse the resize
            | otherwise =
                let (w', h') = minSize w h
                 in [ReqResize wid w' h']
     in (wm0, reqs)
handleEvent _ EvCmdMaximize wm0 =
    wmWithFocused
        ( \wm wid ->
            let bs = containingScreenBounds wm wid
             in (wm, catMaybes [ReqMoveResize wid <$> bs])
        )
        wm0
handleEvent WmConfig{wcBorderWidth = bw} (EvCmdFullscreen) wm0 =
    wmWithFocused
        ( \wm wid ->
            let req = do
                    (Bounds sl sr st sb) <- containingScreenBounds wm wid
                    let l = sl - bw
                        r = sr + bw
                        t = st - bw
                        b = sb + bw
                    return $ ReqMoveResize wid (Bounds l r t b)
             in (wm, maybeToList req)
        )
        wm0
handleEvent _ EvCmdLower wm0 =
    wmWithFocused
        ( \wm wid ->
            -- we don't change focus history here or send any focus-change
            -- requests; after lowering, if there's a window under the cursor, X
            -- will send us a crossing-event and we'll change focus then.
            --
            -- hmmm kind of indicates that the only time we should ever change focus history is
            -- on focus in/out events, otherwise we mismatch with x. ... not quite true: the only time we change the
            -- _focused window_ is in response to events, we can change up the rest of the history as much as we like.
            -- and this split also neatly solves all the sill `head`s everywhere: we split into focusedWindow and focusHistory
            -- and focusedWindow is in history as well
            --
            -- then lower can still send the acted-on-win to the back of the focus history
            --
            -- still want another bind for cycle mru under mouse
            (wm, [ReqLower wid])
        )
        wm0
-- todo this event -> action mapping does not belong here
handleEvent _ (EvMouseClicked wid button) wm0
    | button == 1 = (wm0, [ReqRaise wid])
    | button == 3 = (wm0, [ReqLower wid])
    | otherwise = (wm0, [])
handleEvent _ EvCmdClose wm0 =
    wmWithFocused
        ( \wm wid ->
            (wm, [ReqClose wid])
        )
        wm0
handleEvent _ EvCmdFocusNext wm0 =
    let wm1 = rotateRing ringRotate $ wmInitFocusRing wm0
        mfoc = ringFocus . fcsRing <$> wmFocusRing wm1
     in ( wm1
        , case mfoc of
            Just foc -> [ReqFocus foc, ReqRaise foc]
            Nothing -> []
        )
handleEvent _ EvCmdFocusPrev wm0 =
    let wm1 = rotateRing ringRotateBack $ wmInitFocusRing wm0
        mfoc = ringFocus . fcsRing <$> wmFocusRing wm1
     in ( wm1
        , case mfoc of
            Just foc -> [ReqFocus foc, ReqRaise foc]
            Nothing -> []
        )
handleEvent _ EvCmdFocusFinished wm0 =
    (finishFocusChange wm0, [])

containingScreenBounds :: WmState -> WinId -> Maybe Bounds
containingScreenBounds wm wid = do
    let screens = wmScreenBounds wm
    win <- findWindow wm wid
    homeBounds screens (winBounds win)

{- | Finds the bounds which "most appropriately" contains another
bounds. For example, the bounds which has the greatest overlap,
or whose centre is closest. The current implementation finds the
bounds which contains the candidate bounds' centre, but it would
be better to do something different so we can appropriately
handle those windows which might map outside any screens' bounds.
todo
-}
homeBounds :: [Bounds] -> Bounds -> Maybe Bounds
homeBounds candidates b =
    let centre = boundsCentre b
     in find (boundsContains centre) candidates

{- | Finish cycling focus.
Recall the focus-history when we started,
but prepend the newly focused window.
-}
finishFocusChange :: WmState -> WmState
finishFocusChange wm0 =
    let reinstatedHistory = case wmFocusRing wm0 of
            Just fr ->
                focusHistoryNew
                    wm0
                    (ringFocus (fcsRing fr) : fcsOrig fr)
            Nothing -> wmFocusHistory wm0
     in wm0
            { wmFocusRing = Nothing
            , wmFocusHistory = reinstatedHistory
            }

rotateRing :: (Ring WinId -> Ring WinId) -> WmState -> WmState
rotateRing f wm0 =
    case do
        let wm1 = wmInitFocusRing wm0
        fcs1 <- wmFocusRing wm1
        let fr2 = f (fcsRing fcs1)
        let fcs2 = fcs1{fcsRing = fr2}
        return wm1{wmFocusRing = Just fcs2} of
        Nothing -> wm0
        Just wm -> wm

wmInitFocusRing :: WmState -> WmState
wmInitFocusRing wm0
    | null (wmFocusHistory wm0) || isJust (wmFocusRing wm0) =
        wm0
    | otherwise =
        let mf =
                FocusCycleState
                    { fcsRing = ringFromList $ wmFocusHistory wm0
                    , fcsOrig = wmFocusHistory wm0
                    }
         in wm0{wmFocusRing = Just mf}

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
        f w = if winId w == wid then w{winMapped = True} else w
     in wm0{wmWindows = wins1}

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

snapWindowBounds :: WmConfig -> WmState -> WinId -> Bool -> Bounds -> Bounds
snapWindowBounds conf wm wid preserveSize bs =
    let otherWins = filter (\w -> winId w /= wid) $ wmWindows wm
        otherBounds = map winBounds otherWins ++ wmScreenBounds wm
     in snapBounds conf otherBounds preserveSize bs

{- | Finds the smallest offset which moves a given value to one of a set of snap values with a limit.
If there is no such offset within the limit, then nothing is returned.
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

{- | Finds the set of snap stops, in x and y,
which the "low" edge may snap to,
where "low" is left or top.
-}
snapStopsL :: Integer -> [Bounds] -> ([Integer], [Integer])
snapStopsL g bs = (stopsX, stopsY)
  where
    stopsX = concatMap (\(Bounds l r _ _) -> [l, r + g + 1]) bs
    stopsY = concatMap (\(Bounds _ _ t b) -> [t, b + g + 1]) bs

{- | Finds the set of snap stops, in x and y,
which the "high" edge may snap to,
where "high" is right or bottom.
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
