import Shlurp
import Test.HUnit

-- todo, missing tests
--
-- focus change wrapping
-- backwards focus changing
--
-- screen bounds snap
--
-- fullscreen

handleEvents :: WmState -> [Ev] -> WmState
handleEvents = foldl (\wm0 e -> fst $ e wm0)

-- | Sequence events with assertions at each stage.
sequenceTests ::
    -- | starting state
    WmState ->
    -- | events and assertions
    [(Ev, WmState -> [Request] -> [Test])] ->
    -- | resulting test cases bundled up together
    Test
sequenceTests wmInit ts =
    let go (wm0, acc) (e, t) =
            let (wm1, cs) = e wm0 in (wm1, test (t wm1 cs) : acc)
     in test . reverse . snd $ foldl go (wmInit, []) ts

wid0, wid1, wid2 :: WinId
wid0 = WinId 0
wid1 = WinId 1
wid2 = WinId 2

win0 :: Win
win0 =
    Win
        { winId = wid0
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        , winPrevBounds = Nothing
        }

win1 :: Win
win1 =
    Win
        { winId = wid1
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        , winPrevBounds = Nothing
        }

win2 :: Win
win2 =
    Win
        { winId = (WinId 2)
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        , winPrevBounds = Nothing
        }

wmTwoWindows :: WmState
wmTwoWindows =
    wmBlankState
        { wmWindows = [win0, win1]
        , wmFocusHistory = [wid0, wid1]
        , wmFocused = Just wid0
        , wmDragResize = Nothing
        }

mapsWindow :: Test
mapsWindow =
    sequenceTests
        wmBlankState
        [
            ( evWantsMap win0
            , \wm cs ->
                [ "nothing focused" ~: wmFocused wm ~?= Nothing
                , "nothing mapped" ~: wmMappedWindows wm ~?= []
                , "requests map" ~: cs ~?= [ReqManage wid0, ReqMap wid0]
                ]
            )
        ,
            ( evWasMapped wid0
            , \wm cs ->
                [ "window mapped" ~: wmMappedWindows wm ~?= [wid0]
                , "no requests" ~: cs ~?= []
                ]
            )
        ]

mapsWindowBoundsExceedScreen :: Test
mapsWindowBoundsExceedScreen =
    let wm0 =
            wmBlankState
                { wmWindows = []
                , wmScreenBounds = [Bounds 0 800 0 640]
                }
        bigWin =
            Win
                { winId = WinId 0
                , winBounds = Bounds (-20) 820 (-20) 650
                , winMapped = False
                , winPrevBounds = Nothing
                }
     in sequenceTests
            wm0
            [
                ( evWantsMap bigWin
                , \_ cs ->
                    [ "requests resize then map" ~: cs
                        ~?= [ ReqManage wid0
                            , ReqMoveResize wid0 (Bounds 0 800 0 640)
                            , ReqMap wid0
                            ]
                    ]
                )
            ]

windowDestroyed :: Test
windowDestroyed =
    let wm0 = wmTwoWindows
        (wm1, cs1) = evWasDestroyed wid0 wm0
     in "window desroyed"
            ~: [ "window is gone" ~: findWindow wm1 wid0 ~?= Nothing
               , "no further requests" ~: cs1 ~?= []
               -- todo focus?
               ]

-- todo destroyed while switching

focusFollowsMouse1 :: Test
focusFollowsMouse1 =
    let wm0 = wmTwoWindows
        (wm1, cs1) = evMouseEntered wid1 wm0
        (wm2, cs2) = evFocusIn wid1 wm1
     in "focus change"
            ~: [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
               , "requests new focus" ~: cs1 ~?= [ReqFocus wid1]
               , "window 1 is focused" ~: wmFocused wm2 ~?= Just wid1
               , "requests focus styles" ~: cs2 ~?= [ReqStyleFocused wid1]
               ]

focusFollowsMouse2 :: Test
focusFollowsMouse2 =
    let wm0 = wmTwoWindows
        (wm1, cs1) = evMouseEntered wid0 wm0
     in "no focus change"
            ~: [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
               , "we still emit a request" ~: cs1 ~?= [ReqFocus wid0]
               , "window 0 is still focused" ~: wmFocused wm1 ~?= Just wid0
               ]

dragMove :: Test
dragMove =
    let wm0 = wmBlankState{wmWindows = [mappedWinAt (WinId 0) (Bounds 10 100 10 100)]}
        (wm1, cs1) = evDragStart wcDefault DragMoveResize (WinId 0) 45 45 wm0
        (wm2, cs2) = evDragMove wcDefault 65 65 wm1
        (wm3, cs3) = evDragFinish wm2
        (_, cs4) = evDragMove wcDefault 99 99 wm3
        newBounds = Bounds 30 120 30 120
     in "window can be drag moved"
            ~: [ "no initial requests" ~: cs1 ~?= []
               , "requests move" ~: cs2 ~?= [ReqMoveResize wid0 newBounds]
               , "no final requests" ~: cs3 ~?= []
               , "out of context move has no effect" ~: cs4 ~?= []
               ]

mappedWinAt :: WinId -> Bounds -> Win
mappedWinAt wid bs =
    Win
        { winId = wid
        , winBounds = bs
        , winMapped = True
        , winPrevBounds = Nothing
        }

-- | Makes a drag move/resize test case.
dragMoveResizeTest ::
    -- | initial state
    WmState ->
    -- | id of window to move
    WinId ->
    -- | start drag position
    (Integer, Integer) ->
    -- | end drag position
    (Integer, Integer) ->
    -- | expected final bounds
    Bounds ->
    -- | a test case
    Test
dragMoveResizeTest wm0 wid (x0, y0) (x1, y1) bs1 =
    let (wm1, _) = evDragStart wcDefault DragMoveResize  wid x0 y0 wm0
        (_, cs2) = evDragMove wcDefault x1 y1 wm1
     in cs2 ~?= [ReqMoveResize wid bs1]

-- | Test cases for snapping between two windows.
snap2Wins :: Test
snap2Wins =
    let w1 = (WinId 1)
        w2 = (WinId 2)
        wm0 =
            wmBlankState
                { wmWindows =
                    [ mappedWinAt w1 (Bounds 20 40 100 120)
                    , mappedWinAt w2 (Bounds 300 400 200 300)
                    ]
                }
     in "simple snapping cases"
            ~: [ "window snaps to single opposing edge"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (30, 110)
                        (280, 250)
                        (Bounds 277 297 240 260)
               , "window snaps to single same edge (left)"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (30, 110)
                        (330, 250)
                        (Bounds 300 320 240 260)
                        -- todo other cases:
                        -- multi edge
                        -- multi edge different window
               ]

-- | Test cases for snapping between two windows.
snap3Wins :: Test
snap3Wins =
    let w1 = WinId 1
        w2 = WinId 2
        w3 = WinId 3
        wm0 =
            wmBlankState
                { wmWindows =
                    [ mappedWinAt w1 (Bounds 20 40 100 120)
                    , mappedWinAt w2 (Bounds 300 400 200 500)
                    , mappedWinAt w3 (Bounds 350 500 100 250)
                    ]
                }
     in "slightly trickier snapping cases"
            ~: [ "window snaps to two opposing edges from different windows"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (30, 110)
                        (430, 280)
                        (Bounds 403 423 253 273)
               ]

resizeAWindow :: Test
resizeAWindow =
    let w1 = WinId 1
        wm0 =
            wmBlankState{wmWindows = [mappedWinAt w1 (Bounds 100 200 300 400)]}
     in "single window resize, no snap"
            ~: [ "drag right handle"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (195, 350)
                        (215, 360)
                        (Bounds 100 220 300 400)
               , "drag left handle"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (105, 350)
                        (95, 360)
                        (Bounds 90 200 300 400)
               , "drag top handle"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (150, 305)
                        (160, 295)
                        (Bounds 100 200 290 400)
               , "drag bottom handle"
                    ~: dragMoveResizeTest
                        wm0
                        w1
                        (150, 395)
                        (160, 405)
                        (Bounds 100 200 300 410)
               ]

resizeSnap :: Test
resizeSnap =
    -- Three windows are already perfectly snapped into place.
    -- In each test, we'll dry to de-snap them, and it shouldn't work.
    let b1 = Bounds 100 200 100 200
        b2 = Bounds 203 303 100 200
        b3 = Bounds 306 406 100 200
        wm0 =
            wmBlankState
                { wmWindows = [mappedWinAt (WinId 1) b1, mappedWinAt (WinId 2) b2, mappedWinAt (WinId 3) b3]
                }
     in "resize de-snap resist"
            ~: [ "left" ~: dragMoveResizeTest wm0 (WinId 2) (208, 150) (213, 150) b2
               , "right" ~: dragMoveResizeTest wm0 (WinId 2) (298, 150) (293, 150) b2
               ]

windowResized :: Test
windowResized =
    let wm0 = wmTwoWindows
        bs = Bounds 20 220 30 330
        (wm1, cs1) = evWasResized wid0 bs wm0
        Just win = findWindow wm1 wid0
        bounds1 = winBounds win
     in "window is resized"
            ~: [ "window has new bounds" ~: bounds1 ~?= bs
               , "no requests" ~: cs1 ~?= []
               ]

noConfigureWhileDragging :: Test
noConfigureWhileDragging =
    let (wm1, _) = evDragStart wcDefault DragMoveResize wid0 0 0 wmTwoWindows
        (_, cs2) = evWantsResize wid0 0 0 wm1
        (_, cs3) = evWantsMove wid0 0 0 wm1
     in "no configures while dragging"
            ~: [ "resize not forwarded" ~: cs2 ~?= []
               , "move not forwarded" ~: cs3 ~?= []
               ]

wm3Windows :: WmState
wm3Windows =
    wmBlankState
        { wmWindows = [win0, win1, win2]
        , wmFocusHistory = [wid0, wid1, wid2]
        , wmFocused = Just wid0
        , wmDragResize = Nothing
        }

mruFocusSwitching :: Test
mruFocusSwitching =
    let wm0 =
            handleEvents
                wm3Windows
                [ evFocusIn (WinId 2)
                , evFocusIn (WinId 1)
                , evFocusIn (WinId 0) -- set up known focus order
                ]
     in "mru focus switching"
            ~: sequenceTests
                wm0
                [
                    ( evCmdFocusNext Nothing -- switch
                    , \wm cs ->
                        [ "window 0 is still focused" ~: wmFocused wm ~?= Just (WinId 0)
                        , "requests focus for 1" ~: cs ~?= [ReqFocus (WinId 1), ReqRestack [WinId 1]]
                        ]
                    )
                ,
                    ( evFocusIn (WinId 1)
                    , \wm _ -> ["window 1 now focused" ~: wmFocused wm ~?= Just (WinId 1)]
                    )
                ,
                    ( evCmdFocusFinished
                    , \wm cs ->
                        [ "window 1 still focused" ~: wmFocused wm ~?= Just (WinId 1)
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( evCmdFocusNext Nothing -- switch back
                    , \wm cs ->
                        [ "window 1 is still focused" ~: wmFocused wm ~?= Just (WinId 1)
                        , "requests focus for 0" ~: cs ~?= [ReqFocus (WinId 0), ReqRestack [WinId 0]]
                        ]
                    )
                ,
                    ( evFocusIn (WinId 0)
                    , \wm _ -> ["window 0 now focused" ~: wmFocused wm ~?= Just (WinId 0)]
                    )
                ,
                    ( evCmdFocusFinished
                    , \wm cs ->
                        [ "window 0 still focused" ~: wmFocused wm ~?= Just (WinId 0)
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( evCmdFocusNext Nothing -- double switch
                    , \wm cs ->
                        [ "window 0 is still focused" ~: wmFocused wm ~?= Just (WinId 0)
                        , "requests focus for 1" ~: cs ~?= [ReqFocus (WinId 1), ReqRestack [WinId 1]]
                        ]
                    )
                ,
                    ( evFocusIn (WinId 1)
                    , \wm _ -> ["window 1 now focused" ~: wmFocused wm ~?= Just (WinId 1)]
                    )
                ,
                    ( evCmdFocusNext Nothing
                    , \wm cs ->
                        [ "window 1 is still focused" ~: wmFocused wm ~?= Just (WinId 1)
                        , "requests focus for 2" ~: cs ~?= [ReqFocus (WinId 2), ReqRestack [WinId 2]]
                        ]
                    )
                ,
                    ( evFocusIn (WinId 2)
                    , \wm _ -> ["window 2 now focused" ~: wmFocused wm ~?= Just (WinId 2)]
                    )
                ,
                    ( evCmdFocusFinished
                    , \wm cs ->
                        [ "window 2 still focused" ~: wmFocused wm ~?= Just (WinId 2)
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( evCmdFocusNext Nothing -- switch back again
                    , \wm cs ->
                        [ "window 2 is still focused" ~: wmFocused wm ~?= Just (WinId 2)
                        , "requests focus for 0" ~: cs ~?= [ReqFocus (WinId 0), ReqRestack [WinId 0]]
                        ]
                    )
                ,
                    ( evFocusIn (WinId 0)
                    , \wm _ -> ["window 0 now focused" ~: wmFocused wm ~?= Just (WinId 0)]
                    )
                ,
                    ( evCmdFocusFinished
                    , \wm cs ->
                        [ "window 0 still focused" ~: wmFocused wm ~?= Just (WinId 0)
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ]

maximize :: Test
maximize =
    let screen0 = Bounds 0 800 0 640
        screen1 = Bounds 801 1000 10 500
        w0 =
            Win
                { winId = wid0
                , winBounds = Bounds 0 10 0 10
                , winMapped = True
                , winPrevBounds = Nothing
                }
        w1 =
            Win
                { winId = wid1
                , winBounds = Bounds 850 900 15 300
                , winMapped = True
                , winPrevBounds = Nothing
                }
        wm0 =
            wmBlankState
                { wmWindows = [w0, w1]
                , wmScreenBounds = [screen0, screen1]
                , wmFocusHistory = [wid0, wid1]
                , wmFocused = Just wid0
                }
        (wm1, reqs1) = evCmdMaximize wm0
        (wm2, _) = evFocusIn wid1 wm1
        (_, reqs3) = evCmdMaximize wm2
     in "maximize windows"
            ~: [ "screen 1" ~: reqs1 ~?= [ReqMoveResize (WinId 0) screen0]
               , "screen 2" ~: reqs3 ~?= [ReqMoveResize (WinId 1) screen1]
               ]

lower :: Test
lower =
    "lower a window"
        ~: sequenceTests
            wm3Windows
            [
                ( evCmdLower
                , \_ cs -> ["emits lower" ~: cs ~?= [ReqLower wid0]]
                )
                --            ,
                --                ( EvFocusIn wid1
                --                , \wm cs ->
                --                    [ "lowered window is at the back of focus history" ~: last (wmFocusHistory wm) ~?= wid0
                --                    , "style the next one focused" ~: cs ~?= [ReqStyleFocused (WinId 1)]
                --                    ]
                --                )
            ]

wmSingleWin :: WmState
wmSingleWin =
    wmBlankState
        { wmWindows = [win0]
        , wmFocused = Just wid0
        }

maximizeRestore :: Test
maximizeRestore =
    let bounds0 = Bounds 10 200 20 200
        screen0 = Bounds 0 800 0 640
        screen1 = Bounds 801 1000 10 500
     in "maximize and restore a window"
            ~: sequenceTests
                wmSingleWin
                    { wmScreenBounds = [screen0, screen1]
                    , wmWindows = [win0{winBounds = bounds0}]
                    }
                [ (evCmdToggleMaximize, \_ cs -> ["emits resize for whole screen" ~: cs ~?= [ReqMoveResize wid0 screen0]])
                , (evCmdToggleMaximize, \_ cs -> ["emits resize for orig position" ~: cs ~?= [ReqMoveResize wid0 bounds0]])
                -- now go fullscreen again; manually resize; the next toggle maximize should go whole screen again
                , (evCmdToggleMaximize, \_ cs -> ["emits resize for whole screen" ~: cs ~?= [ReqMoveResize wid0 screen0]])
                , (evCmdScreenProportionalResize wcDefault (0.2, 0.3, 0.2, 0.3), \_ _ -> [])
                , (evCmdToggleMaximize, \_ cs -> ["emits resize for whole screen" ~: cs ~?= [ReqMoveResize wid0 screen0]])
                ]

allTests :: Test
allTests =
    TestList
        [ mapsWindow
        , mapsWindowBoundsExceedScreen
        , windowDestroyed
        , focusFollowsMouse1
        , focusFollowsMouse2
        , dragMove
        , windowResized
        , noConfigureWhileDragging
        , snap2Wins
        , snap3Wins
        , resizeAWindow
        , resizeSnap
        , mruFocusSwitching
        , maximize
        , maximizeRestore
        , lower
        ]

main :: IO ()
main = do
    _ <- runTestTT allTests
    return ()
