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
handleEvents = foldl (\wm0 e -> fst $ handleEvent wcDefault e wm0)

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
            let (wm1, cs) = handleEvent wcDefault e wm0 in (wm1, test (t wm1 cs) : acc)
     in test . reverse . snd $ foldl go (wmInit, []) ts

wid0, wid1, wid2 :: WinId
wid0 = 0
wid1 = 1
wid2 = 2

win0 :: Win
win0 =
    Win
        { winId = wid0
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        }

win1 :: Win
win1 =
    Win
        { winId = wid1
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        }

win2 :: Win
win2 =
    Win
        { winId = 2
        , winBounds = Bounds 0 10 0 10
        , winMapped = False
        }

wmTwoWindows :: WmState
wmTwoWindows =
    wmBlankState
        { wmWindows = [win0, win1]
        , wmFocusHistory = [wid0, wid1]
        , wmDragResize = Nothing
        }

mapsWindow :: Test
mapsWindow =
    sequenceTests
        wmBlankState
        [
            ( EvWantsMap win0
            , \wm cs ->
                [ "nothing focused" ~: wmFocused wm ~?= Nothing
                , "nothing mapped" ~: wmMappedWindows wm ~?= []
                , "requests map" ~: cs ~?= [ReqManage wid0, ReqMap wid0]
                ]
            )
        ,
            ( EvWasMapped wid0
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
                { winId = 0
                , winBounds = Bounds (-20) 820 (-20) 650
                , winMapped = False
                }
     in sequenceTests
            wm0
            [
                ( EvWantsMap bigWin
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
        (wm1, cs1) = handleEvent wcDefault (EvWasDestroyed wid0) wm0
     in "window desroyed"
            ~: [ "window is gone" ~: findWindow wm1 wid0 ~?= Nothing
               , "no further requests" ~: cs1 ~?= []
               -- todo focus?
               ]

focusFollowsMouse1 :: Test
focusFollowsMouse1 =
    let wm0 = wmTwoWindows
        (wm1, cs1) = handleEvent wcDefault (EvMouseEntered wid1) wm0
        (wm2, cs2) = handleEvent wcDefault (EvFocusIn wid1) wm1
     in "focus change"
            ~: [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
               , "requests new focus" ~: cs1 ~?= [ReqFocus wid1]
               , "window 1 is focused" ~: wmFocused wm2 ~?= Just wid1
               , "requests focus styles" ~: cs2 ~?= [ReqStyleFocused wid1]
               ]

focusFollowsMouse2 :: Test
focusFollowsMouse2 =
    let wm0 = wmTwoWindows
        (wm1, cs1) = handleEvent wcDefault (EvMouseEntered wid0) wm0
     in "no focus change"
            ~: [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
               , "we still emit a request" ~: cs1 ~?= [ReqFocus wid0]
               , "window 0 is still focused" ~: wmFocused wm1 ~?= Just wid0
               ]

dragMove :: Test
dragMove =
    let wm0 = wmBlankState{wmWindows = [mappedWinAt 0 (Bounds 10 100 10 100)]}
        (wm1, cs1) = handleEvent wcDefault (EvDragStart 0 45 45) wm0
        (wm2, cs2) = handleEvent wcDefault (EvDragMove 65 65) wm1
        (wm3, cs3) = handleEvent wcDefault EvDragFinish wm2
        (_, cs4) = handleEvent wcDefault (EvDragMove 99 99) wm3
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
    let (wm1, _) = handleEvent wcDefault (EvDragStart wid x0 y0) wm0
        (_, cs2) = handleEvent wcDefault (EvDragMove x1 y1) wm1
     in cs2 ~?= [ReqMoveResize wid bs1]

-- | Test cases for snapping between two windows.
snap2Wins :: Test
snap2Wins =
    let w1 = 1
        w2 = 2
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
    let w1 = 1
        w2 = 2
        w3 = 3
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
    let w1 = 1
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
                { wmWindows = [mappedWinAt 1 b1, mappedWinAt 2 b2, mappedWinAt 3 b3]
                }
     in "resize de-snap resist"
            ~: [ "left" ~: dragMoveResizeTest wm0 2 (208, 150) (213, 150) b2
               , "right" ~: dragMoveResizeTest wm0 2 (298, 150) (293, 150) b2
               ]

windowResized :: Test
windowResized =
    let wm0 = wmTwoWindows
        bs = Bounds 20 220 30 330
        (wm1, cs1) = handleEvent wcDefault (EvWasResized wid0 bs) wm0
        Just win = findWindow wm1 wid0
        bounds1 = winBounds win
     in "window is resized"
            ~: [ "window has new bounds" ~: bounds1 ~?= bs
               , "no requests" ~: cs1 ~?= []
               ]

noConfigureWhileDragging :: Test
noConfigureWhileDragging =
    let (wm1, _) = handleEvent wcDefault (EvDragStart wid0 0 0) wmTwoWindows
        (_, cs2) = handleEvent wcDefault (EvWantsResize wid0 0 0) wm1
        (_, cs3) = handleEvent wcDefault (EvWantsMove wid0 0 0) wm1
     in "no configures while dragging"
            ~: [ "resize not forwarded" ~: cs2 ~?= []
               , "move not forwarded" ~: cs3 ~?= []
               ]

wm3Windows :: WmState
wm3Windows =
    wmBlankState
        { wmWindows = [win0, win1, win2]
        , wmFocusHistory = [wid0, wid1, wid2]
        , wmDragResize = Nothing
        }

mruFocusSwitching :: Test
mruFocusSwitching =
    let wm0 = handleEvents wm3Windows [EvFocusIn 2, EvFocusIn 1, EvFocusIn 0] -- set up known focus order
     in "mru focus switching"
            ~: sequenceTests
                wm0
                [
                    ( EvCmdFocusNext -- switch
                    , \wm cs ->
                        [ "window 0 is still focused" ~: wmFocused wm ~?= Just 0
                        , "requests focus for 1" ~: cs ~?= [ReqFocus 1, ReqRaise 1]
                        ]
                    )
                ,
                    ( EvFocusIn 1
                    , \wm _ -> ["window 1 now focused" ~: wmFocused wm ~?= Just 1]
                    )
                ,
                    ( EvCmdFocusFinished
                    , \wm cs ->
                        [ "window 1 still focused" ~: wmFocused wm ~?= Just 1
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( EvCmdFocusNext -- switch back
                    , \wm cs ->
                        [ "window 1 is still focused" ~: wmFocused wm ~?= Just 1
                        , "requests focus for 0" ~: cs ~?= [ReqFocus 0, ReqRaise 0]
                        ]
                    )
                ,
                    ( EvFocusIn 0
                    , \wm _ -> ["window 0 now focused" ~: wmFocused wm ~?= Just 0]
                    )
                ,
                    ( EvCmdFocusFinished
                    , \wm cs ->
                        [ "window 0 still focused" ~: wmFocused wm ~?= Just 0
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( EvCmdFocusNext -- double switch
                    , \wm cs ->
                        [ "window 0 is still focused" ~: wmFocused wm ~?= Just 0
                        , "requests focus for 1" ~: cs ~?= [ReqFocus 1, ReqRaise 1]
                        ]
                    )
                ,
                    ( EvFocusIn 1
                    , \wm _ -> ["window 1 now focused" ~: wmFocused wm ~?= Just 1]
                    )
                ,
                    ( EvCmdFocusNext
                    , \wm cs ->
                        [ "window 1 is still focused" ~: wmFocused wm ~?= Just 1
                        , "requests focus for 2" ~: cs ~?= [ReqFocus 2, ReqRaise 2]
                        ]
                    )
                ,
                    ( EvFocusIn 2
                    , \wm _ -> ["window 2 now focused" ~: wmFocused wm ~?= Just 2]
                    )
                ,
                    ( EvCmdFocusFinished
                    , \wm cs ->
                        [ "window 2 still focused" ~: wmFocused wm ~?= Just 2
                        , "no requests after first change" ~: cs ~?= []
                        ]
                    )
                ,
                    ( EvCmdFocusNext -- switch back again
                    , \wm cs ->
                        [ "window 2 is still focused" ~: wmFocused wm ~?= Just 2
                        , "requests focus for 0" ~: cs ~?= [ReqFocus 0, ReqRaise 0]
                        ]
                    )
                ,
                    ( EvFocusIn 0
                    , \wm _ -> ["window 0 now focused" ~: wmFocused wm ~?= Just 0]
                    )
                ,
                    ( EvCmdFocusFinished
                    , \wm cs ->
                        [ "window 0 still focused" ~: wmFocused wm ~?= Just 0
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
                { winId = 0
                , winBounds = Bounds 0 10 0 10
                , winMapped = True
                }
        w1 =
            Win
                { winId = 1
                , winBounds = Bounds 850 900 15 300
                , winMapped = True
                }
        wm0 =
            wmBlankState
                { wmWindows = [w0, w1]
                , wmScreenBounds = [screen0, screen1]
                }
        (wm1, reqs1) = handleEvent wcDefault (EvCmdMaximize 0) wm0
        (_, reqs2) = handleEvent wcDefault (EvCmdMaximize 1) wm1
     in "maximize windows"
            ~: [ "screen 1" ~: reqs1 ~?= [ReqMoveResize 0 screen0]
               , "screen 2" ~: reqs2 ~?= [ReqMoveResize 1 screen1]
               ]

lower :: Test
lower =
    "lower a window"
        ~: sequenceTests
            wm3Windows
            [
                ( EvCmdLower wid0
                , \_ cs -> ["emits lower" ~: cs ~?= [ReqLower wid0, ReqFocus 1]]
                )
            ,
                ( EvFocusIn wid1
                , \wm cs ->
                    [ "lowered window is at the back of focus history" ~: last (wmFocusHistory wm) ~?= wid0
                    , "style the next one focused" ~: cs ~?= [ReqStyleFocused 1]
                    ]
                )
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
        , lower
        ]

main :: IO ()
main = do
    _ <- runTestTT allTests
    return ()
