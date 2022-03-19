import Test.HUnit
import Shlurp

wid0, wid1, wid2 :: WinId
wid0 = 0
wid1 = 1
wid2 = 2

win0 :: Win
win0 =
  Win { winId = wid0
      , winName = "win 0"
      , winBounds = Bounds 0 10 0 10
      , winMapped = False
      }

win1 :: Win
win1 =
  Win { winId = wid1
      , winName = "win 1"
      , winBounds = Bounds 0 10 0 10
      , winMapped = False
      }

win2 :: Win
win2 =
  Win { winId = 2
      , winName = "win 2"
      , winBounds = Bounds 0 10 0 10
      , winMapped = False
      }

wmTwoWindows :: WmState
wmTwoWindows =
  wmBlankState
  { wmWindows = [win0, win1]
  , wmFocused = Just wid0
  , wmDragResize = Nothing
  }

mapsWindow :: Test
mapsWindow =
  let wm0 = wmBlankState
      (wm1, cs1) = handleEvent (EvWantsMap win0) wm0
      (wm2, cs2) = handleEvent (EvWasMapped wid0) wm1
  in "maps window" ~:
        [ "nothing is focused" ~: wmFocused wm1 ~?= Nothing
        , "no windows mapped" ~: wmMappedWindows wm1 ~?= []
        , "requests to map window" ~: cs1 ~?= [ ReqManage wid0
                                              , ReqMap wid0 ]
        , "window is mapped" ~: wmMappedWindows wm2 ~?= [wid0]
        , "no further requests" ~: cs2 ~?= []
        ]

windowDestroyed :: Test
windowDestroyed =
  let wm0 = wmTwoWindows
      (wm1, cs1) = handleEvent (EvWasDestroyed wid0) wm0
  in "window desroyed" ~:
        [ "window is gone" ~: findWindow wm1 wid0 ~?= Nothing
        , "no further requests" ~: cs1 ~?= []
        -- todo focus?
        ]

focusFollowsMouse1 :: Test
focusFollowsMouse1 =
  let wm0 = wmTwoWindows
      (wm1, cs1) = handleEvent (EvMouseEntered wid1) wm0
      (wm2, cs2) = handleEvent (EvFocusIn wid1) wm1
  in "focus change" ~:
        [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
        , "requests new focus" ~: cs1 ~?= [ReqFocus wid1]
        , "window 1 is focused" ~: wmFocused wm2 ~?= Just wid1
        , "requests focus styles" ~: cs2 ~?= [ReqStyleFocused wid1, ReqStyleUnfocused wid0]
        ]

focusFollowsMouse2 :: Test
focusFollowsMouse2 =
  let wm0 = wmTwoWindows
      (wm1, cs1) = handleEvent (EvMouseEntered wid0) wm0
  in "no focus change" ~:
        [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
        , "we still emit a request" ~: cs1 ~?= [ReqFocus wid0]
        , "window 0 is still focused" ~: wmFocused wm1 ~?= Just wid0
        ]

dragMove :: Test
dragMove =
  let wm0 = wmBlankState
            { wmWindows = [ mappedWinAt 0 (Bounds 10 100 10 100) ]
            }
      (wm1, cs1) = handleEvent (EvDragStart 0 45 45) wm0
      (wm2, cs2) = handleEvent (EvDragMove 65 65) wm1
      (wm3, cs3) = handleEvent EvDragFinish wm2
      (_, cs4) = handleEvent (EvDragMove 99 99) wm3
      newBounds = Bounds 30 120 30 120
  in "window can be drag moved" ~:
        [ "no initial requests" ~: cs1 ~?= []
        , "requests move" ~: cs2 ~?= [ReqResize wid0 newBounds]
        , "no final requests" ~: cs3 ~?= []
        , "out of context move has no effect" ~: cs4 ~?= []
        ]

mappedWinAt :: WinId -> Bounds -> Win
mappedWinAt wid bs =
  Win { winId = wid
      , winName = ""
      , winBounds = bs
      , winMapped = True
      }

-- The window's width & height as reported by X do not include the border
-- width. But the x & y position specify the top left position of the start
-- of the border. So on-screen, the window occupies a rectangle starting
-- from the reported x & y and extending to the width + 2 * the border
-- width.
--
-- But let's keep this shitty situation outside our garden.

-- | Makes a drag move/resize test case.
dragMoveResizeTest
  -- | Initial state
  :: WmState
  -- | ID of window to move
  -> WinId
  -- | Start drag position
  -> (Integer, Integer)
  -- | End drag position
  -> (Integer, Integer)
  -- | Expected final bounds
  -> Bounds
  -- | A test case
  -> Test
dragMoveResizeTest wm0 wid (x0, y0) (x1, y1) bs1 =
  let (wm1, _) = handleEvent (EvDragStart wid x0 y0) wm0
      (_, cs2) = handleEvent (EvDragMove x1 y1) wm1
  in cs2 ~?= [ReqResize wid bs1]

-- | Test cases for snapping between two windows.
snap2Wins :: Test
snap2Wins =
  let w1 = 1
      w2 = 2
      wm0 = wmBlankState
            { wmWindows = [ mappedWinAt w1 (Bounds 20 40 100 120)
                          , mappedWinAt w2 (Bounds 300 400 200 300)
                          ]
            }
  in "simple snapping cases" ~:
     [ "window snaps to single opposing edge"
       ~: dragMoveResizeTest wm0 w1 (30, 110) (280, 250) (Bounds 277 297 240 260)
     , "window snaps to single same edge (left)"
       ~: dragMoveResizeTest wm0 w1 (30, 110) (330, 250) (Bounds 300 320 240 260)
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
      wm0 = wmBlankState
            { wmWindows = [ mappedWinAt w1 (Bounds 20 40 100 120)
                          , mappedWinAt w2 (Bounds 300 400 200 500)
                          , mappedWinAt w3 (Bounds 350 500 100 250)
                          ]
            }
  in "slightly trickier snapping cases" ~:
     [ "window snaps to two opposing edges from different windows"
       ~: dragMoveResizeTest wm0 w1 (30, 110) (430, 280) (Bounds 403 423 253 273)
     ]

-- todo screen bounds snap

resizeAWindow :: Test
resizeAWindow =
  let w1 = 1
      wm0 = wmBlankState { wmWindows = [ mappedWinAt w1 (Bounds 100 200 300 400) ]}
  in "single window resize, no snap" ~:
     [ "drag right handle"
       ~: dragMoveResizeTest wm0 w1 (195, 350) (215, 360) (Bounds 100 220 300 400)
     , "drag left handle"
       ~: dragMoveResizeTest wm0 w1 (105, 350) (95, 360) (Bounds 90 200 300 400)
     , "drag top handle"
       ~: dragMoveResizeTest wm0 w1 (150, 305) (160, 295) (Bounds 100 200 290 400)
     , "drag bottom handle"
       ~: dragMoveResizeTest wm0 w1 (150, 395) (160, 405) (Bounds 100 200 300 410)
     ]

resizeSnap :: Test
resizeSnap =
  -- Three windows are already perfectly snapped into place.
  -- In each test, we'll dry to de-snap them, and it shouldn't work.
  let b1 = Bounds 100 200 100 200
      b2 = Bounds 203 303 100 200
      b3 = Bounds 306 406 100 200
      wm0 = wmBlankState
            { wmWindows = [ mappedWinAt 1 b1
                          , mappedWinAt 2 b2
                          , mappedWinAt 3 b3
                          ]
            }
  in "resize de-snap resist" ~:
     [ "left" ~: dragMoveResizeTest wm0 2 (208, 150) (213, 150) b2
     , "right" ~: dragMoveResizeTest wm0 2 (298, 150) (293, 150) b2
     ]

windowResized :: Test
windowResized =
  let wm0 = wmTwoWindows
      bs = Bounds 20 220 30 330
      (wm1, cs1) = handleEvent (EvWasResized wid0 bs) wm0
      Just win = findWindow wm1 wid0
      bounds1 = winBounds win
  in "window is resized" ~:
        [ "window has new bounds" ~: bounds1 ~?= bs
        , "no requests" ~: cs1 ~?= []
        ]

wm3Windows :: WmState
wm3Windows =
  wmBlankState
  { wmWindows = [win0, win1, win2]
  , wmFocused = Just wid0
  , wmDragResize = Nothing
  }

handleEvents :: WmState -> [Ev] -> WmState
handleEvents = foldl (\wm0 e -> fst $ handleEvent e wm0)

mruFocusSwitching :: Test
mruFocusSwitching =
  let wm0 = handleEvents wm3Windows [EvFocusIn 2, EvFocusIn 1, EvFocusIn 0] -- set up known focus order
      (wm1, cs1) = handleEvent EvCmdFocusNext wm0
      (wm2, _) = handleEvent (EvFocusIn 1) wm1
      (wm3, cs3) = handleEvent EvCmdFocusFinished wm2
      -- then switch back
      (wm4, cs4) = handleEvent EvCmdFocusNext wm3
      (wm5, _) = handleEvent (EvFocusIn 0) wm4
      (wm6, cs6) = handleEvent EvCmdFocusFinished wm5
  in test
     [ "switch" ~:
       [ "window 0 is focused" ~: wmFocused wm0 ~?= Just 0
       , "window 0 is still focused" ~: wmFocused wm1 ~?= Just 0
       , "requests focus for 1" ~: cs1 ~?= [ReqFocus 1]
       , "window 1 now focused" ~: wmFocused wm2 ~?= Just 1
       , "window 1 still focused" ~: wmFocused wm3 ~?= Just 1
       , "no requests after first change" ~: cs3 ~?= []
       ]
     , "switch back" ~:
       [ "window 1 is focused" ~: wmFocused wm3 ~?= Just 1
       , "window 1 is still focused" ~: wmFocused wm4 ~?= Just 1
       , "requests focus for 1" ~: cs4 ~?= [ReqFocus 0]
       , "window 0 now focused" ~: wmFocused wm5 ~?= Just 0
       , "window 0 still focused" ~: wmFocused wm6 ~?= Just 0
       , "no requests after second change" ~: cs6 ~?= []
       ]
     ]

allTests :: Test
allTests =
  TestList [ mapsWindow
           , windowDestroyed
           , focusFollowsMouse1
           , focusFollowsMouse2
           , dragMove
           , windowResized
           , snap2Wins
           , snap3Wins
           , resizeAWindow
           , resizeSnap
           , mruFocusSwitching
           ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
