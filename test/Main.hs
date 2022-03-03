import Test.HUnit
import Shlurp

wid0 :: WinId
wid0 = 0

win0 :: Win
win0 =
  Win { winId = wid0
      , winName = "win 0"
      , winBounds = Bounds 0 10 0 10
      , winMapped = False
      }

wid1 :: WinId
wid1 = 1

win1 :: Win
win1 =
  Win { winId = wid1
      , winName = "win 0"
      , winBounds = Bounds 0 10 0 10
      , winMapped = False
      }

wmTwoWindows :: WmState
wmTwoWindows =
  WmState { wmWindows = [win0, win1]
          , wmFocused = Just wid0
          , wmDragResize = ResizeNone
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
        , "no requests" ~: cs1 ~?= []
        , "window 0 is still focused" ~: wmFocused wm1 ~?= Just wid0
        ]

dragMove :: Test
dragMove =
  let wm0 = wmTwoWindows
      (wm1, cs1) = handleEvent (EvDragStart wid0 5 5) wm0
      (wm2, cs2) = handleEvent (EvDragMove 120 130) wm1
      (wm3, cs3) = handleEvent EvDragFinish wm2
      (_, cs4) = handleEvent (EvDragMove 99 99) wm3
      newBounds = Bounds 115 125 125 135
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

-- TODO
--
-- snap cases
-- grows out to meet opposing edge
-- grows to overlap same edge
--
-- add conf for
-- snap distance
-- snap gap

snap :: Test
snap =
  let w1 = 1
      w2 = 2
      wm0 = wmBlankState
            { wmWindows = [ mappedWinAt w1 (Bounds 20 100 20 100)
                          , mappedWinAt w2 (Bounds 300 400 200 300)
                          ]
            }
      (wm1, _) = handleEvent (EvDragStart w1 30 30) wm0
      (_, cs2) = handleEvent (EvDragMove 220 30) wm1
      expectedBounds = Bounds 219 299 20 100
  in "window snap 1" ~:
        [ "window snaps to edge" ~: cs2 ~?= [ReqResize w1 expectedBounds]
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

allTests :: Test
allTests =
  TestList [ mapsWindow
           , focusFollowsMouse1
           , focusFollowsMouse2
           , dragMove
           , windowResized
           , snap
           ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
