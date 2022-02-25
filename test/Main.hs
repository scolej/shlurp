import Test.HUnit
import Shlurp
import Data.Time.Clock.System

wid0 :: WinId
wid0 = 0

win0 :: Win
win0 = Win { winId = wid0
           , winLastFocus = MkSystemTime 0 0
           , winName = "win 0"
           , winBounds = Bounds 0 10 0 10
           , winMapped = False
           }

wid1 :: WinId
wid1 = 1

win1 :: Win
win1 = Win { winId = wid1
           , winLastFocus = MkSystemTime 0 0
           , winName = "win 0"
           , winBounds = Bounds 0 10 0 10
           , winMapped = False
           }

wmTwoWindows :: WmState
wmTwoWindows =
  WmState { wmWindows = [win0, win1]
          , wmFocused = Just wid0
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
        -- focus time
        ]

focusFollowsMouse2 :: Test
focusFollowsMouse2 =
  let wm0 = wmTwoWindows
      (wm1, cs1) = handleEvent (EvMouseEntered wid0) wm0
  in "no focus change" ~:
        [ "window 0 focused" ~: wmFocused wm0 ~?= Just wid0
        , "no requests" ~: cs1 ~?= []
        , "window 0 is still focused" ~: wmFocused wm1 ~?= Just wid0
        -- focus time
        ]

allTests :: Test
allTests =
  TestList [ mapsWindow
           , focusFollowsMouse1
           , focusFollowsMouse2
           ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
