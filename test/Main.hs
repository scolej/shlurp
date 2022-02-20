import Test.HUnit
import Shlurp
import Data.Time.Clock.System

win0 :: Win
win0 = Win { winId = 0
           , winLastFocus = MkSystemTime 0 0
           , winName = "win 0"
           , winBounds = Bounds 0 10 0 10
           , winMapped = False
           }

mapsWindow :: Test
mapsWindow =
  let wm0 = wmBlankState
      (wm1, cs1) = handleEvent (EvWantsMap win0) wm0
      (wm2, cs2) = handleEvent (EvWasMapped win0) wm1
      win1 = win0 { winMapped = True }
  in "maps window" ~:
        [ "nothing is focused" ~: wmFocused wm1 ~?= Nothing
        , "no windows mapped" ~: wmMappedWindows wm1 ~?= []
        , "requests to map window" ~: cs1 ~?= [ReqMap win0]
        , "window is mapped" ~: wmMappedWindows wm2 ~?= [win1]
        , "no further requests" ~: cs2 ~?= []
        ]

allTests :: Test
allTests =
  TestList [ mapsWindow ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
