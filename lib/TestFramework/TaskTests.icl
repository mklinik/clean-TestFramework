implementation module TestFramework.TaskTests

import TestFramework.TaskTests
import TestFramework
import StdMisc

derive class iTask TestResult
derive gText TestableList

assertValueT :: Bool a -> TestResult | gText{|*|} a
assertValueT True _ = Passed
assertValueT False v = Failed ("assertion failed on value " +++ toSingleLineText v)

assertValueLT :: Bool [a] -> TestResult | gText{|*|} a
assertValueLT True _ = Passed
assertValueLT False v = Failed ("assertion failed on value " +++ listToString toSingleLineText v)

(shouldBeLT) :: [a] [a] -> TestResult | == a & gText{|*|} a
(shouldBeLT) x y = shouldBeImpl (TestableList x) (TestableList y) toSingleLineText

runTaskTests :: [TaskTestcase] *World -> *World
// A little bit of waiting is needed to synchronize client and server,
// otherwise shutDown leaves a spinning circle on the screen.
// There currently is a bug in waitForTimer forcing us to specify zero second
// wait time as minus one hour.
runTaskTests tests world = startEngine (waitForTimer -3600 >>| runAllTests tests >>| shutDown) world

runAllTests :: [TaskTestcase] -> Task String
runAllTests tests =
      sequence "all test cases" (map runOneTest tests)
  @   foldr sumTuple (0,0)
  >>- \(numPassed, numFailed) -> traceValue
         (toString numPassed +++ " test(s) passed\n" +++
          toString numFailed +++ " test(s) failed\n")

// Sums tuples element-wise
sumTuple :: (Int, Int) (Int, Int) -> (Int, Int)
sumTuple (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

// Returns (1,0) if the test succeeds, (0,1) if the test fails
runOneTest :: TaskTestcase -> Task (Int, Int)
runOneTest (TaskTestcase description test)
  = test
  >>- \result -> case result of
    Passed -> return (1,0)
    Failed reason -> traceValue msg @! (0,1)
      where
        msg = description +++ ": " +++ reason +++ "\n"
