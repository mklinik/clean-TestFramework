implementation module TestFramework.TaskTests

import TestFramework.TaskTests
import TestFramework
import StdMisc

derive class iTask TestResult

runTaskTests :: [TaskTestcase] *World -> *World
runTaskTests tests world = startEngine (runAllTests tests >>| shutDown) world

runAllTests :: [TaskTestcase] -> Task ()
runAllTests tests =
      sequence "all test cases" (map runOneTest tests)
  @   foldr sumTuple (0,0)
  >>- \(numPassed, numFailed) -> traceValue
         (toString numPassed +++ " test(s) passed\n" +++
          toString numFailed +++ " test(s) failed\n")
  @! ()

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
