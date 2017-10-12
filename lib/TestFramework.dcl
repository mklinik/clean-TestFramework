definition module TestFramework

import StdOverloaded
import GenPrint
from System.IO import :: IO

Testcase :: String TestResult -> Testcase
IOTestcase :: String (IO TestResult) -> Testcase

(shouldBe) :: a a -> TestResult | == a & toString a
(shouldBeL) :: [a] [a] -> TestResult | == a & toString a
(shouldBe_) :: a a -> TestResult | == a & gPrint{|*|} a
(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
assert :: Bool -> TestResult
// If the assertion is false, include a in the error message
assertValue :: Bool a -> TestResult | toString a
assertValueL :: Bool [a] -> TestResult | toString a

// Evaluate tests and print results to stdout
runTests :: [Testcase] *World -> *World

// Keep only testcases whose description starts with the given string
only :: String [Testcase] -> [Testcase]


// for internal use only
:: TestableList a = TestableList [a]
instance toString (TestableList a) | toString a
instance == (TestableList a) | == a
listToString :: (a -> String) [a] -> String
shouldBeImpl :: a a (a -> String) -> TestResult | == a

:: Testcase =
  { description :: String
  , runTestcase :: *World -> *(TestResult, *World)
  }
:: TestResult = Passed | Failed String
