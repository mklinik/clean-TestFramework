definition module TestFramework

import StdOverloaded
import GenPrint

:: Testcase
  = Testcase String TestResult
  | IOTestcase String (*World -> *(TestResult, *World))
:: TestResult = Passed | Failed String

(shouldBe) :: a a -> TestResult | == a & toString a
(shouldBeL) :: [a] [a] -> TestResult | == a & toString a
(shouldBe_) :: a a -> TestResult | == a & gPrint{|*|} a
(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
assert :: Bool -> TestResult

// Evaluate tests and print results to stdout
runTests :: [Testcase] *World -> *World
