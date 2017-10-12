definition module TestFramework.GastTests

import TestFramework
import Gast

// Takes a Gast property and tests it.
// There currently is no way to determine if a Gast property was successful or
// not, because all it returns is a string with the result. Therefore,
// GastTestcases always pass, and you must look at the output to determine the
// result.
GastTestcase :: String [Testoption] test -> Testcase | Testable test
