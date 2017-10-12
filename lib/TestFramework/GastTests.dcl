definition module TestFramework.GastTests

import TestFramework
import Gast

gastTestcase :: String [Testoption] test -> Testcase | Testable test
