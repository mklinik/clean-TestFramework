implementation module TestFramework.GastTests

import TestFramework.GastTests

import System.IO
import Control.Monad
import Control.Applicative

GastTestcase :: String [Testoption] test -> Testcase | Testable test
GastTestcase desc opts test =
  { description = desc
  , runTestcase = \world -> doTest (Test opts test) world
  }

// Strict World is needed to avoid a space leak
doTest :: [String] *World -> *(TestResult, !*World)
doTest [] world = (Passed, world)
doTest [s:ss] world
  # (out, world) = stdio world
  # out = fwrites s out
  # (_, world) = fclose out world
  = doTest ss world
