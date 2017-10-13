implementation module TestFramework.GastTests

import TestFramework.GastTests

import Control.Monad
import Control.Applicative

GastTestcase :: String [Testoption] test -> Testcase | Testable test
GastTestcase desc opts test =
  { description = desc
  , runTestcase = \world -> runGastTest desc (Test opts test) world
  }

runGastTest desc output world
  # (out, world) = stdio world
  # out = fwrites (desc +++ "\n") out
  # (_, world) = fclose out world
  = printAll output world

// Strict World is needed to avoid a space leak
printAll :: [String] *World -> *(TestResult, !*World)
printAll [] world = (Passed, world)
printAll [s:ss] world
  # (out, world) = stdio world
  # out = fwrites s out
  # (_, world) = fclose out world
  = printAll ss world
