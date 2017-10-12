implementation module TestFramework.GastTests

import TestFramework.GastTests

import System.IO
import Control.Monad
import Control.Applicative

GastTestcase :: String [Testoption] test -> Testcase | Testable test
GastTestcase desc opts test =
  { description = desc
  , runTestcase = \world -> evalIO (doTest desc opts test) world
  }

doTest desc opts test =
  putStrLn desc >>|
  mapM_ putStr (Test opts test) >>|
  pure Passed
  
