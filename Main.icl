module Main

import StdEnv
import TestFramework
import TestFramework.GastTests

import System.IO
import Control.Applicative
import Control.Monad
import Data.Func

tests =
  [ testcase "simple truth" (True shouldBe True)
  , testcase "simple property" (10 shouldSatisfy (\n -> n > 9))
  , testcase "list equality" ([1,1+1,1+1+1] shouldBeL [1,2,3])
  , ioTestcase "write and read file" $
          writeFileM "test.txt" "test string"
      >>| readFileM "test.txt"
      >>= \content -> pure (content shouldBe "test string")

  , gastTestcase "a simple property" [Tests 1000] propPlusCommutative
  ]

propPlusCommutative :: Int Int -> Bool
propPlusCommutative x y = x + y == y + x

Start world = runTests tests world
