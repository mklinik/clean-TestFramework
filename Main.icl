module Main

import StdEnv
import TestFramework
import TestFramework.GastTests

import System.IO
import Control.Applicative
import Control.Monad
import Data.Func

tests =
  [ Testcase "simple truth" (True shouldBe True)
  , Testcase "simple property" (10 shouldSatisfy (\n -> n > 9))
  , Testcase "list equality" ([1,1+1,1+1+1] shouldBeL [1,2,3])
  , IOTestcase "write and read file" $
          writeFileM "test.txt" "test string"
      >>| readFileM "test.txt"
      >>= \content -> pure (content shouldBe "test string")

  , GastTestcase "a simple property" [Tests 2000] propPlusCommutative
  ]

propPlusCommutative :: Int Int -> Bool
propPlusCommutative x y = x + y == y + x

Start world = runTests tests world
