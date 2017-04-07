module Main

import StdEnv
import TestFramework
import System.IO
import Control.Monad
import Control.Applicative

tests =
  [ Testcase "simple truth" (True shouldBe True)
  , Testcase "simple property" (10 shouldSatisfy (\n -> n > 9))
  , IOTestcase "write and read file" (evalIO
      (   writeFileM "test.txt" "test string"
      >>| readFileM "test.txt"
      >>= \content -> pure (content shouldBe "test string")
      )
    )
  ]

Start world = runTests tests world
