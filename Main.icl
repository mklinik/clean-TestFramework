module Main

import StdEnv
import TestFramework

tests =
  [ Testcase "" (True shouldBe True)
  ]

Start world = runTests tests world
