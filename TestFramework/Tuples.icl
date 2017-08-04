implementation module TestFramework.Tuples

import StdEnv

instance toString (a, b) | toString a & toString b where
  toString (a, b) = "(" +++ toString a +++ "," +++ toString b +++ ")"
