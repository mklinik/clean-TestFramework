implementation module TestFramework

import StdEnv
import StdMisc
import Text.GenPrint

import System.IO
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Func

Testcase :: String TestResult -> Testcase
Testcase desc result =
  { description = desc
  , runTestcase = \world -> (result, world)
  }

IOTestcase :: String (IO TestResult) -> Testcase
IOTestcase desc ioRes =
  { description = desc
  , runTestcase = \world -> evalIO ioRes world
  }

(shouldBe) :: a a -> TestResult
  | == a
  & toString a
(shouldBe) x y = shouldBeImpl x y toString

(shouldBe_) :: a a -> TestResult | == a & gPrint{|*|} a
(shouldBe_) x y = shouldBeImpl x y printToString

(shouldBeL) :: [a] [a] -> TestResult | == a & toString a
(shouldBeL) x y = (TestableList x) shouldBe (TestableList y)

shouldBeImpl :: a a (a -> String) -> TestResult | == a
shouldBeImpl x y print
  | x == y    = Passed
  | otherwise = Failed (  "expected: '" +++ print y +++
                       "'\n but got: '" +++ print x +++ "'")

(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
(shouldSatisfy) x p
  | p x       = Passed
  | otherwise = Failed (" " +++ toString x +++ " doesn't satisfy the condition.")

assert :: Bool -> TestResult
assert True  = Passed
assert False = Failed "assertion failed"

assertValue :: Bool a -> TestResult | toString a
assertValue True _ = Passed
assertValue False v = Failed ("assertion failed on value " +++ toString v)

assertValueL :: Bool [a] -> TestResult | toString a
assertValueL True _ = Passed
assertValueL False v = Failed ("assertion failed on value " +++ listToString toString v)

runTests :: [Testcase] *World -> *World
runTests tests world = execIO doRun world
  where
    doRun =
      runTests` tests >>= \result ->
      case result of
        Nothing -> putStrLn $ toString (length tests) +++ " tests passed"
        Just (err, test) ->
          putStrLn (test.description) >>|
          putStrLn err

// Nothing if all tests pass, Just for the first failed test
runTests` :: [Testcase] -> IO (Maybe (String, Testcase))
runTests` [] = pure (Nothing)
runTests` [test:tests] =
  withWorld (test.runTestcase) >>= \result ->
  case result of
    Failed err -> pure (Just (err, test))
    Passed     -> runTests` tests

listToString :: (a -> String) [a] -> String
listToString f xs = listToString` f xs (\x = "[" +++ x)
  where
    listToString` :: (a -> String) [a] (String -> String) -> String
    listToString` _ []     c = c "]"
    listToString` f [s:[]] c = c (listToString` f [] (\x = f s +++ x))
    listToString` f [s:ss] c = c (listToString` f ss (\x = f s +++ "," +++ x))

instance toString (TestableList a) | toString a
  where toString (TestableList l) = listToString toString l

instance == (TestableList a) | == a
  where (==) (TestableList l) (TestableList r) = l == r

only :: String [Testcase] -> [Testcase]
only prefix tests = filter hasPrefix tests
  where
    hasPrefix test = startsWith prefix (test.description)

// Stolen from Data.Text to not depend on clean platform
startsWith :: !String !String -> Bool
startsWith needle haystack
  #! s_needle = size needle
  = s_needle <= size haystack && needle == haystack%(0,s_needle-1)
