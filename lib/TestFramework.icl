implementation module TestFramework

import StdEnv
import GenPrint

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
  | otherwise = Failed ("\n expected: '" +++ print y +++
                       "'\n  but got: '" +++ print x +++ "'")

(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
(shouldSatisfy) x p
  | p x       = Passed
  | otherwise = Failed ("\n " +++ toString x +++ " doesn't satisfy the condition.")

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
runTests tests world
  # (output, world) = runTests` tests "" world
  # (console, world) = stdio world
  # console = fwrites (if (output == "")
      (toString (length tests) +++ " test(s) passed\n")
      output) console
  # (ok, world) = fclose console world
  | not ok = abort "Cannot close console\n"
  | otherwise = world

runTests` :: [Testcase] String *World -> (String, *World)
runTests` [] output world = (output, world)
runTests` [(IOTestcase description test):rest] output world
  # (result, world) = test world
  = case result of
      Passed -> runTests` rest output world
      (Failed reason) ->
        runTests` rest (output +++ description +++ ": " +++ reason +++ "\n") world
runTests` [(Testcase description test):rest] output world
  # (result, world) = (test, world)
  = case result of
      Passed -> runTests` rest output world
      (Failed reason) ->
        runTests` rest (output +++ description +++ ": " +++ reason +++ "\n") world

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
    hasPrefix (Testcase   description _) = startsWith prefix description
    hasPrefix (IOTestcase description _) = startsWith prefix description

// Stolen from Data.Text to not depend on clean platform
startsWith :: !String !String -> Bool
startsWith needle haystack
  #! s_needle = size needle
  = s_needle <= size haystack && needle == haystack%(0,s_needle-1)
