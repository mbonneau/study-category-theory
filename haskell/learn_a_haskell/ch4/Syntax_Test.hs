module Syntax_Test where
-- see: https://leiffrenzel.de/papers/getting-started-with-hunit.html
import Test.HUnit
import Syntax

-- The structure of a test case is always this: 
-- 1. create some input, 
-- 2. run the tested code on that input,
-- 3. make some assertions over the results.
-- 4. group them by test lists, and label them

-- TestCase :: Assertion -> Test
-- assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
-- TestList :: [Test] -> Test

testSeven :: Test
testSeven = TestCase $ assertEqual 
    "Should get seven" "LUCKY NUMBER SEVEN!" $ lucky 7 

simpleCases :: Test
simpleCases = TestLabel "Simple cases: " $ TestList [testSeven]

testEdge :: (Integral a) => a -> Test
testEdge a = TestCase $ assertEqual
    "Should say sorry" "Sorry, you're out of luck, pal!" $ lucky a

borderCases :: Test
borderCases = TestLabel "Border cases: " $ TestList [testEdge 6, testEdge 8] 

-- runTestTT :: Test -> IO Counts
-- TestList :: [Test] -> Test
main :: IO Counts
main = runTestTT $ TestList [simpleCases, borderCases]