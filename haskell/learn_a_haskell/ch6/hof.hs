-- see: http://learnyouahaskell.com/higher-order-functions

module Chapter6 where 
import Test.HUnit

-- partially applied the max function with '4'
maxComparedTo4 :: (Ord a, Num a) => a -> a
maxComparedTo4 = (max 4)

-- apply the function twice
-- (+3) is a function a -> a
-- x is a literal eg. a number or a String
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- maxCases
max45 :: Test
max45 = TestCase $ assertEqual 
    "max 4 5 should be 5" (max 4 5) 5
maxComparedTo43 :: Test
maxComparedTo43 = TestCase $ assertEqual 
    "maxComparedTo4 3 shouldBe 4" (maxComparedTo4 3) 4

maxCases :: Test
maxCases = TestLabel "Max cases" $ TestList [max45, maxComparedTo43]

-- apply cases
applyTwiceAddition :: Test
applyTwiceAddition = TestCase $ assertEqual 
    "Should apply +3 two times to 10" (applyTwice (+3) 10) 16
applyTwiceConcat :: Test
applyTwiceConcat = TestCase $ assertEqual 
    "Should apply concat \"HAHA\" two times to \"HEY\"" (applyTwice (++ "HAHA") "HEY") "HEYHAHAHAHA"
applyTwiceCons :: Test
applyTwiceCons = TestCase $ assertEqual 
    "Should apply (3:) two times to [1]" (applyTwice (3:) [1]) [3,3,1]

applyCases :: Test
applyCases = TestLabel "Apply cases" $ TestList [applyTwiceAddition, applyTwiceConcat, applyTwiceCons]

-- testcases
testcases :: Test
testcases = TestList [maxCases, applyCases]

main :: IO Counts
main = runTestTT $ testcases