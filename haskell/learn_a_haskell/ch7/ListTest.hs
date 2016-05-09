-- see: http://learnyouahaskell.com/modules

module ListTest where
import Test.HUnit
import Data.List

-- list tests
-- intersperse: takes an element and a list and then puts that element in between each pair of elements in the list.
intersperseChar = TestCase $ assertEqual 
    "" (intersperse '.' "MONKEY") "M.O.N.K.E.Y" 

intersperseDigit = TestCase $ assertEqual
    "" (intersperse 0 [1,2,3,4]) [1,0,2,0,3,0,4]

intercalateStrings = TestCase $ assertEqual
    "" (intercalate " " ["hey", "there", "guys"]) "hey there guys"

intercalateDigits = TestCase $ assertEqual
    "" (intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]) [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

listCases = TestList [
    intersperseChar
    , intersperseDigit
    , intercalateStrings
    , intercalateDigits
    ]

main :: IO Counts
main = runTestTT $ TestList [listCases]