-- see: http://learnyouahaskell.com/modules

module Chapter7 where
import Test.HUnit
import Data.List

-- list tests
-- intersperse: takes an element and a list and then puts that element in between each pair of elements in the list.
intersperseChar = TestCase $ assertEqual 
    "Should be \"M.O.N.K.E.Y\"" (intersperse '.' "MONKEY") "M.O.N.K.E.Y" 

intersperseDigit = TestCase $ assertEqual
    "Should be [1,2,3,4,5,6]" (intersperse 0 [1,2,3,4]) [1,0,2,0,3,0,4]
    


listCases = TestList [intersperseChar, intersperseDigit]

main :: IO Counts
main = runTestTT $ TestList [listCases]