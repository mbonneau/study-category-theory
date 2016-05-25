module ShapeTest where
import Test.HUnit

data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n) 

vmult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vmult` (Vector l m n) = Vector (i*l) (j*m) (k*n) 

smult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `smult` (Vector l m n) = i*l + j*m + k*n

vplusTest :: Test
vplusTest = TestCase $ assertEqual
    "" (Vector 1 2 3 `vplus` Vector 4 5 6) (Vector 5 7 9)

vmultTest :: Test
vmultTest = TestCase $ assertEqual
    "" (Vector 1 2 3 `vmult` Vector 4 5 6) (Vector 4 10 18)

smultTest :: Test
smultTest = TestCase $ assertEqual
    "" (Vector 1 2 3 `smult` Vector 4 5 6) 32

-- testcases
testcases :: Test
testcases = TestList [
    vplusTest,
    vmultTest,
    smultTest
    ]

main :: IO Counts
main = runTestTT $ testcases