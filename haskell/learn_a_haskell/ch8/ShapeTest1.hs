module ShapeTest1 where 
import Test.HUnit

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
    
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

circleSurfaceTest :: Test
circleSurfaceTest = TestCase $ assertEqual
    "Should be pi" (surface $ Circle 1 1 1) pi 

rectSurfaceTest :: Test
rectSurfaceTest = TestCase $ assertEqual
    "" (surface $ Rectangle 1 0 0 1) 1

-- testcases
testcases :: Test
testcases = TestList [circleSurfaceTest, rectSurfaceTest]

main :: IO Counts
main = runTestTT $ testcases