module ShapeTest2 where 
import Test.HUnit

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show
    
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

circleSurfaceTest :: Test
circleSurfaceTest = TestCase $ assertEqual
    "Should be pi" (surface $ Circle (Point 1 1) 1) pi 

rectSurfaceTest :: Test
rectSurfaceTest = TestCase $ assertEqual
    "" (surface $ Rectangle (Point 1 0) (Point 0 1)) 1

-- testcases
testcases :: Test
testcases = TestList [
    circleSurfaceTest
    , rectSurfaceTest
    ]

main :: IO Counts
main = runTestTT $ testcases