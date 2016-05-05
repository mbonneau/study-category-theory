-- see: http://learnyouahaskell.com/syntax-in-functions#guards-guards

module Guards where
import Data.Monoid
import Numeric 

-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two 

-- see: http://www.whathealth.com/bmi/formula.html
calcBmi :: (RealFloat a) => a -> a -> a
calcBmi weight height = weight / (height/100)^2

-- see: http://stackoverflow.com/questions/1559590/haskell-force-floats-to-have-two-decimals
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

-- based on your bmi, the function tells you off (omg!)
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi 
    | bmi <= 18.5 = "You're underweight, you emo you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!" 

max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b     = a 
    | otherwise = b

main :: IO ()
main = do
       echo "calcBmi 72 182: " $ formatFloatN (calcBmi 72 182) 2
       echo "bmiTell calcBmi 72 182: " $ bmiTell $ calcBmi 72 182
       echo "max' 1 2: " $ show $ max' 1 2 
       echo "max' 4 3: " $ show $ max' 4 3 
