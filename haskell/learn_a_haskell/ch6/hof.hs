-- see: http://learnyouahaskell.com/higher-order-functions

module Chapter6 where 
import Data.Monoid

-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two 

-- partially applied the max function with '4'
maxComparedTo4 :: (Ord a, Num a) => a -> a
maxComparedTo4 = (max 4)

-- apply the function twice
-- (+3) is a function a -> a
-- x is a literal eg. a number or a String
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main :: IO ()
main = do
       echo "max 4 5: " $ show $ max 4 5
       echo "(max 4) 5: " $ show $ (max 4) 5
       echo "maxComparedTo4 3: " $ show $ maxComparedTo4 3
       echo "applyTwice (+3) 10: " $ show $ applyTwice (+3) 10
       echo "applyTwice (++ \"HAHA\") \"HEY\": " $ show $ applyTwice (++ "*HAHA*") "HEY "
       echo "applyTwice: (3:) [1]: " $ show $ applyTwice (3:) [1]