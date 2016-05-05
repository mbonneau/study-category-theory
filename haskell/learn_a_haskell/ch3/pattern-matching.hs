-- see: http://learnyouahaskell.com/syntax-in-functions#pattern-matching

module PatternMatching where
import Data.Monoid
    
-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two 

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
charName char = "Unexpected input " `Data.Monoid.mappend` show char

addTuples :: (Enum a, Num a) => [(a, a)] -> [a]
addTuples xs = [a + b | (a, b) <- xs] 

-- error :: [Char] -> a

-- returns the head of a list
head' :: (Enum a) => [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x : _) = x

-- recursive length
length' :: (Enum a, Integral b) => [a] -> b
length' [] = 0
length' (_: xs) = 1 + length' xs 

main :: IO ()
main = do
       echo "lucky 7: " $ lucky 7
       echo "lucky 5: " $ lucky 5
       echo "factorial 5: " $ show $ factorial 5
       echo "charName 'a'  " $ charName 'a'  
       echo "charName 'b'  " $ charName 'b'  
       echo "charName 'c'  " $ charName 'c'  
       echo "charName 'h'  " $ charName 'h'
       echo "addTuples [(1,2), (3,4)]: " $ show $ addTuples [(1,2), (3,4)]
       echo "head' [1,2,3]: " $ show $ head' [1,2,3]
       echo "head' \"hello\": " $ show $ head' "hello"
       echo "length' \"hello\": " $ show $ length' "hello"
       echo "length' [1,2,3,4]: " $ show $ length' [1,2,3,4]




