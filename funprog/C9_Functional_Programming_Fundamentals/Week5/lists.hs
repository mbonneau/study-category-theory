
squared :: (Enum a, Num a) => a -> [a]
squared n = [x^2 | x <- [1..n]]  

two_generators :: [(Integer, Integer)]
two_generators = [(x, y) | y <- [4,5], x <- [1..3]]

correlated_subqueries :: [(Integer, Integer)]
correlated_subqueries = [(x, y) | x <- [1..3], y <- [x..3]]

concat' :: [[a]] -> [a]
concat' xxs = [x | xs <- xxs, x <- xs]

only_even :: Integral a => a -> [a]
only_even n = [x | x <- [1..n], even x]

echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one ++ msg_two 

main :: IO ()
main = do
       echo "squared [1..5]: " $ show $ squared 5
       echo "two_generators: " $ show two_generators
       echo "correlated_subqueries: " $ show correlated_subqueries
       echo "concat' [[1,2,3],[4,5],[6]]: " $ show $ concat' [[1,2,3],[4,5],[6]]
       echo "only_even 10: " $ show $ only_even 10