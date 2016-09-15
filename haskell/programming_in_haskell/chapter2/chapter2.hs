module Chapter2 where

n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

last' :: (Num a, Ord a) => [a] -> a
last' ns = head $ reverse ns

init' :: (Num a, Ord a) => [a] -> [a]
init' ns = take (length ns - 1) ns

main :: IO()
main = do
    putStrLn $ show $ n
    putStrLn $ show $ last' [1, 2, 3]
    putStrLn $ show $ init' [1, 2, 3]