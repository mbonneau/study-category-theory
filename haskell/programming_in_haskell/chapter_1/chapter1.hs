module Chapter1 where 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns

main :: IO ()
main = do
    putStrLn $ show $ sum' [1..10]