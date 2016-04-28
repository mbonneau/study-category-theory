double :: Num a => a -> a
double x = x + x

-- quadruple x = double (double x)
-- quadruple x = double $ double x

quadruple :: Num a => a -> a
quadruple = double . double

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

average :: Foldable t => t Int -> Int
average xs = sum xs `div` length xs

main :: IO ()
main = do
       putStrLn $ "Double 2: " ++ (show $ double 2)
       putStrLn $ "Quadruple 4: " ++ (show $ quadruple 4)
       putStrLn $ "Factorial: 10" ++ (show $ factorial 10)
       putStrLn $ "Average [1..20]: " ++ (show $ average [1..20])
       