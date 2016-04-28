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