abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

-- conditional expressions can be nested

signum' :: Int -> Int
signum' n = if n < 0 then -1 else 
	          if n == 0 then 0 else 1

-- guarded equations

abs'' :: Int -> Int
abs'' n | n >= 0    = n
        | otherwise = -n

signum'' :: Int -> Int
signum'' n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1

lesstzero :: Int -> Bool
lesstzero n = n < 0
eqtzero :: Int -> Bool
eqtzero n = n == 0

signum''' :: Int -> Int
signum''' n | lesstzero n = -1
            | eqtzero n   = 0
            | otherwise   = 1

-- pattern matching
not' :: Bool -> Bool
not' False = True
not' True = False

-- pattern matching on lists

head' :: [a] -> a
head' (x:_) = x

-- n + k patterns n is a number and k is a constant
-- but n+k patterns are removed from Haskell 2010
-- see: http://stackoverflow.com/questions/3748592/what-are-nk-patterns-and-why-are-they-banned-from-haskell-2010
--pred' :: Num n => n -> n
--pred' (n + 1) = n

-- lambda
-- normal function declaration
sum' :: Num a => a -> a
sum' x = x + x

-- lambda expressions are just values
sum'' :: Num a => a -> a
sum'' = \x -> x + x

-- lambda currying
-- with add' the Haskell compiler will do the currying
-- with add' all the arguments are on the left
add' :: Num a => a -> a -> a 
add' x y = x + y

-- we manually do currying
-- with lambda all arguments are to the right
add'' :: Num a => a -> a -> a
add'' = \x -> (\y -> x + y)

-- lambda in a map function
odds' :: (Enum a, Num a) => a -> [a]
odds' n = map (\x -> x*2 + 1) [0..n-1]

