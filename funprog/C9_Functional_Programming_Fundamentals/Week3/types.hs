add :: Num a => (a, a) -> a
add (x, y) = x + y

-- curried
add' :: Num a => a -> a -> a
add' x y = x + y

zeroto :: (Enum a, Num a) => a -> [a]
zeroto n = [0..n]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

-- Homework 1 - What are the types of the following?

a = ['a','b','c'] :: [Char]
b = ('a','b','c') :: (Char,Char,Char)
c = [(False, '0'), (True, '1')] :: [(Bool, Char)]
d = ([False,True], ['0', '1']) :: ([Bool],[Char])
e = [tail, init, reverse] :: [[a] -> [a]]

-- Homework 2 - What are the types of the following functions?

second :: [a] -> a
second xs     = head (tail xs) 

swap :: (a, b) -> (b, a)
swap (x, y)   = (y, x)

pair :: a -> b -> (a, b)
pair x y      = (x, y)

double :: Num a => a -> a
double x      = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x     = f (f x)

x = add (1,2)

main :: IO ()
main = do
       putStrLn $ "add (1,2): " ++ (show $ add (1,2))
       putStrLn $ "add' 1 2: " ++ (show $ add' 1 2)
       putStrLn $ "zeroto 5: " ++ (show $ zeroto 5)
       putStrLn $ "length [1..5]: " ++ (show $ length' [1..5])
       putStrLn $ "second [10..15]: " ++ (show $ second [10..15])
       putStrLn $ "swap (1,2): " ++ (show $ swap (1,2))
       putStrLn $ "pair 1 2: " ++ (show $ pair 1 2)
       putStrLn $ "double 5: " ++ (show $ double 5)