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