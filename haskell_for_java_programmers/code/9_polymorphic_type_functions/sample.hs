count :: [Int] -> Int
count x = (length x)

count2 x = (length x)

main = do
  print(count [1, 2, 3])
  print(count2 [1, 2, 3])
  -- print(count ["a", "b"]) -- error on type mismatch
  print(count2 ["a", "b"])
  
{-
we can examine the types in ghci

> ghci
Prelude> :set +t
Prelude> :{
Prelude| let count :: [Int] -> Int
Prelude|     count x = (length x)
Prelude| :}
count :: [Int] -> Int
Prelude> 
Prelude> let count2 x = (length x)
count2 :: [a] -> Int  

Notice the type in the second case is 'a' which is 
lowercase, indicates a parametric type as. Int, on the
other hand is a non-parametric type (uppercase).
-}