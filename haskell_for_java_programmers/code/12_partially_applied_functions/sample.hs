minus a b = a - b

main = do
  let twoMinus = minus 2
  print(twoMinus 1)
  print(twoMinus 5)

  let threeMinus = minus 3
  print(threeMinus 1)
  print(threeMinus 5)

  let sub2From = (`minus` 2)
  print(sub2From 8)
  print(sub2From 10)
  
{-
>ghci
>:set +t
Prelude> let minus a b = a - b
minus :: Num a => a -> a -> a
Prelude> let twoMinus = minus 2
twoMinus :: Integer -> Integer
-}