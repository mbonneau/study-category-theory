--factorial 0 = 1
--factorial n = n * factorial(n - 1)

factorial n
  | n == 0 = 1
  | n < 0 = 1
  | otherwise = n * factorial(n - 1)

main = do
  print(factorial 0)
  print(factorial 5)
  print(factorial (-2))