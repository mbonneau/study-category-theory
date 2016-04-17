mult a b = a * b

main = do
  let list = [1..5]
  print(list)
  
  print(map (* 2) list)
  print(map (`mult` 2) list)
  print(map (3 `mult`) list)