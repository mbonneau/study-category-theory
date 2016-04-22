doubleIt x = x * 2

main = do
  let list = [1..5]
  print(list)
  
  print(map doubleIt list)
  
  print(map doubleValue list)
    where doubleValue x = x * 2
