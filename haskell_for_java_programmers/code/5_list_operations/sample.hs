create start size = [start..size]

isGT3 value = value > 3
  
main = do
  let list1 = create 1 5
  let list2 = create 6 10
  
  print(list1)
  print(list2)
  
  print(list1 ++ list2)
  
  print(0 : list1)
  
  print(head list1)
  
  print(tail list1)

  print(take 2 list1)
  
  print(drop 2 list1)
  
  print(init list1)

  print(last list1)
  
  print(elem 0 list1)
  print(elem 4 list1)

  print(filter odd list1)
  print(filter even list1)

  print(takeWhile odd list1)
  print(takeWhile even list1)
  
  print(dropWhile odd list1)
  print(dropWhile even list1)
  
  print(filter isGT3 list1)
  print(takeWhile isGT3 list1)
  print(dropWhile isGT3 list1)