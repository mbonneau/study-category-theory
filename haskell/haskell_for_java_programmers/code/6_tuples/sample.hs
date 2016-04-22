printValues a b c =
  print(show(a) ++ ":" ++ show(b) ++ ":" ++ show(c))
  
main = do
  let pair = ("Jack", "Jill")
  
  print(pair)
  print(fst pair)
  print(snd pair)
  
  let three = (1, "two", 3.0)
  print(three)
  
  case three of 
    (a, b, c) -> printValues a b c
