canIVote age =
  if age > 17
  then "please vote"
  else    
    "go home kid, please come back in " ++ show(18 - age) ++ " years"
  
main = do
  print(canIVote(20))
  print(canIVote(13))