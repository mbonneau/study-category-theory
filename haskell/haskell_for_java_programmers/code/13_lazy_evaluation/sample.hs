greetKid = putStrLn("hello kiddo")
greetAdult = putStrLn("howdy")

greet name age greet1 greet2 = do
  putStr(name ++ ", ")
  if age < 13
  then greet1
  else greet2

main = do
  greetKid
  greetAdult
  print("let's try something else now...")
  greet "Jake" 7 greetKid greetAdult
  greet "Sara" 17 greetKid greetAdult

--The functions were evaluated on 11, 12, but they were
--delayed evaluation (and one of the skipped in each line)
--in 14, 15.