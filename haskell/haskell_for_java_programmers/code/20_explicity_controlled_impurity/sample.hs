greet :: IO() --this line is optional
greet = do
  putStrLn("What's your name")
  name <- getLine
  putStrLn("Hello, " ++ name)

greet2 = 
  putStrLn("What's your name") >>
  getLine >>= (\name -> putStrLn("Howdy, " ++ name))
  
main = do
  greet
  greet2