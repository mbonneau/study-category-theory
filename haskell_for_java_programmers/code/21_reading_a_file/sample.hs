readStuff fileName = do
  content <- readFile fileName
  return ("The content is " ++ content)
  
main = do
  content <- readStuff "/etc/hosts"
  putStrLn(content)
