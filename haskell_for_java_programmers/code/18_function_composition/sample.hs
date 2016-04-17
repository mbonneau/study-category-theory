totalDoubleOfEven1 xs =
  (foldr (+) 0 (map (* 2) (filter even xs)))
  
totalDoubleOfEven2 xs =
  foldr (+) 0 . map (* 2) . filter even $ xs
  
main = do
  let list = [1..10]
  print(totalDoubleOfEven1 list)
  print(totalDoubleOfEven2 list)