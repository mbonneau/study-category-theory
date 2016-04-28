doubleMe :: Num a => a -> a
doubleMe x = x + x

msg :: (Show a, Num a) => a -> String
msg x = "doubleMe 5: " ++ show x

main :: IO ()
main = do
       putStrLn $ msg $ doubleMe 5