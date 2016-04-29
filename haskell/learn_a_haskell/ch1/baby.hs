module Baby where 
import Data.Monoid

doubleMe :: Num a => a -> a
doubleMe x = x + x

msg :: (Show a, Num a) => a -> String
msg x = "doubleMe 5: " `Data.Monoid.mappend` show x

main :: IO ()
main = do
       putStrLn . msg $ doubleMe 5