import Data.Char(toUpper)
main :: IO ()
main = do
       let name = "dennis"
       writeFile "writefile.txt" (fmap toUpper name)