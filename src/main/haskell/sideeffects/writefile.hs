import Data.Char(toUpper)
main :: IO ()
main = do
       let name = "dennis"
       writeFile "writefile.txt" (map toUpper name)