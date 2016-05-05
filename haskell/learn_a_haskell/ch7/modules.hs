-- see: http://learnyouahaskell.com/modules

module Chapter7 where
import Data.Monoid

-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two 

main :: IO ()
main = do
    echo "foo" " bar 12345"