{-
A function maps a single input value to a single output value
>ghci
Prelude> :set +t
Prelude> let greet name = "greetings..." ++ name
greet :: [Char] -> [Char]
Prelude> greet "Jake"
"greetings...Jake"
it :: [Char]
Prelude>
-}

greet name = "greetings..." ++ name
main = putStrLn(greet "Jake")