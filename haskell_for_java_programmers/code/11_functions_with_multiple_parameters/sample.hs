add a b = a + b

main = print(add 1 2)

{-
>ghci
>:set +t
Prelude> let add a b = a + b
add :: Num a => a -> a -> a

add is a function that takes one input and returns a function that, in turn, takes another input to get the result
Hard to see the benefit from the traditional language point of view, but this readily takes us to partially applied functions
-}