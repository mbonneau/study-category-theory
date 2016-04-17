# 1.0 The REPL
Watch from [5'18](https://youtu.be/VGCE_3fjzU4?t=5m18s).

What is the benefit of having the types at compile time? 

1. You can do type verification at compile time,
2. Eliminate errors at compile time.

Bad static typing is: **Typing the type information all over the place**.

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> "hello Haskell"
"hello Haskell"
ghci> :set +t
ghci> "hello Haskell"
"hello Haskell"
it :: [Char]
ghci> 2 + 3
5
it :: Num a => a
ghci> it
5
it :: Num a => a
ghci> :type it
it :: Num a => a
ghci> it + 4
9
it :: Num a => a
ghci> it
9
it :: Num a => a
ghci> it + 4 :: Int
13
it :: Int
ghci>
```
