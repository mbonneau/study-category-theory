# Type Inferred
Watch from [14'30](https://youtu.be/VGCE_3fjzU4?t=14m30s)

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let add a b = a + b
add :: Num a => a -> a -> a
-- type inference at work!
ghci> add 1 2
3
it :: Num a => a
ghci> add 1.1 2.2
3.3000000000000003
it :: Fractional a => a
ghci> add "a" "b"

<interactive>:6:1:
    No instance for (Num [Char]) arising from a use of ‘add’
    In the expression: add "a" "b"
    In an equation for ‘it’: it = add "a" "b"
ghci>
```
