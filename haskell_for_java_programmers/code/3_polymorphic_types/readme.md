# Polymorphic types
Watch from [16'35](https://youtu.be/VGCE_3fjzU4?t=16m35s)

In Haskell, types have uppercase letters.

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let echo a = a
echo :: t -> t
ghci> echo 1
1
it :: Num t => t
ghci> echo 1.1
1.1
it :: Fractional t => t
ghci> echo "haha"
"haha"
it :: [Char]
ghci>
```