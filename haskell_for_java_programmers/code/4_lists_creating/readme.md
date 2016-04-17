# Creating Lists
Watch from [20'12](https://youtu.be/VGCE_3fjzU4?t=20m12s).

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let list1 = [1, 2, 3, 4, 5, 6]
list1 :: Num t => [t]
ghci> print(list1)
[1,2,3,4,5,6]
it :: ()

ghci> let list2 = [1..10]
list2 :: (Enum t, Num t) => [t]
ghci> print(list2)
[1,2,3,4,5,6,7,8,9,10]
it :: ()

ghci> let list3 = [2, 4..10]
list3 :: (Enum t, Num t) => [t]
ghci> print(list3)
[2,4,6,8,10]
it :: ()

ghci> let list4 = [1, 4..15]
list4 :: (Enum t, Num t) => [t]
ghci> print(list4)
[1,4,7,10,13]
it :: ()
```