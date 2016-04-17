# List operations
Watch from [26'15](https://youtu.be/VGCE_3fjzU4?t=26m15s).

Everything in Haskell is immutable. If values are immutable, they can be shared without
any consequences in sharing the state.  

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let create start size = [start..size]
create :: Enum t => t -> t -> [t]

ghci> let isGT3 value = value > 3
isGT3 :: (Num a, Ord a) => a -> Bool

ghci> let list1 = create 1 5
list1 :: (Enum t, Num t) => [t]

ghci> let list2 = create 6 10
list2 :: (Enum t, Num t) => [t]

ghci> print(list1)
[1,2,3,4,5]
it :: ()

ghci> print(list2)
[6,7,8,9,10]
it :: ()

ghci> print(list1 ++ list2)
[1,2,3,4,5,6,7,8,9,10]
it :: ()

ghci> print(0 : list1)
[0,1,2,3,4,5]
it :: ()

ghci> print(head list1)
1
it :: ()

ghci> print(tail list1)
[2,3,4,5]
it :: ()

ghci> print(take 2 list1)
[1,2]
it :: ()

ghci> print(drop 2 list1)
[3,4,5]
it :: ()

ghci> print(init list1)
[1,2,3,4]
it :: ()

ghci> print(last list1)
5
it :: ()

ghci> print(elem 0 list1)
False
it :: ()

ghci> print(elem 4 list1)
True
it :: ()

ghci> print(filter odd list1)
[1,3,5]
it :: ()

ghci> print(filter even list1)
[2,4]
it :: ()

ghci> print(takeWhile odd list1)
[1]
it :: ()

ghci> print(takeWhile even list1)
[]
it :: ()

ghci> print(dropWhile odd list1)
[2,3,4,5]
it :: ()

ghci> print(dropWhile even list1)
[1,2,3,4,5]
it :: ()

ghci> print(filter isGT3 list1)
[4,5]
it :: ()

ghci> print(takeWhile isGT3 list1)
[]
it :: ()

ghci> print(dropWhile isGT3 list1)
[1,2,3,4,5]
it :: ()
```