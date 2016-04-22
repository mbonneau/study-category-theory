# Lists
Lists can be used to solve a whole bunch of problems in functional programming. So use them in both Scala and Haskell!

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let lostNumbers = [4,8,15,16,23,42]
lostNumbers :: Num t => [t]

ghci> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]

ghci> head [1..5]
1

ghci> tail [1..5]
[2,3,4,5]

ghci> last [1..5]
5

ghci> init [1..5]
[1,2,3,4]

ghci> length [1..10]
10

ghci> print $ reverse [1..10]
[10,9,8,7,6,5,4,3,2,1]

ghci> take 3 [1..5]
[1,2,3]

ghci> drop 3 [1..5]
[4,5]

ghci> minimum [8,4,1,2,3]
1

ghci> maximum [8,4,1,2,3]
8

ghci> sum [1..5]
15

ghci> product [1..5]
120

ghci> elem 2 [1,2,3]
True

ghci> print [x | x <- [1..5]]
[1,2,3,4,5]

ghci> [x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

ghci> concat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

```scala
$ scala
Welcome to Scala 2.11.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_77).
Type in expressions for evaluation. Or try :help.
scala> val lostNumbers = List(4,8,15,16,23,42)
lostNumbers: List[Int] = List(4, 8, 15, 16, 23, 42)

scala> List(1,2,3) ++ List(4,5,6)
res0: List[Int] = List(1, 2, 3, 4, 5, 6)

scala> (1 to 5).head
res1: Int = 1

scala> List.range(1, 5).head
res2: Int = 1

scala> (1 to 5).tail
res3: scala.collection.immutable.Range = Range(2, 3, 4, 5)

scala> (1 to 5).last
res4: Int = 5

scala> (1 to 5).init
res5: scala.collection.immutable.Range = Range(1, 2, 3, 4)

scala> (1 to 10).length
res6: Int = 10

scala> (1 to 10).reverse
res8: scala.collection.immutable.Range = Range(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

scala> (1 to 5).take(3)
res9: scala.collection.immutable.Range = Range(1, 2, 3)

scala> (1 to 5).drop(3)
res10: scala.collection.immutable.Range = Range(4, 5)

scala> List(8,4,1,2,3).min
res11: Int = 1

scala> List(8,4,1,2,3).max
res12: Int = 8

scala> (1 to 5).sum
res13: Int = 15

scala> (1 to 5).product
res14: Int = 120

scala> (1 to 5).contains(2)
res18: Boolean = true

scala> for (x <- 1 to 5) yield x
res19: scala.collection.immutable.IndexedSeq[Int] = Vector(1, 2, 3, 4, 5)

scala> for (x <- 50 to 100 if x % 7 == 3) yield x
res20: scala.collection.immutable.IndexedSeq[Int] = Vector(52, 59, 66, 73, 80, 87, 94)

scala> List(List(1,2),List(3,4),List(5,6)).flatten
res27: List[Int] = List(1, 2, 3, 4, 5, 6)
```