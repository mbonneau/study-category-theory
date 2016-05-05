# Learn you a Haskell - Typeclasses
See: [Learn you a Haskell - Typeclasses](http://learnyouahaskell.com/types-and-typeclasses)

## Believe the type!
Haskell has a static type system. The type of every expression is known at compile time, which leads to safer code. That's good because it's better to 
catch errors, such as dividing a boolean by a number, at compile time instead of having your program crash at runtime! The compiler's knowledge of Types lets 
you mitigate the consequences of your mistakes, often turning them into straightforward-to-fix-during-development compilation errors, 
thus provides Safety in developing your code.

Everything in Haskell has a type, so the compiler can (quickly - yes its quick!) reason quite a lot about your program before compiling it.
Understanding the type system is a very important part of learning Haskell and other type safe languages for that matter.  

## Type safety?
See: [Li Haoyi Haoyi blog about type safety](http://www.lihaoyi.com/post/StrategicScalaStylePracticalTypeSafety.html).

## What are type variables?
A type variable is a variable that can be of a certain type. It allows us to easily write very general functions if they don't use 
any specific behavior of the types in them. Functions that have type variables are called polymorphic functions.  

## What is a typeclass?
A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements 
the behavior the typeclass describes.

## What is a class constraint?
In the type definition of the following function: 

```haskell
> $t (==)
(==) :: (Eq a) => a -> a -> Bool  
```
...everything before the `=>` symbol is called a __class constraint__. We can read the previous type declaration like this: the equality function takes any 
two values that are of the same type and returns a Bool. The type of those two values must be a member of the __Eq__ class (this was the class constraint).

The __Eq__ typeclass provides an interface for testing for equality. Any type where it makes sense to test for equality between two values of that type 
should be a member of the __Eq__ class. All standard Haskell types except for IO (the type for dealing with input and output) and functions are a part 
of the __Eq__ typeclass.

## Some basic typeclasses
- __Eq__ is used for types that support equality testing. The functions its members implement are `==` and `/=`. 
  So if there's an Eq class constraint for a type variable in a function, it uses `==` or `/=` somewhere inside its definition. 
  All the types we mentioned previously except for functions are part of Eq, so they can be tested for equality.
- __Ord__ is for types that have an ordering. Ord covers all the standard comparing functions such as `>`, `<`, `>=` and `<=`. 
  The `compare` function takes two Ord members of the same type and returns an ordering. __Ordering__ is a type that can be `GT`, `LT` or `EQ`, 
  meaning greater than, lesser than and equal, respectively.
- __Show__ is for types that can be presented as strings. All types covered so far except for functions are a part of Show. The most used function 
  that deals with the Show typeclass is `show`. It takes a value whose type is a member of Show and presents it to us as a string.
- __Read__ is sort of the opposite typeclass of Show. The `read` function takes a string and returns a type which is a member of Read.
- __Enum__ members are sequentially ordered types — they can be enumerated. The main advantage of the Enum typeclass is that we can use its types 
  in list ranges. They also have defined successors and predecesors, which you can get with the `succ` and `pred` functions. Types in this class: 
  (), Bool, Char, Ordering, Int, Integer, Float and Double.
- __Bounded__ members have an upper and a lower bound. The functions `minBound` and `maxBound` are interesting because they have a type of 
  __(Bounded a) => a__. In a sense they are polymorphic constants. All tuples are also part of Bounded if the components are also in it.
- __Num__ is a numeric typeclass. Its members have the property of being able to act like numbers. To join Num, a type must already be friends with `Show` and `Eq`.
- __Integral__ is also a numeric typeclass. Num includes all numbers, including real numbers and integral numbers, __Integral__ includes only integral 
 (whole) numbers. In this typeclass are __Int__ and __Integer__.
- __Floating__ includes only floating point numbers, so __Float__ and __Double__.





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