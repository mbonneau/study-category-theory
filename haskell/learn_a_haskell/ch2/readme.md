# Learn you a Haskell - Typeclasses
## Resources used
See: [Learn you a Haskell - Typeclasses](http://learnyouahaskell.com/types-and-typeclasses)
See: [Strategic Scala Style: Practical Type Safety](http://www.lihaoyi.com/post/StrategicScalaStylePracticalTypeSafety.html)

__Disclaimer:__ The following is mostly taken from other resources on  the Internet, but this is the way I learn. I search resources for a subject and gather them all in one place, I read the content and create my own copy so I really understand the subject.  

## What is a type?
A __Type__ is an abstract concept. It is not 'something concrete' in a programming language like an interface or a class. A type is something you know about a __value__ in your program at compile time. It is _knowledge_ about a value, for example:

- Something which is an __Int__ definitely contains a 32 bit integer from _-2147483648_ to _2147483647_
- Something which is a __Option[T]__ definitely contains either a _Some[T]_, or _None_
- Something which is a __CharSequence__ definitely contains _Chars_ and lets me call _.length_, _.chatAt_, and _.subsequence_ methods, but I don't know whether it is a _String_, a _StringBuffer_, or something else. You don't know if it's mutable or immutable, how its stores its contents, or what the performance characteristics are.
- Something which is a __String__ also has characters, but you know it's immutable, stores its contents in an internal array of characters, and has _O(1)_ lookup to look up Chars by index

The _type_ of a value tells you both what something _can be_, and what it _cannot be_. A __Option[String]__ can be _Some_ or _None_, but it cannot be a _32 bit integer_! In statically typed languages like eg. Haskell and Scala, this is not something you need to check in your code: it is something you can rely on being correct, with the compiler checking during compilation.

This __knowledge about the value__ is exactly what comprises the __"Type"__ of a value. So __types__ let you _describe to the compiler things you know about the values in your program_.

## What is a type not?
### A Class
Because a type is an abstract concept that exactly defines what a value is, a type cannot be a class. Yes, in Java, and Scala on the JVM, all types are represented by classes (or interfaces). This does not hold true in e.g. Scala.js, where you can define types to be synthetic (traits extending js.Any) with no remnants left behind to inspect after everything is compiled, or in other programming languages.

While the types being backed by classes is a true fact, __it is an implementation detail__ that is mostly irrelevant to the discussion.

## What is a type system?
Basically every programming language has a different type system. Some have generics, some have reified generics (the types are (made) available (again) at runtime). Some, like Java, have reified types, where the "type" of a value always corresponds to a class or interface can be checked at runtime. Also in Java, generics are implemented using erasure, in which generic type parameters are simply __removed__ (after compilation) and are __not__ available at runtime. Other languages, like C, don't have reified types. Dynamic languages like Python do not have static types - types that are available at compile time - and so a type only exists at runtime.

A type system __lets you describe to the compiler things you know about the values in your program__, _and_ __let it check that what you're doing is consistent with what you said you wanted to do__.

Type systems are generally formulated as collections of rules for checking the “consistency” of programs. This kind of checking exposes not only trivial mental slips, but also deeper conceptual errors, which frequently manifest as type errors.

## What is safety
__Safety means that when you make a mistakes, the consequences are minor.__ 

People make all sorts of mistakes: typos in code, poor load-estimation, copy-pasting the wrong command. When you make a mistake, what happens?

- You see a red squiggly in your editor and fix it in 5 seconds,
- You wait for a full compile, taking 10s, then fix it,
- You run the test suite, which takes 10s (you're optimistic), then fix it,
- You deploy the mistake, notice the bug a few hours later, fix it, and deploy the fix,
- You deploy the mistake, the bug goes un-noticed for weeks, and even when noticed and fixed it takes weeks to clean up the mess of corrupted data that it left behind,
- You deploy the mistake, and find your company totally bankrupt 45 minutes later. Your job, your team, your organization and plans, all gone.

Ignoring the idea of "types" and "compile time", it is obvious that different environments have different levels of safety. even runtime errors can have smaller impact if caught early and are easy to debug, making Python's habit of throwing `TypeError` at runtime when something doesn't match significantly "Safer" than PHP's habit of coercing values when things don't match (which tends to mask/hide problems resulting in data-corruption and obscure/hard-to-trace bugs).

## What is Type Safety?
__Type-safety is making use of what we know of our values at compile-time to minimize the consequences of most mistakes.__

## Believe the type!
Haskell has a static type system. The type of every expression is known at compile time, which leads to safer code. That's good because it's better to 
catch errors, such as dividing a boolean by a number, at compile time instead of having your program crash at runtime! The compiler's knowledge of Types lets you mitigate the consequences of your mistakes, often turning them into straightforward-to-fix-during-development compilation errors, thus provides Safety in developing your code.

Everything in Haskell has a type, so the compiler can (quickly - yes its quick!) reason quite a lot about your program before compiling it.
Understanding the type system is a very important part of learning Haskell and other type safe languages for that matter.  

## Type safety?
See: [Li Haoyi Haoyi blog about type safety](http://www.lihaoyi.com/post/StrategicScalaStylePracticalTypeSafety.html).

The bottom line of his blog is: _...the compiler cannot help you verify_ when you aren't specific in describing the properties of the values that are present in your program using the abstract concept of types.

## What are type variables?
A type variable is a variable that can be of a certain type. It allows us to easily write very general functions if they don't use 
any specific behavior of the types in them. Functions that have type variables are called polymorphic functions.  

## What is a typeclass?
A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements 
the behavior the typeclass describes.

## What is a class constraint?
In the type definition of the following function: 

```haskell
> :t (==)
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