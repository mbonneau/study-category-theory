#  From Functions to Monads

## Introduction
"Monad" that's a word you hear quite often these days. There is a lot of unjustified Fear Uncertainty and Doubt whenever this word pops up. 
This paper shows how to derive Monads from what you already know and give names to things you maybe never even thought about. This paper is 
an adaption from the very good [original blog post by Raichoo](http://raichoo.blogspot.nl/2011/07/from-functions-to-monads-in-scala.html).  

##  What are Categories?
Since Monads come from category theory  we first have to define what a [category](https://en.wikipedia.org/wiki/Category_(mathematics)) is. A category is a 
**mathematical structure** that has **two basic properties**, a set of **objects** (also known as **values** or **concepts**) and a set of **morphisms** 
(also known as **transformation functions** or **arrows**). 

## The first property; set of objects
The **first** property of a category is _the set of objects_. In both computer science (and therefor also in Java, Scala and other programming languages) 
and in category theory, **objects are members of a collection**. In Scala, _**a category is the collection of values associated with a certain type**_. 

* For example, the collection of objects (also called **values** or **concepts**) in “**the category of type Int**” is  **-2^31 to 2^31-1**. 
* The collection of objects in “**the category of type Long**” it is **-2^63 to 2^63-1**. 
* The collection of objects in “**the category of type Seasons**”, in which seasons consists of the values **{ Spring, Summer, Fall, Winter }** are _‘Spring’, ‘Summer’, ‘Fall’ and ‘Winter’_. 

We can also broaden the category. For example, when we think about objects in “**the category ‘all-the-types-in-the-Scala-programming-language’**”, 
the collection of objects is virtually unlimited, but to give an example, objects that are part of that collection are “foo”, “bar”, the boolean literal ‘true’, 
the Int literal ‘1’, but also an OutputStream instance, and many more values.

## The second property; set of morphisms
The **second** property of a category is **the collection of processes called morphisms** (also called **transformation functions** or **arrows**) that go between values. 
Morphisms are (all) the possible ways to get from one value **of that category** to another value **of that category**. The important thing about morphisms is that they 
can be composed. In the Scala programming language, a morphism is implemented as a **function** and as you know, functions can be composed. So for example, 
in “**the category of Int**”, possible values are ‘1’ and ‘2’. How can we go from ‘1’ to ‘2’, well by means of a morphism (an arrow / transformation function). 
So, when we define a function `val f = (x: Int) => x + 1` and apply the value 1 to it: `f(1)`, we get 2.

##  A category is a mathematical structure
So a **category** is a mathematical structure, is of a certain **type**, and is formalized in terms of a collection of **objects** (values/concepts), which are all 
the possible values of the category’s type, and a second collection that consists of all the possible **morphisms** (functions/arrows) that go between values. 

Beyond these two properties by which the category is defined, the category has two **behaviors**:

1. It can compose morphisms (functions/arrows)
2. has an identity function that does nothing. It just takes a value and returns it unmodified. 

## The formal definition of a category
So, a category **C** consists of two **sets** and has two **behaviors**:

- _ob_(C): the objects of the category **C**,
- _hom_(C): the morphisms of the category **C**; which are the transformations that go between objects of category **C**
- It can _compose_ functions and it has an _identity_ function

We call _ob(**C**)_ the **objects** of **C** and _hom(**C**)_ the **morphisms** of **C**. 

## Recap
In the category of “**all-the-types-in-the-Scala-programming-language**” or shorter “**the category of all types**”,
let **f** be a function of type **Int => String**. Both **Int** and **String** are elements of the set _ob(**C**)_ and **f** is an element of the set _hom(**C**)_. 
So ”**the category of all types**” has types like **Int** and **String** and Int(s) can be converted to String(s) by means of the **morphism** (function/arrow) f. 

There's more to a **category**, for example, it needs to be able to **compose** morphisms and there has to be a special element called **identity**. Let's looks at _functions_ first. 

# Functions (Morphisms between values of a category)
A function is a mathematical construct that operates on the values of the category. It can only operate within the bounds of the category. 
When the category is of type Int, in can only operate on type Int and in the range of Int. It can define any transformation, but within that bound. 
When the category is broader, for example the category “the category of all types”, then the function can do more, for example transform Int => String. 

Let’s look at function composition next.

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
$ scala
Welcome to Scala 2.11.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_77).
Type in expressions for evaluation. Or try :help.

scala> val f: Int => Int = (x: Int) => x + 1
f: Int => Int = <function1>

scala> f(1)
res0: Int = 2

scala> // or shorter

scala> val f = (_:Int) + 1
f: Int => Int = <function1>

scala> f(1)
res1: Int = 2
```

Using the [Ammonite](https://github.com/lihaoyi/Ammonite) REPL: (brew install ammonite-repl):

```scala
$ amm
Welcome to the Ammonite Repl 0.5.7
(Scala 2.11.8 Java 1.8.0_77)

amm@ val f = (_:Int) + 1
f: Int => Int = <function1>
amm@ f(1)
res1: Int = 2
```

Using the [Haskell](https://www.haskell.org/) REPL: (brew install ghc):

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let f x = x + 1
ghci> f 1
2
```
  
Note: I get the nice `ghci> ` instead of the `Prelude> ` prompt because I have created a file `~/.ghci` with the following content:
  
```bash
:set prompt "ghci> "
```

[Ghci](https://wiki.haskell.org/GHC/GHCi) will load all the contents of this file each time it is started.
  
## Function Composition
[Function composition](https://wiki.haskell.org/Function_composition) is the act of pipelining 
the result of one function, to the input of another, creating an entirely new function.

So, functions can be composed to produce new functions. This means that these
new functions can also be composed because they are also functions, etc, etc.

Please take a moment to think about this, because we will leverage function composition
in every aspect of our every day programming work from now on! Functions are the ultimate
building blocks because they can be composed to create new building blocks.

For example:
Let **f** and **g** be functions:  

- f: A => B  
- g: B => C
- h: f compose g = A => C

We can **compose** those 2 functions in Scala by using the [compose method](http://www.scala-lang.org/api/current/#scala.Function1),
which is just a method of the [Function1](http://www.scala-lang.org/api/current/#scala.Function1) class compose these two functions
and get a new function from A => C:

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
$ scala

Welcome to Scala 2.11.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_77).
Type in expressions for evaluation. Or try :help.

scala> val f = (_:Int) + 1
f: Int => Int = <function1>

scala> val g = (_:Int) + 2
g: Int => Int = <function1>

scala> val h = f compose g
h: Int => Int = <function1>

scala> h(1)
res0: Int = 4
```

Haskell uses the [dot operator](https://wiki.haskell.org/Function_composition) to compose 
functions:

Using the [Haskell](https://www.haskell.org/) REPL: (brew install ghc):

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let f x = x + 1
ghci> let g x = x + 2
ghci> let h = g . f
ghci> h(1)
4
```

To return to our **category** discussion, we just need the special element called **identity** which basically is a function that does nothing. 
It just takes a value and returns it unmodified. So when we compose a function **f** with **identity** we get back a function that is 
equivalent to **f** and it should not matter if we compose f with identity or identity with f. 

Now we know what a **category** is, it is a mathematical structure, it is of a certain type, it is formalized in terms of a collection of **objects** 
(values/concepts), which are all the possible values of the category’s type, and a second collection that consists of all the possible **morphisms** 
(functions/arrows) that go between values, it has a special element the **identity function**, and it can **compose morphisms** (functions/arrows).

## What is a kind? 
What is a kind? We know what types and values are. You can think of them as levels. A value like **1** and **x => x** (a function value) live a the **value level** 
while types like **Int**, **String** and the type **Int => Int** live at the **type level**. We **group values into types**. 
For example, `true` and `false` are both **values** of type **Boolean** (**think of types as sets of values**). 

OK, if we group values into types, can we group types? Yes, we can! **We can group types into kinds, so kinds are a grouping of types**. 

## Type Constructors
We now have a basic idea of what **kinds** are (a grouping of types), so what are higher kinded types? Like high order functions 
(functions that take functions as arguments and yield functions), higher kinded types are types that take [type constructors](http://debasishg.blogspot.nl/2009/01/higher-order-abstractions-in-scala-with.html) 
as arguments and yield types.

## Higher kinded types
Think about a Scala List. It's not a type by itself, you need to feed it a type as an argument to get a type. This is called a **Type Constructor**. List takes one type in 
its type constructor and returns a type. The Scala notation for a type constructor like List is `List[_]`. 
For example to create a list of type String you’ll have to type: `List[String]`. 

So a higher kinded type looks something like this: `trait Functor[F[_]]` where `F` could be a `List[_]`, `Option[_]`, `Future[_]` or any other **type constructor** 
that takes one argument.

## Functors (Morphisms between categories)
Now that we have **higher kinded types**, let's take a look at our functions **f** and **g** from the beginning and let's add a **type constructor** `F[_]` to the mix 
(this is Scala notation and means: _takes one type and yield a type_). We can now produce a whole **new set of types** with this higher kind (e.g. `F[Int]`, `F[String]` etc.). 

We could now create functions that work with these types... hey I know this. This sounds like... a **category**! Yes, it is! It's actually a **subcategory*
of the **category “all-types-in-the-scala-language”**. Let's call this new category **D** (so we don't confuse it with the **type constructor** F). 

So elements of _ob(**D**)_ would be something like `F[A]` and for _hom(**D**)_ (the morphisms, functions/arrows) it would be something like `F[A] => F[B]`.
  
Now wouldn't it be cool if there was a morphism between those two categories, one that preserves composition and the identity rule? Something that could convert a function of 
type `A => B` to a function of type `F[A] => F[B]`? That's what we call a **Functor**, **it's a morphism that operates between categories**. Wait what? Well, `F[A]` is a category, 
“**the category of A’s**” and `F[B]` is a category, ”**the category of B’s**”.  The Functor is a mathematical constructs that makes it possible to apply a function `A => B` to objects of the 
“**category-of-A’s**” and point to an object of the “**category-of-B’s**”, between the two categories, so it is now possible to get an `F[B]` by applying the function `A => B` with 
preservation of the composition and identity rule. 

Please note that `F[A]` does not know about B’s, it is the “category-of-A’s”, it only knows of ‘A’s’ so in that ‘world’ it would be impossible to apply the function `A => B`, 
but the Functor makes it possible because the Functor **knows how to convert** a plain old function `A => B` to a function of `F[A] => F[B]`.  

For example if `F[A]` is an `Option[Int]`, then some of the possible values of this category are `Some(1)`, `Some(2500)`, `Some(10)`, `Option.empty[Int]`, and so on. 
If `F[B]` is an `Option[String]`, possible values of this category are `Some(“foo”)`, `Some(“bar”)`, `Some(“baz”)`, `Option.empty[String]`. 

There is no way for the function `A => B` to operate on these categories. It has first to be converted to eg. `Option[A] => Option[B]`, then it can operate on the values. 
The Functor is the mathematical construct that knows how to convert the function `A => B` to `F[A] => F[B]` and preserves the composition and identity rule.

"Why is this useful?" you might ask. Just think about the kind of code reuse you could achieve, when we could define only one type of function that could be applied to all 
types without having to take into account that there are many subcategories (domains/partitions) in systems that could not support the function. We have to create custom 
functions for that subcategory, for example, a function that can only operate on Option, or only on List or only on Future, that would be a pain! 

Let's look at Option as a concrete example. So how do we convert a function of type `Int => Int` to a function of type `Option[Int] => Option[Int]` using the Functor Option? 

In Scala all Functors have the map method, that method converts a plain function `A => B` to a function of `F[A] => F[B]`, taking into account the type of the Functor, so if we 
use the map method of the Functor Option we would convert the function `A => B` to `Option[A] => Option[B]`, and using the map method of the Functor List we would convert the 
function `A => B` to `List[A] => List[B]` and so on.

Let's check it out, first we define a function.

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
scala> val f = (_: Int) + 1
f: Int => Int = <function1>
```
We can now use the function **f** with Option, there was no need to write a special version of **f** that only works with Option, 
it also works on other Functors like List, Future, etc. 

```scala
scala> Option(1) map f
res0: Option[Int] = Some(2)
```

The second ingredient we need to make our Functor complete is a morphism that 
maps a value of type A to a value of type `F[A]`. In the case of Option this is just 
the factory (creational pattern) function: 

```scala
scala> Option(1)
res1: Option[Int] = Some(1)
```

## Endofunctors (A functor that maps a category to itself)
Now, let's assume that we want something like this: 

```scala
scala> :paste
Some(1) map {
    case x if x % 2 == 0 => Some("YES!")
    case _               => None
}
res0: Option[Option[String]] = Some(None)
```

We want to use a function of type **Int => Option[String]** with Option's map method. But what's that? 
We get an **Option[Option[**String**]]**. That's stupid, now I have to unpack one Option to get the result I actually wanted. 

What happened here? Remember how we mapped a function of type **A => B** to a function of type **F[A] => F[B]** in the previous 
section about functors? What we did now is: we mapped a function of type **Int => Option[String]** to a function of **Option**[Int] => **Option[Option**[String**]]**. 
Yikes! We mapped something into an Option **and** into an Option this is how we ended up with an **Option[Option**[String**]]**. 
You can think of this as a result of a **composition** of two Option Functors. 

This is actually a special kind of Functor that maps a category unto itself and is called an **Endofunctor**, so when you end up with 
**Option[Option**[A**]]**, or **List[List**[A**]]** or a **Future[Future**[A**]]** or more abstract, **mapped a category unto itself**, 
this is a mathematical structure called an **Endofunctor** (read on).

## Natural Transformations (Morphisms between Functors)
So we have the following:

- Morphisms between objects of a single [Category](https://en.wikipedia.org/wiki/Category_(mathematics)) which are the **functions** we all know, 
- **Functors** which are **morphisms between categories**  

Now it's time to introduce **morphisms between Functors**.

Why do we need this? Well in the last example we ended up with an **Endofunctor** (we mapped a Category unto itself). 
We need to get rid of one of the Options. We do this with a morphism that maps the composition of 
two **Functors F (F composed with F)** to the **Functor F**. 

Let's do a concrete example that involves List: 

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
scala> List(1,2,3) map { x => List(x + 1) }
res1: List[List[Int]] = List(List(2), List(3), List(4))
```

It happened again, but this time we ended up with a **List[List**[Int**]]**. We now need a 
[Natural Transformation](https://en.wikipedia.org/wiki/Natural_transformation) to join those lists 
together, this **Natural Transformation** is called **flatten** in Scala. 

```scala
scala> List(1,2,3) map { x => List(x + 1) } flatten
res2: List[Int] = List(2, 3, 4)
```

In Scala, List comes with a method that does both in one step, it's called **flatMap**. 

```scala
scala> List(1,2,3) flatMap { x => List(x + 1) }
res3: List[Int] = List(2, 3, 4)
```

This enables us to chain functions together in a very convenient way. 

```scala
scala> Some(1) flatMap { x => Some(2) flatMap { y => Some(x + y) } }
res4: Option[Int] = Some(3)
```

or a little more concise 

```scala
scala> Some(1) flatMap { x => Some(2) map { y => x + y } }
res5: Option[Int] = Some(3)
```

## Monads (A monoid in the category of endofunctors)
This is actually what makes up a Monad. It's an **Endofunctor** (A functor that maps a category to itself) with 
two **Natural Transformations** (morphisms between Functors) called **unit** and **join** (which is called flatten in Scala). 
They are such a fundamental concept that Scala features a syntactic sugar for them, the [for-comprehension](http://nerd.kelseyinnis.com/blog/2013/11/12/idiomatic-scala-the-for-comprehension/). 

The last example above can be written in the following way: 

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
scala> :paste
for {
    x <- Option(1)
    y <- Option(2)
} yield (x + y)
res6: Option[Int] = Some(3)
```

Which translates to:

```scala
scala> Option(1) flatMap(x => Option(2) map(y => x + y))
res7: Option[Int] = Some(3)
```

## Purpose
Monads are a very general algebraic construction. They are not only limited to Option and List. 
In general, monads host an effect. There are also types one will encounter in practice that have very much 
Monad-like behaviour, but are not actually. Let’s consider some of them using the perspective of their effect:

- [Option[A]](http://www.scala-lang.org/api/2.11.8/#scala.Option): The potential containment of a single item of type `A`. Represents optional values. Instances of Option are either an instance of scala.Some or the object None.
- [List[A]](http://www.scala-lang.org/api/2.11.8/#scala.collection.immutable.List): The containment of a linear sequence of items of type `A`. It representing an ordered collections of elements of type T. This class comes with two implementing case classes scala.Nil and scala.:: that implement the abstract members isEmpty, head and tail.                                                                                                                                                                                                                                                                                              
- [Set[A]](http://www.scala-lang.org/api/2.11.8/#scala.collection.immutable.Set): The containment of any number of unique items (it contains no duplicate elements) of type `A`. 
- [Map[K, V]](http://www.scala-lang.org/api/2.11.8/#scala.collection.immutable.Map): The containment of any number of unique keys of type `K` that point to one value of type `V`.
- [Try[T]](http://www.scala-lang.org/api/2.11.8/#scala.util.Try): Harbours a fallible computation, which either succeeds with some value of type `T`, or fails with some Throwable. The Try type represents a computation that may either result in an exception, or return a successfully computed value. It's similar to, but semantically different from the scala.util.Either type. Instances of `Try[T]`, are either an instance of `scala.util.Success[T]` or `scala.util.Failure[T]`.
- [Future[T]](http://www.scala-lang.org/api/2.11.8/#scala.concurrent.Future): Represents an asynchronous computation whose outcome (of type `T`) may become available in the future, or will explicitly fail if one explicitly stops waiting for its result. May be seen as an asynchronous Try, although it is good practice to not flatten a Future[Try[T]] to Future[T].
- [Validation[T]](http://eed3si9n.com/learning-scalaz/Validation.html): (commonly available outside the Scala library). Represents a validated value of type T, or a list of the violations if at least one validation failed for the value. Similar to Try; but where a Try[T] hosting some computation that may result in a T outcome will fail at the first exception encountered, Validation[T] will allow the continued application of validation rules, such that all violations may be captured.

`Try` and `Future` are typically not considered actual Monads from a category theory perspective. For the developer, this usually makes no difference, 
but for those interested, one may consult [stack-overflow](http://stackoverflow.com/questions/27454798/is-future-in-scala-a-monad) and the blog ['Scala's Either, Try and the M word' by Maurício Linhares](http://mauricio.github.io/2014/02/17/scala-either-try-and-the-m-word.html).

##  Scala Typeclassopedia
[Haskell](https://www.haskell.org/) is a functional programming language. Haskell uses [type classes](http://eed3si9n.com/learning-scalaz/polymorphism.html#Ad-hoc+polymorphism) 
to help structure functional code by means of functional design patterns, which is a structured way to solve common problems in functional code. 
The [Scala](http://www.scala-lang.org/) programming language’s standard library however, does not contain these type classes, so it is not possible 
for a functional programmer using Scala to leverage these design patterns. [Scalaz](https://github.com/scalaz/scalaz) is a library that tries to bring 
some of the concepts from Haskell into the Scala world. The following graph is the Typeclassopedia (encyclopedia of type classes) that Scalaz 
brings to the scala world. The Typeclassopedia is a set of interrelated type classes that have proven extremely handy for **structuring functional code**. 
These type classes help you solve common problems in functional code in a structured way. 

![Image Here]

**Legend**:
Solid arrows: is-a relation: every Monad is an Applicative is a Functor, every Traversable is a Functor
The dotted line is some other relationship: (can be used by?)
The greyed out things: typeclasses that are not in the Haskell standard libraries but are in Scalaz v7+

##  Functor
A [Functor](http://eed3si9n.com/learning-scalaz/Functor.html) is a function that can be applied to a value. This turns out to be very useful. 
If you start with the number 1 and apply a function to it, for example the function: `val f =  (_: Int) + 2` and apply it `f(1)` you arrive at the answer 3. 
Simple enough. But a Functor has an important twist.

## Computational Context
For example, if you have a domain object, such as a **House** and you want **to apply a function to a value in the house**, say a specific room, 
then House needs to have a **method** on its class that **takes a function** as an argument. That function **will be used by House** to apply it 
to the room to produce an output. **The output stays in the house**. For example, perhaps your functions paints the room. The room is still there 
and the original color is still there. But now there is a new color that covers the room.

A Functor is a mathematical construct and a **design pattern** that **applies a function to a value inside of a computational context** like a House. 
There are a number of [fancy rules](https://github.com/scalaz/scalaz/blob/series/7.1.x/core/src/main/scala/scalaz/Functor.scala#L90) that go along with this 
key idea. In scala, the **map** operation on class `A` means that class `A` is a Functor assuming it obeys the Functor rules that are not detailed here. 
If you use the example of a list, a Functor would allow you to apply a function to each element of a list and return a list where each element has been 
replaced by the function's output for that element.

That's it. In the programmer's world, a Functor is usually expressed as a class that has a **map** function although there are other properties, 
the functor laws that must also be true.

## Scala List[A] is a Functor
A Scala List is a good way to play around and understand the functor concepts. A scala List takes a type parameter eg. Int to create a List of type Int 
`List[Int]`. We can construct the list and apply the function to each element of the list using the map method. The new values stay in the List's computational context.

Using the [Scala](http://www.scala-lang.org/) REPL: (brew install scala):

```scala
scala> List[Int](1,2,3).map ((_: Int) + 1)
res8: List[Int] = List(2, 3, 4)
```

## Scala Option[A] is a Functor
A Scala Option is a Functor because it has a **map** method that applies the function to the value it contains. 
The new value stays in the Option's computational context.

```scala
scala> Option(1).map((_:Int)+1)
res9: Option[Int] = Some(2)
```

##  Scala Future[A] is a Functor
A Scala Future is a Functor because it has a **map** method that applies the function to the value it contains. 
The new value stays in the Future's computational context.

```scala
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
scala> Future[Int](1).map ((_: Int) + 1).foreach(println)
2
```

## Scala contains no types from category theory
The Scala standard library **does not** contain a Functor type. To be more precise, the Scala standard library does not contain any 
of the types from category theory. It does not contain Functor, Applicative, Monad and so on. Scala contains types that can have the 
behavior of for example a Functor or Monad. These “behavioral types” have certain methods (like map or flatMap) that, when applied 
can behave like a Functor or Monad. Some of these types we already know, the List, Option and Future are some examples of Scala types 
that behave like types from category theory and there are many more. These “behavioral types” **do not** carry the name like eg. 
ListFunctor, OptionFunctor or FutureFunctor nor inherit behavior from a supertype like eg. Functor. 

So how do we know which types are actually a Functor or a Monad? That is a very good question and not easy to answer. 
One way is to look at the API documentation of a type and look for the marker methods like **map** and **flatMap**. 

## Scalaz’s Functor Typeclass
A Functor has a bit of strange syntax. To use a scalaz functor, you need to specify a type that takes a type. A scala List takes a type 
parameter so we can use List as the type parameter to the Functor smart constructor. Since the smart constructor does not take any arguments, 
you just write `Functor[List]` to create an instance. Once you have an instance you can use it directly.

## Semigroup
A **Semigroup** is an object that [express an binary association operation](https://www.youtube.com/watch?v=x9hoPIMNPw4). 
It is defined for a set, for example, like the set of Int in scala. This type of design pattern may seem so high-level as to be useless. 
However, it expresses the most basic idea of **append** for the set it describes. If you add numbers together, add items to a list or "add" 
together two of your domain objects then a **Semigroup** expresses this concept.

Using the [Ammonite](https://github.com/lihaoyi/Ammonite) REPL: (brew install ammonite-repl):

```scala
load.ivy("org.scalaz" %% "scalaz-core" % "7.2.2")
import scalaz._
import Scalaz._
//
amm@ val s = Semigroup[Int]
s: Semigroup[Int] = scalaz.std.AnyValInstances$$anon$5@13df2f89
amm@ s.append(1,2)
res1: Int = 3
amm@ val slist = Semigroup[List[Int]]
slist: Semigroup[List[Int]] = scalaz.std.ListInstances$$anon$4@ae04104
amm@ slist.append(List(1, 2), List(3, 4))
res2: List[Int] = List(1, 2, 3, 4)
```

For more information: [Learning Scalaz - Semigroup](http://eed3si9n.com/learning-scalaz/Monoid.html#Semigroup)

