# What are the implicit resolution rules
Anwsered by [Daniel C. Sobral at StackOverflow](http://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits)

## Types of Implicits
Implicits in Scala refers to either

1. a value that can be passed "automatically", so to speak, or
2. a conversion from one type to another that is made automatically.

## Implicit Conversion
Speaking very briefly about the latter type, a conversion from one type to another, if one calls a method m on an object
o of a class C, and that class does not support method m, then Scala will look for an implicit conversion from C to
something that does support m.

A simple example would be the method map on String:

```scala
"abc".map(_.toInt)
```

String does not support the method map, but `StringOps` does, and there's an implicit conversion from String to StringOps
available (see implicit def `augmentString` on `Predef`).

## Implicit Parameters
The other kind of implicit is the implicit parameter. These are passed to method calls like any other parameter, but
the compiler tries to fill them in automatically. If it can't, it will complain. One can pass these parameters
explicitly, which is how one uses breakOut, for example (see question about breakOut, on a day you are feeling up for a challenge).

In this case, one has to declare the need for an implicit, such as the foo method declaration:

```scala
def foo[T](t: T)(implicit integral: Integral[T]): Unit = {
 println(integral)
}
```

## View Bounds
(Aren't these removed in Scala 2.12?)
There's one situation where an implicit is both an implicit conversion and an implicit parameter. For example:

```scala
def getIndex[T, CC](seq: CC, value: T)(implicit conv: CC => Seq[T]) = seq.indexOf(value)

getIndex("abc", 'a')
```

The method getIndex can receive any object, as long as there is an implicit conversion available from its
class to `Seq[T]`. Because of that, I can pass a String to getIndex, and it will work.

Behind the scenes, the compiler changes `seq.IndexOf(value)` to `conv(seq).indexOf(value)`.

This is so useful that there is syntactic sugar to write them. Using this syntactic sugar, getIndex can be defined like this:

```scala
def getIndex[T, CC <% Seq[T]](seq: CC, value: T) = seq.indexOf(value)
```

This syntactic sugar is described as a view bound, akin to an upper bound `(CC <: Seq[Int])` or a lower bound `(T >: Null)`.

## Context Bounds
Another common pattern in implicit parameters is the type class pattern. This pattern enables the provision of common
interfaces to classes which did not declare them. It can both serve as a bridge pattern -- gaining separation of concerns --
and as an adapter pattern.

Type classes are a programming pattern borrowed from Haskell (the word “class” has nothing to do with classes in
object oriented programming). We encode them in Scala using traits and implicits. A type class is a parameterised
trait representing some sort of general functionality that we would like to apply to a wide range of types:

```scala
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}
```

We implement our type class with instances for each type we care about. If we want the instances to automatically
be in scope we can place them in the type class’ companion object. Otherwise we can place them in a separate library
object for the user to import manually.

The Integral class (from the Stackoverflow question) is a classic example of type class pattern.
Another example on Scala's standard library is `Ordering`. There's a library that makes heavy use of this pattern, called Scalaz.

This is an example of its use:

```scala
def sum[T](list: List[T])(implicit integral: Integral[T]): T = {
    import integral._   // get the implicits in question into scope
    list.foldLeft(integral.zero)(_ + _)
}
```

There is also syntactic sugar for it, called a context bound, which is made less useful by the need to refer to the implicit.
A straight conversion of that method looks like this:

```scala
def sum[T : Integral](list: List[T]): T = {
    val integral = implicitly[Integral[T]]
    import integral._   // get the implicits in question into scope
    list.foldLeft(integral.zero)(_ + _)
}
```

Context bounds are more useful when you just need to pass them to other methods that use them. For example,
the method sorted on Seq needs an implicit Ordering. To create a method reverseSort, one could write:

```scala
def reverseSort[T : Ordering](seq: Seq[T]) = seq.sorted.reverse
```

Because `Ordering[T]` was implicitly passed to reverseSort, it can then pass it implicitly to sorted.

## Where do Implicits come from?
When the compiler sees the need for an implicit, either:

1. because you are calling a method which does not exist on the object's class,
2. or because you are calling a method that requires an implicit parameter,

The compiler will search for an implicit that will fit the need. This search obey certain rules that define which implicits
are visible and which are not.

It is advicable to now go and see this very good presentation from two great guys, Joshua Suereth and Dick Wall about
[Youtube - Part 1: Scala Implicits, the Real Story - 1'33hr](https://www.youtube.com/watch?v=SH8aGSWJRHE) and
[Youtube - Part 2: Part 2: Scala Implicits, the Real Story - 1'19hr](https://www.youtube.com/watch?v=hBjTHl9cA3c)
about implicits.

## Implicit resolution rules
The implicits available under number 1 below has precedence over the ones under number 2.

If there are several eligible arguments which match the implicit parameter’s type, a most specific one will be chosen
using the rules of static overloading resolution (see Scala Specification §6.26.3). More detailed information can be
found in a question I link to at the end of this answer.

1. First look in current scope
  - Implicits defined in current scope
  - Explicit imports
  - wildcard imports
  - _(*) Same scope in other files (*)_

2. Now look at associated types in
  - Companion objects of a type
  - Implicit scope of an argument's type (2.9.1)
  - Implicit scope of type arguments (2.8.0)
  - Outer objects for nested types
  - Other dimensions

Let's give some examples for them:

## 1. Implicits Defined in Current Scope

```scala
implicit val n: Int = 5
def add(x: Int)(implicit y: Int) = x + y
add(5) // takes n from the current scope
```

## 1. Explicit Imports

```scala
import scala.collection.JavaConversions.mapAsScalaMap
def env = System.getenv() // Java map
val term = env("TERM")    // implicit conversion from Java Map to Scala Map
```

### 1. Wildcard Imports

```scala
def sum[T : Integral](list: List[T]): T = {
    val integral = implicitly[Integral[T]]
    import integral._   // get the implicits in question into scope
    list.foldLeft(integral.zero)(_ + _)
}
```

## (*) Same Scope in Other Files
__Edit:__ It seems this does not have a different precedence. If you have some example that demonstrates a
precedence distinction, please make a comment. Otherwise, don't rely on this one.

This is like the first example, but assuming the implicit definition is in a different file than its usage.
See also how package objects might be used in to bring in implicits.

## 2. Companion Objects of a Type
There are two object companions of note here. First, the object companion of the __"source"__ type is looked into.

(1) For instance, inside the object `Option` there is an implicit conversion to `Iterable`, so one can call Iterable
methods on Option, or pass Option to something expecting an Iterable. For example:

```scala
for {
    x <- List(1, 2, 3)
    y <- Some('x')
} yield, (x, y)
```

That expression is translated by the compiler to

```scala
List(1, 2, 3).flatMap(x => Some('x').map(y => (x, y)))
```

However, `List.flatMap` expects a `TraversableOnce`, which Option is not. The compiler then looks inside Option's
object companion and finds the conversion to Iterable, which is a TraversableOnce, making this expression correct.

(2) Second, the companion object of the expected type:

```scala
List(1, 2, 3).sorted
```

The method sorted takes an implicit `Ordering`. In this case, it looks inside the object Ordering, companion to the
class Ordering, and finds an implicit Ordering[Int] there.

Note that companion objects of super classes are also looked into. For example:

```scala
object A {
    implicit def str(a: A) = "A: %d" format a.n
}
class A(val n: Int)

class B(val x: Int, y: Int) extends A(y)

val b = new B(5, 2)

val s: String = b  // s == "A: 2"
```

This is how Scala found the implicit `Numeric[Int]` and `Numeric[Long]` of the Stackoverflow question, by the way,
as they are found inside Numeric, not Integral.

## 2. Implicit Scope of an Argument's Type
If you have a method with an argument type A, then the implicit scope of type A will also be considered.
By "implicit scope" I mean that all these rules will be applied recursively -- for example, the companion object
of A will be searched for implicits, as per the rule above.

Note that this does not mean the implicit scope of A will be searched for conversions of that parameter, but of the
whole expression. For example:

```scala
object A {
  implicit def fromInt(n: Int) = new A(n)
}
class A(val n: Int) {
  def +(other: A) = new A(n + other.n)
}

// This becomes possible:
1 + new A(1)
// because it is converted into this:
A.fromInt(1) + new A(1)
This is available since Scala 2.9.1.
```

## 2. Implicit Scope of Type Arguments
This is required to make the type class pattern really work. Consider Ordering, for instance: It comes with some
implicits in its companion object, but you can't add stuff to it. So how can you make an Ordering for your own class
that is automatically found?

Let's start with the implementation:

```scala
object A {
 implicit val ord = new Ordering[A] {
   def compare(x: A, y: A) = implicitly[Ordering[Int]].compare(x.n, y.n)
 }
class A(val n: Int)
}
```

So, consider what happens when you call

```scala
List(new A(5), new A(2)).sorted
```

As we saw, the method sorted expects an `Ordering[A]` (actually, it expects an Ordering[B], where B >: A).
There isn't any such thing inside Ordering, and there is no "source" type on which to look. Obviously, __it is
finding it inside A, which is a type argument of Ordering__.

This is also how various collection methods expecting `CanBuildFrom` work:
_the implicits are found inside companion objects to the type parameters of CanBuildFrom_.

__Note:__ Ordering is defined as trait `Ordering[T]`, where `T` is a type parameter. Previously, I said that
Scala looked inside type parameters, which doesn't make much sense. The implicit looked for above is
Ordering[A], where A is an actual type, not type parameter: it is a type argument to  Ordering.
See section 7.2 of the Scala specification.

This is available since Scala 2.8.0.

## 2. Outer Objects for Nested Types
I haven't actually seen examples of this. I'd be grateful if someone could share one. The principle is simple:

```scala
object A {
  implicit def bToString(b: A#B) = "B: %d" format b.m
}
class A(val n: Int) {
  class B(val m: Int) { require(m < n) }
}
val a = new A(5)
val b = new a.B(3)
val s: String = b  // s == "B: 3"
```

## 2. Other Dimensions
I'm pretty sure this was a joke, but this answer might not be up-to-date. So don't take this question as being the
final arbiter of what is happening, and if you do noticed it has gotten out-of-date,
please inform me so that I can fix it.

(It was a joke.)

## Related questions of interest:

- [Context and view bounds](http://stackoverflow.com/questions/4465948/context-and-view-bounds-again/4467012)
- [Chaining implicits](http://stackoverflow.com/questions/5332801/how-can-i-chain-implicits-in-scala)
- [Scala: Implicit parameter resolution precedence](Scala: Implicit parameter resolution precedence)
- [conversion 1 to bigint](http://stackoverflow.com/questions/7647835/how-does-1-bigint1-work-and-how-can-i-do-the-same/7648364#7648364)
- [scala-implicit-parameter-resolution-precedence](http://stackoverflow.com/questions/8623055/scala-implicit-parameter-resolution-precedence)
- [revisiting-implicits-without-import-tax](http://eed3si9n.com/revisiting-implicits-without-import-tax#comment-400276618)

## Eugene yokota
The current scope also includes:

- implicit parameters of the current method,
- outer blocks,
- inherited implicits,
- and implicits made available by package object.

The only precedence among the current scope is:
- locally defined one vs the rest in 2.9.1, but it's a bug.
- After 2.10, all of them within current scope have the same precedence. The same goes for implicit scope.
- When there are more than one candidate, static overloading rule is used, which means the implicits defined
  in an object wins over implicits defined in its supertrait etc.



## The package objects of the companions of the parts of the type are also searched.
[SI-4427](https://issues.scala-lang.org/browse/SI-4427). Things defined in the package object are "current scope"
for other stuff in the package, so they'd be handled either as current scope in the same file or as same scope, different
file, as the case may be.

In this case, it's part of the implicit scope. The call site need not be within that package. That was surprising to me.

7, actually. They 8th is a joke. Yes, these source are ordered by priority.
Within a category, there's ambiguity, except in the case of inheritance.
An inherited implicit has lower precedence than one not inherited.

Odersky said we look in the "implicit scope", which contains all sort of companion objects that bear some relation to the type which we search for -- "type which we search for" is the expected type. Josh says on 5.1.3 "implicit scope of the type it's looking for" -- again, expected type
