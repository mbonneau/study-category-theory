# scalaz-test
A small study project on [scalaz][scalaz]

## What is Scalaz
* A library for functional programming in Scala
* A library for programming with expressions making use of the type system
* An extension to the core Scala library for functional programming
* A Scala type class library
* A Principal Functional Data Structure library for Scala

It provides:
- Data types: 
  - [Disjunction (\/)][disjunction]: a result that is either an `A` or a `B`. An instance of `A \/ B` is either a `-\/[A]` (aka a "left") or a `\/-[B]` (aka a "right")
  - [NonEmptyList][nel]: A singly-linked list that is guaranteed to be non-empty
- Abstractions (type classes):
  - [Semigroup][Semigroup]: An [associative][associative] [binary operation][binary operation] subject to the [semigroup laws][semigroup laws]. 
  - [Monoid][Monoid], Provides an identity element (`zero`) to the binary `append` operation in [Semigroup][Semigroup], subject to the [monoid laws][monoid laws].
  - [Functor][Functor]: a [Functor][Functor] allows application of a function (that takes one parameter) to a value in a context: 
  ```scala
  load.ivy("org.scalaz" %% "scalaz-core" % "7.2.3")
  import scalaz._, Scalaz._
  amm> val functionThatTakesOneParameter = (_:Int) + 1
  amm> Option(1).map(functionThatTakesOneParameter) === Some(2)
  ```
  - [Apply][Apply]: An [Applicative][Applicative] without `point`.
  - [Bind][Bind]: An [Apply][Apply] [functor][functor], where a [lifted][lifting] function can introduce new values _and_ new [functor][functor] context to be incorporated into the lift context. The essential new operation of [Monad][Monad]s.
  - [Applicative][Applicative]: An [Applicative Functor][applicative programming with effects]. An [Applicative functor][lyah-applicative] allows application of a function __in a context__ to a value __in a context__, so both the function and the value are in a context:
  - [Monad][Monad]: An [Applicative][Applicative] that also supports [Bind][Bind] circumscribed by the [monad][Monad] laws
  - [Foldable][Foldable]: the ability to extract zero or more values of that type
  - [Traverse][Traverse]: Idiomatic traversal of a structure, as described in [The Essence of the Iterator Pattern][iterator-pattern-pdf]

## scalaz-contrib
[scalaz-contrib](https://github.com/typelevel/scalaz-contrib) is a library that provides interoperability libraries and additional data structures and instances for Scalaz.

## Legend
When studying the source code you will see some symbols, below is a legend:
 
Symbol | Description
------ | -----------
[\|+\|][semigrouptest] | comes from [SemigroupSyntax][semigroup-syntax] and is an 'append' operation. The `|+|` is a nice syntax for an associative binary operation
[<*>][applicativetest] | comes from [ApplySyntax][apply-syntax] and is a symbol for applying a functor containing a function and applying it on a functor containing a value, extracts the function of the first functor, and then maps it over the functor containing the value, returning the functor containing the new value  
[^][applicativetest] | comes from [ApplySyntax][apply-syntax] and is a symbol for extracts values from containers and apply them to a single function
[\|@\|][applicativetest] | comes from [ApplicativeBuilder][applicative-builder] and is a symbol for constructing Applicative expressions

## Tutorial
The following tutorial comes from the [Type-class-101-semigroup blog by Von Markus Klink](https://inoio.de/blog/2014/07/19/type-class-101-semigroup/)
and describes how to think about [type classes][type classes in scala with dan rosen] and how to use the concepts with the most simple [type class][type classes in scala with dan rosen], the _Semigroup_. 

## Higher Kinded Type
Higher-kinded types (HKT) are types that use other types to construct a new type. This is similar to how higher-order functions 
are those functions that take other functions as parameters. HKTs are also called _type constructors_ because 
they’re used to construct types. Higher kinded types are often written as `F[_]` in which the _underscore_ means it accepts
_any other_ type to construct a new type and the `F` must be replaced with a HKT like `List`. For example, the HKT `List[_]` 
takes a type to create a concrete type. For example, we can create a concrete type `List[Int]` by applying the type `Int` to the 
HKT `List[_]`. Similar we can create a concrete type `Option[String]` by applying the type `String` to the HKT `Option[_]`. 

## Type classes
A [type class][type classes in scala with dan rosen] `F` is a [higher kinded type (HKT)](http://blogs.atlassian.com/2013/09/scala-types-of-a-higher-kind/)
that describes a set of functionality for some type `A` which this type must adhere to when we build an instance of 
`F[A]` using the type constructor. This allows us to use ad hoc polymorphism and we avoid complex and brittle 
inheritance structures.

## Type class laws
Each [type class][type classes in scala with dan rosen] usually comes along with some laws, which have to be obeyed by all instances of this typeclass. 
For a semigroup it is the law of it is the law of associativity, which states that (simplified notation) 
`append(a1, append(a2, a3) ==> append(append(a1, a2), a3)`. These laws must hold at all costs.

## Implicit type class instances
Once we have defined our [type class][type classes in scala with dan rosen] we can provide instances for it. One simple case of a semigroup instance is:
  
```scala
implicit val intAppendSemigroup = new Semigroup[Int] {
  override def append(f1: Int, f2: ⇒ Int): Int = f1 + f2
}
```

Here we have defined a semigroup for Int and addition, and we know that the law of associativity holds for natural numbers. 
For multiplication we can define a semigroup:

```scala
implicit val intProductSemigroup = new Semigroup[Int] {
  override def append(f1: Int, f2: ⇒ Int): Int = f1 * f2
}
```

## Type classes, type class instances and laws
It's all about reasoning in patterns about code and algorithms. As we will construct more and more complex programms 
with the help of [type classes][type classes in scala with dan rosen], it is extremely important that we obey the laws. If we can rely on them, we can predict 
the behaviour of the code. No side effects, exceptions and obscure assumptions dampen our understanding of the code. 

We will also learn to think in patterns of behaviour and structure, much like when we use design patterns in Java.

## Putting it all together with a nice syntax
When we want to use our new semigroup there are essentially two common patterns. I call the first one direct invocation, 
and it looks like this:

```scala
Semigroup[Int].append(1,2) == 3
```
 
## Typeclassopedia
![typeclassopedia](https://github.com/dnvriend/scalaz-test/blob/master/img/typeclassopedia.png)

## Learning Resources

## Blogs
- [Eugene Yokota - Learning Scalaz](http://eed3si9n.com/learning-scalaz/)
- [Eugene Yokota - Herding Cats](http://eed3si9n.com/herding-cats/day1.html)
- [Underscore.io - Free Monads Are Simple](http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html)
- [Underscore.io - Scalaz Monad Transformers](http://underscore.io/blog/posts/2013/12/20/scalaz-monad-transformers.html)
- [Twitter - Effective Scala](http://twitter.github.io/effectivescala/)
- [Scala: Types of a higher kind by Jed Wesley-Smith](http://blogs.atlassian.com/2013/09/scala-types-of-a-higher-kind/)
- [The Typeclassopedia by John Kodumal](http://typeclassopedia.bitbucket.org/)
- [Semigroup by Von Markus Klink](https://inoio.de/blog/2014/07/19/type-class-101-semigroup/)
- [John Kurkowskiux - Accumulating More Than One Failure In A ValidationNEL](http://johnkurkowski.com/posts/accumulating-multiple-failures-in-a-ValidationNEL/)
- [Yann Moisan - Scalaz from the trenches](http://www.yannmoisan.com/scalaz.html)
- [Mavilein - Error Handling: Comparing Scala's Try with Scalaz's Either/Disjunction](http://mavilein.github.io/scala/2015/09/01/comparing-error-handling-scalas-try-with-scalazs-either-disjunction/)
- [Typelevel - Towards Scalaz (Part 1)](http://typelevel.org/blog/2013/10/13/towards-scalaz-1.html)
- [Denis Kalinin - ScalaZ Disjunctions](http://appliedscala.com/blog/2016/scalaz-disjunctions/)

## Videos
- [Learning Scalaz by Eugene Yokota](https://www.youtube.com/watch?v=jyMIvcUxOJ0)
- [Scalaz 102 Level Up Your Scalaz Foo by Colt Frederickson](https://www.youtube.com/watch?v=O5QwVqdkVtY)
- [Bill Venners - Comparing Functional Error Handling in Scalaz and Scalactic](https://www.youtube.com/watch?v=2kFigGFqML0)
- [Type classes in Scala by Dan Rosen][type classes in scala with dan rosen]
- [Oliver Daff - Functor, Apply, Applicative, Bind and Monad (or How to explain FP with the help of Mario and Luigi)](https://www.youtube.com/watch?v=3Ycp55QEbGM)

## Other projects on GitHub
- [Channing Walton - My tinkering to understand the typeclassopedia](https://github.com/channingwalton/typeclassopedia)
- [Inoio - SimpleZ](https://github.com/inoio/simplez)

---

[scalaz]: https://github.com/scalaz/scalaz
[apply-syntax]: https://github.com/scalaz/scalaz/blob/ea856759e60d0d3fbf2becc7b4e1918ecdf70085/core/src/main/scala/scalaz/syntax/ApplySyntax.scala
[semigroup-syntax]: https://github.com/scalaz/scalaz/blob/ea856759e60d0d3fbf2becc7b4e1918ecdf70085/core/src/main/scala/scalaz/syntax/SemigroupSyntax.scala
[semigrouptest]: https://github.com/dnvriend/scalaz-test/blob/master/src/test/scala/com/github/dnvriend/semigroup/SemigroupTest.scala
[applicativetest]: https://github.com/dnvriend/scalaz-test/blob/master/src/test/scala/com/github/dnvriend/applicative/ApplicativeTest.scala
[applicative-builder]: https://github.com/scalaz/scalaz/blob/ea856759e60d0d3fbf2becc7b4e1918ecdf70085/core/src/main/scala/scalaz/syntax/ApplicativeBuilder.scala
[lifting]: https://wiki.haskell.org/Lifting
[associative]: https://en.wikipedia.org/wiki/Associative_property
[lyah-applicative]: http://eed3si9n.com/learning-scalaz/Applicative.html
[binary operation]: https://en.wikipedia.org/wiki/Binary_operation
[disjunction]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Either.scala
[nel]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/NonEmptyList.scala
[Semigroup]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Semigroup.scala
[semigroup laws]: https://en.wikipedia.org/wiki/Semigroup
[Monoid]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monoid.scala
[monoid laws]: https://en.wikipedia.org/wiki/Monoid
[Functor]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Functor.scala 
[Apply]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Apply.scala
[Bind]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Bind.scala
[Applicative]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Applicative.scala
[Monad]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monad.scala 
[Foldable]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Foldable.scala
[Traverse]: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Traverse.scala
[scalaz]: (https://github.com/scalaz/scalaz).
[iterator-pattern-pdf]: http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf 
[applicative programming with effects]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html
[type classes in scala with dan rosen]: https://www.youtube.com/watch?v=sVMES4RZF-8
[railway oriented programming]: http://fsharpforfunandprofit.com/rop/