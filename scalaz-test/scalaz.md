# Scalaz
An extension to the core Scala library for functional programming

---

## Modularity
Scalaz has a modular architecture:

* scalaz-core: Type class hierarchy, data structures, type class instances for the Scala and Java standard libraries, implicit conversions / syntax to access these.
* scalaz-effect: Data structures to represent and compose IO effects in the type system.
* scalaz-concurrent: Actor and Future implementation
* scalaz-iteratee: Experimental new Iteratee implementation

---

## Imports
```scala
import scalaz._ // imports type names
import scalaz.Id.Id // imports Id type alias
import scalaz.std.option._ // imports instances, converters, and functions related to `Option`
import scalaz.std.AllInstances._ // imports instances and converters related to standard types
import scalaz.std.AllFunctions._ // imports functions related to standard types
import scalaz.syntax.monad._ // injects operators to Monad
import scalaz.syntax.all._ // injects operators to all typeclasses and Scalaz data types
import scalaz.syntax.std.boolean._ // injects operators to Boolean
import scalaz.syntax.std.all._ // injects operators to all standard types
import scalaz._, Scalaz._ // all the above
```

---

## Uber import
```scala
// imports *all* implicit conversions that provide *syntax* 
// as well as type class instances and other functions
// aka  the uber import, to be backwards compatible with Scalaz 6

import scalaz.Scalaz._  
```

# Links
- [Scalaz cheat sheet](http://eed3si9n.com/learning-scalaz/scalaz-cheatsheet.html)
- [David R. MacIver - Existential types in Scala](http://www.drmaciver.com/2008/03/existential-types-in-scala/)
- [TypeLevel - How can we map a Set?](http://typelevel.org/blog/2014/06/22/mapping-sets.html)
- [James Iry - Scala Option will save you from null](http://james-iry.blogspot.nl/2010/08/why-scalas-and-haskells-types-will-save.html)

# Scalaz none[A]
None has type None.type. scalaz provides none[A] as a convenience method for None as an Option[A].

```scala
import scalaz._, Scalaz._
scala> None
res0: None.type = None

scala> None: Option[Int]
res1: Option[Int] = None

scala> Option.empty[Int]
res2: Option[Int] = None

scala> none[Int]
res3: Option[Int] = None
```

# scalaz some
Some(1) returns a Some[Int]. 1.some returns an Option[Int], which is probably what you actually want.

```scala
scala> Option(1)
res20: Option[Int] = Some(1)

scala> Some(1)
res21: Some[Int] = Some(1)

scala> 1.some
res22: Option[Int] = Some(1)
```

# scalaz type class apply method
scalaz type classes provide apply methods in their companion objects that are shortcuts to grab the implicit instance

```scala
scala> implicitly[Equal[Int]].equal(1, 3)
res23: Boolean = false

scala> Equal[Int].equal(1, 3)
res24: Boolean = false

scala> implicitly[Functor[IList]].map(IList(1, 2, 3))(_ + 1)
res25: scalaz.IList[Int] = [2,3,4]
              ^
scala> Functor[IList].map(IList(1, 2, 3))(_ + 1)
res26: scalaz.IList[Int] = [2,3,4]
```

# Monoid#instance gives you a way of concisely creating a Monoid instance:

```scala
scala> case class Foo(bar: Int)
defined class Foo                                                                                     ^

scala> implicit val fooMonoid: Monoid[Foo] = Monoid.instance( { (x, y) => Foo(x.bar + y.bar) }, Foo(0))
fooMonoid: scalaz.Monoid[Foo] = scalaz.Monoid$$anon$6@12ef69cf

scala> (1 to 5).map(Foo.apply).toList.suml
res27: Foo = Foo(15)
```