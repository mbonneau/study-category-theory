package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class FunctorTest extends TestSpec {

  // see: https://www.youtube.com/watch?v=Dsd4pc99FSY
  // see: http://www.drmaciver.com/2008/03/existential-types-in-scala/

  /**
   * Scala is a Typed Language. This means that everything has a type, so there are different kinds-of-types
   * everywhere we look in Scala. The most simplest form eg. the one below where we define a new value
   * named 'x' of type 'Int' and assign it the value '1'. The value 'xs' is of type List[Int] (List-of-Int)
   * and is initialized with List(1, 2, 3).
   */

  val x: Int = 1
  val xs: List[Int] = List(1, 2, 3)

  /**
   * We call the type of x, (the Int-type) and the type of xs (the List[Int]), "proper-types".
   * This means that for the value x, the type that can be applied to x should be an Int (like 1)
   * and for xs the values that can be applied must be a List-of-Int. So we are talking about an Int and
   * a List-of-Int (thus the List[Int]).
   *
   * Because scala's type system is polymorphic, subtypes are also allowed.
   */

  /**
   * A question arises is what do we call the type 'Int' or the type 'List' by itself. Please note the kind of
   * 'List', so not the kind of 'List[Int]'
   *
   * In type systems we call the types-by-itself, (so the Int, List, String, etc) "kinds".
   *
   * The 'kind' of 'Int' is an "A", it's a normal kind. The same is true for a String, a Long, a Double, a java.io.File,
   * a BigDecimal, java.util.UUID, scala.io.Source, etc.. We can initialize it as an Int or as a String or whatever
   * 'proper-type' we choose.
   *
   * You can ask the REPL by typing :k Int, and the response should be something like:
   *
   * "scala.Int's kind is A"
   *
   * (try out some other proper-types you can think of)
   */

   /**
   * The 'kind' of 'List' is a bit more complex. It is a higher-kinded-type (don't you just love Scala :)
   *
   * When we ask the REPL for the kind of 'List' it will respond with:
   *
   * "scala.collection.immutable.List's kind is F[+A]"
   *
   * So the kind of 'List' is an F-of-A, written as F[A]. This means, in order to use the List, you must first put
   * an A (a proper-type) in it and then Scala will return us a proper type for the List that could be List[Int], so
   * List[Int] is called a 'proper-type' or 'applied-type'. Scala has support for sub/super-type invariance that is
   * written by -A and +A, but let's leave that for another time.
   *
   * Knowing that we can only assign a proper type to a value or variable, it means that we cannot write (it just wouln't compile):
   *
   * val xs: List = ???
   *
   * Because the kind of List is an F[A], it has an argument. Granted, a different kind of argument than we are
   * used to, until now, because it has an argument of a 'proper type', but an argument the same. These kind of types that
   * have an argument we call type-constructors. Type constructors, well, construct types and return proper types that
   * can be assigned to values or variables, etc.
   */

  /**
   * Like Java, Scala also supports generics. For example, to make a generic method that only applies to proper types
   * we could write:
   */

  def foo[A](x: A, y: A): A = x

  /**
   * The generic foo function first registers the proper-type with brackets [A], we call those 'type-parameters',
   * and then the proper type can be used in the method for example for the parameter x and for the parameter y,
   * and also for the return type.
   *
   * Note that when A is assigned a type, ALL A's are of that type, so when we use an Int as the proper type for A.
   * The type of A can be inferred by applying the method with an instant of that type (like 1) or given explicitly.
   */

  "Foo" should "be called" in {
    foo[Int](1, 1) // both a and b are of type Int
    foo(1, 1) // inferred
  }

  /**
   * A generic method can have multiple type parameters:
   */

  def foo2[A, B](x: A, y: B): A = x

  "Foo2" should "be called" in {
    foo2[Int, Long](1, 1L) // a is of type Int and b is of type Long
    foo2(1, 1L) // inferred
  }

  def foo3[A, B, C](x: A, y: B, z: C): A = x

  "Foo3" should "be called" in {
    foo3[Int, Long, Double](1, 1L, 1.0) // a is of type Int, b = Long and C = Double
    foo3(1, 1L, 1.0) // inferred
  }

  /**
   * The methods foo, foo2 and foo3 all can be called with proper types like an Int, String, Double etc. Introducing
   * multiple proper types as identifier gives us the option to define those types with eg, Int, Long and Double.
   *
   * Can we do the same for the higher-kinded types like List-and-the-like? Yes you can!
   *
   * For example, how do we create a method that can be called with two List[Int] or two Option[Int] etc.
   *
   * What we will need is two type parameters, one that defines a higher-kinded type with one argument and the other
   * one that defines the proper type for which the type-constructor will be applied to:
   */

  def bar[F[_], A](x: F[A], y: F[A]): F[A] = x

  "bar" should "be called" in {
    bar(List(1, 2, 3), List(1, 2, 3))
    bar(Option(1), Option(2))
  }

  // Now for some Functors :)
  // see: http://eed3si9n.com/learning-scalaz/Functor+Laws.html
}

/**
 * A Functors is a design pattern that exhibit certain kinds of functor-like properties and behaviors.
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// the map definition of Functor must comply with the following rules...
trait FunctorLaws {
  // 1st law: If we map the id function over a functor, the functor that
  // we get back should be the same as the original functor.
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]): Boolean =
    F.map(fa)(a => a) == fa

  // 2nd law: Composing two functions and then mapping the resulting function over a functor
  // should be the same as first mapping one function over the functor and then mapping the other one.
  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]): Boolean =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

// to get the test classpath on the sbt REPL, type:
// test:console
object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
  }
}
