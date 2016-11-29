/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend.typesystem

import java.io.{ FileInputStream, InputStream }

import com.github.dnvriend.TestSpec

class TypeTest extends TestSpec {

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
   * This means that for the value x, the type that can be applied to x should be an Int (like eg. the number 1)
   * and for xs the values that can be applied must be a List-of-Int.
   *
   * Because scala's type system is polymorphic, subtypes are also allowed.
   */

  val ys: Seq[Int] = List(1, 2, 3) // List is a subtype of Seq
  def head[F[_] <: Iterable[_], A](xs: F[A]): Any = xs.head

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

  /**
   * From the book: Scala in Depth (please buy, it's great!)
   *
   * The Scala Type System
   * =====================
   * The type system is an important component of the Scala language. It enables lots of
   * rich optimizations and constraints to be used during compilation, which helps runtime
   * speed and prevents programming errors. The type system allows us to create
   * all sorts of interesting walls around ourselves, known as "types". These walls help prevent
   * us from accidentally writing improper code. This is done through the compiler
   * tracking information about variables, methods, and classes. The more you
   * know about Scala’s type system, the more information you can give the compiler,
   * and the type walls become less restrictive while still providing the same protection.
   *
   * The type system will constantly warn you of problems or prevent you from doing things altogether.
   * The better you communicate with the type system, the less restrictive it becomes. But if you attempt
   * to do something deemed inappropriate, the compiler will warn you. The compiler can be a great means
   * of detecting errors (early) if you give it enough information.
   *
   * What are types?
   * ===============
   * A type is a set of information the compiler knows. This could be anything from “what
   * class was used to instantiate this variable” to “what collection-of-methods are known to
   * exist on this variable.” The user can explicitly provide this information, or the compiler
   * can infer it through inspection of other code. When passing or manipulating variables,
   * this information can be expanded or reduced, depending on how you’ve written your methods.
   *
   * In Scala, types can be defined in two ways:
   *  1. Defining a class, trait or object.
   *  2. Directly defining a type using the "type" keyword.
   *
   * Defining types using Class, Trait or Object
   * ===========================================
   * Defining a class, trait, or object automatically creates an associated type for the class,
   * trait, or object. This type can be referred to using the same name as the class or trait.
   * For objects we refer to the type slightly differently due to the potential of classes or
   * traits having the same name as an object.
   */

  class ClassName
  trait TraitName
  object ObjectName

  def foo(x: ClassName): ClassName = x
  def bar(x: TraitName): TraitName = x
  def baz(x: ObjectName.type): ObjectName.type = x

  /**
   * Directly defining a type using the "type" keyword
   * =================================================
   * Scala also allows types to be constructed using the "type" keyword. This can be used to
   * create both concrete and abstract types. Concrete types are created by referring to
   * existing types, or through structural types. Abstract types are created as place holders
   * that you can later refine in a subclass. This allows a significant level of abstraction
   * and type safety within programs.
   *
   * The type keyword can only define types within some sort of "context", specifically within a class,
   * trait, or object, or within subcontext of one of these. The syntax of the type keyword is simple.
   * It consists of the keyword itself, an identifier, and, optionally, a definition or constraint for
   * the type. If a "definition" is provided, the type is "concrete". If no constraints or assignments are
   * provided, the type is considered "abstract".
   */

  type SomeFooType = String // referring to an existing type
  type SomeBarType = Iterable[Char] // referring to an existing type
  type AbstractType // no constraints nor assignments, it's abstract
  type ConcreteType = SomeFooType
  type ConcreteType2 = SomeFooType with SomeBarType

  /**
   * Side note: types are also useful for naming your types, which could be useful in tuples
   */

  type PersonUUID = String
  type FirstName = String
  type LastName = String

  val personTuple: (PersonUUID, FirstName, LastName) = (id, FirstName, LastName)
  // looks better than (String, String, String)

  /**
   * Notice that concrete types can be defined through combining other types. This new type is referred to
   * as a "compound type". The new type is satisfied only if an instance meets all the requirements of both
   * original types. The compiler will ensure that these types are compatible before allowing the combination.
   */

  /**
   * Structural types
   * ================
   * In Scala, a structural type is created using the type keyword and defining what method "signatures" and
   * variable "signatures" you expect on the desired type. This allows a developer to define an abstract interface
   * without requiring users to extend some trait or class to meet this interface.
   *
   * Note: Structural types make use of reflection to assert whether or not a type meets all requirements defined
   * by the structural types (the method and variable signatures). When performance is an issue (so when *not* doing
   * I/O) don't use structural types, else it is no problem.
   *
   * A common use case for structural types is resource handling, where a resource must always be closed, eg.
   * we could define a type Closable that is a structural type that states that a "Closable" is-a closable when
   * it has a method 'close(): Unit'
   */

  type Closable = { def close(): Unit } // define a structural type that only defines a method signature

  def using[A <: Closable, B](stream: A)(f: A => B): B =
    try f(stream) finally stream.close

  "Resource" should "always be closed after being used" in {
    val inputStream: InputStream = new FileInputStream(".gitignore")
    val lines: List[String] = using(inputStream) { (is: InputStream) =>
      scala.io.Source.fromInputStream(is).getLines().toList
    }
    lines should not be 'empty
  }

  /**
   * Type constraints
   * ================
   * Type constraints are rules associated with a type that must be met for a variable to match the given
   * type. A type can be defined with multiple constraints at once. Each of these constraints must be satisfied
   * when the compiler is type checking expressions. Type constraints take the following two forms:
   *  - Lower bounds (subtype restrictions)
   *  - Upper bounds (supertype restrictions, also known as Conformance relations)
   *
   * Lower bound restrictions can be thought of as super-restrictions. This is where the type selected must be equal
   * to or a supertype of the lower bound restriction
   */

}
