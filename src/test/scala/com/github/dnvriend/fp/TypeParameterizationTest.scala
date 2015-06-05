package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class TypeParameterizationTest extends TestSpec {

  /**
   * In programming, type parameterization allows you to define methods or classes in
   * terms of a type that will be specified later.
   */

  trait Animal
  class Bird extends Animal
  class Elephant extends Animal

  class InvariantCage[A] {
    def put(animal: A): Unit = {}
  }

  class CovariantCage[+A]

  class ContraVariantCage[-A] {
    def put(animal: A): Unit = {}
  }

  "InvariantCage" should "be created with specific type" in {
    val cage: CovariantCage[Animal] = new CovariantCage[Bird]
  }

  /**
   * +A = Covariant
   * -A = Contravariant
   *  A = Invariant (default)
   *
   * Type variance complements type parameterization with a mechanism to specify constraints
   * like covariant and contravariant to the type system. The subtyping relationship gives rise to
   * the question of variance — how do two types with a subtype relationship
   * relate in type parameterization with each other?
   *
   * In Scala, you can use type parameters with classes and traits, as demonstrated previously.
   * When using type parameters for classes or traits, you can use a + sign along with the
   * type parameter to make it covariant (like the Maybe class in the previous example).
   *
   * Covariance allows subclasses to override and use narrower types than their superclass in
   * covariant positions such as the return value. Here the Nil object is a subclass of Maybe with
   * scala.Nothing as a type parameter. The reason you’re using scala.Nothing here is that the get method
   * in the Nil object throws an exception. Because the A type of Maybe is covariant, you
   * can return a narrower type from its subclasses. There’s no narrower type than Nothing
   * in Scala because it’s at the bottom of the hierarchy.
   */
}
