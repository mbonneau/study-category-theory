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

package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class TypeParameterizationTest extends TestSpec {

  /**
   * This is basically a study into Polymorphism.
   *
   * There are two forms of polymorphism:
   *  1. Subtyping (which is used in OO first)
   *  2. Generics (which is used by FP first)
   *
   *  How subtyping and generics interact:
   *
   *  1. Type Bounds - subject type parameters to subtype constraints A <: B (upperbound) and A >: B (A is a supertype of B) (lowerbound)
   *  2. Variance - how parameterized types behave under subtyping
   *
   * In programming, type parameterization allows you to define methods or classes in
   * terms of a type that will be specified later.
   *
   * Liskov substitution principle
   *
   * if A <: B, (A is-a-subtype of B) then everything one can do with
   * a value of type B one should also be able to do with a value of type A
   */

  /**
   * Definition of variance
   * Say, C[T] is a parameterized type and A, B are types such that A <: B
   *
   * In general there are 3 possible relationships between C[A] and C[B]:
   *
   * 1. C[A] <: C[B] then C is `covariant`
   * 2. C[A] >: C[B] then C is `contravariant`
   * 3. neither C[A] nor C[B] is a subtype of the other then C is `nonvariant`
   *
   */

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
