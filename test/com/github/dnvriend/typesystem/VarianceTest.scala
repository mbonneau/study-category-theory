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

import com.github.dnvriend.TestSpec

class VarianceTest extends TestSpec {

  /**
   * ==TL;DR==
   * ''Variance refers to the ability of type parameters to change or vary on higher-kinded types, like T[A].''
   * "Use an [+] wildcard when you only get values out of a structure,
   * use a [-] wildcard when you only put values into a structure,
   * and don't use a wildcard when both get and put"
   *
   * ==Overview==
   * The type system of many programming languages support subtyping. For instance, if Cat is subtype of Animal,
   * then an expression of type Cat can be used whenever an expression of type Animal could.
   *
   * Variance refers to how subtyping between more complex types, container types or more precise, higher kinded types,
   * like T[A], (eg. List[Cat] vs List[Animal]) relates to subtyping. Depending on the variance of the type constructor,
   * the subtyping relation may be either preserved, reversed, or ignored.
   *
   * Variance is a way of declaring how type parameters can be changed to create conformant types.
   *
   * A higher-kinded type T[A] is said to conform to T[B] if you can assign T[B] to T[A] without causing any errors.
   *
   * The rules of variance govern the type conformance of types with parameters. Variance takes three forms:
   * invariance T[A], covariance T[+A] , and contravariance T[-A].
   */

  /**
   * ==Invariance==
   * ==TL;DR==
   * ''It does not vary''
   *
   * ==Overview==
   * ''Invariance T[A] refers to the unchanging nature of a higher-kinded type parameter.''
   *
   * A higher-kinded type that’s invariant implies that for any types T, A, and B if T[A] conforms to T[B] then A must
   * be the equivalent type of B. You can’t change the type parameter of T. Invariance is the default for any
   * higher-kinded type parameter.
   */

  class Invariant[A] {
    var a: A = _
    def get: A = a
    def put(a: A): Unit = this.a = a
  }

  "Invariance" should "Invariant[Object] can be assigned to Invariant[Object]" in {
    "val x: Invariant[Object] = new Invariant[Object]" should compile
  }

  it should "Invariant[String] cannot be assigned to Invariant[Object]" in {
    "val x: Invariant[Object] = new Invariant[String]" shouldNot compile

    /**
     * This might seem strange, even though logically it seems like it should be,
     * because a String is a subtype of Object.
     *
     * This is the effect of invariance. The point is that variance is about the container,
     * the higher kinded type eg. a List, Set, or parameterized type like Invariant. It is the
     * rules we specify on the container (the higher kinded type).
     *
     * Covariant parameters (on higher kinded types) do not have this restriction (so read on)
     */
  }

  it should "be able to assign subclasses because Sub[Object] is a subclass of Invariant[Object]" in {
    class Sub[A] extends Invariant[A]
    "val x: Invariant[Object] = new Sub[Object]" should compile
  }

  /**
   * ==Covariance==
   * ==TL;DR==
   * ''It can vary downwards''
   *
   * ==Overview==
   * ''Covariance T[+A] refers to the ability to substitute a type parameter with its parent type'';
   * For any types T, A and B if T[A] conforms to T[B] then A <: B
   *
   * The Mammal and Cat relationship is such that the Cat type conforms to the Mammal type; if a method requires
   * something of type Mammal, a value of type Cat could be used. If a type T were defined as covariant, then the type
   * T[Cat] would conform to the type T[Mammal]: A method requiring a T[Mammal] would accept a value of type T[Cat].
   *
   * The conformance of T is the same (co-) as the conformance of its type parameters. The easiest example of this is
   * a list, which is higher-kinded on the type of its elements. You could have a list of strings or a list of integers.
   *
   * Because Any is a supertype of String, we can use a list of strings where a list of Any is expected.
   *
   * Creating a Covariant parameter is as easy as adding a + symbol before the type parameter; T[+A].
   *
   * When we declare List[+A], we are saying that List is covariant in the parameter A. What that means is that it
   * takes a type, say Parent, to a new type List[Parent], and if Child is a subtype of Parent,
   * then List[Child] will be a `subtype` of List[Parent].
   */

  class Covariant[+A](val a: A) {
    def get: A = a
  }

  "Covariance" should "Covariant[Object] can be assigned to Covariant[Object]" in {
    "val x: Covariant[Object] = new Covariant[Object](new Object)" should compile
  }

  it should "Covariant[String] can be assigned to Covariant[Object]" in {
    "val x: Covariant[Object] = new Covariant[String](new String)" should compile
    // being covariant, only A or subtypes of A may be assigned, and because A is of type
    // 'Object', anything will do, like eg. a String.
  }

  it should "assigned a List[String] to List[Object]" in {
    val xs: List[Any] = List.empty[String]
    val ys: List[Any] = List.empty[Nothing] // A = Any, the top-level type and Nothing is the bottom level type
    // A List is Covariant, thus List[+A]
    // being covariant, only A or the subtypes of A can be assigned, in this case
    // any type of A like String, Int, and so on can be assigned
  }

  /**
   * ==Contravariance==
   * ==TL;DR
   * ''It can vary upwards''
   *
   * ==Overview
   * ''Contravariance T[-A] is the opposite of covariance, the ability to substitute a type parameter with a subtype.''
   * For any types T, A and B, if T[A] conforms to T[B] then A >: B.
   *
   * If the type T is defined as contravariant, then a method expecting a type of T[Cat] would accept a value of type T[Mammal].
   * Notice that the direction of the conformance relationship is opposite (contra-) that of the Mammal--Cat relationship.
   *
   * Contravariance can be harder to reason through but makes sense in the context of a Function object. A Function object
   * is covariant on the return type and contravariant on the argument type. Intuitively this makes sense. You can take the
   * return value of a function and cast it to any supertype of that return value. As for arguments, you can pass any subtype
   * of the argument type. You should be able to take a function of Any => String and cast it to a String => Any but not vice versa.
   *
   * When we declare List[-A], we are saying that List is contravariant in the parameter A. What that means is that it
   * takes a type, say Child, to a new type List[Child], and if Child is a subtype of Parent,
   * then List[Child] would be a `supertype` of List[Parent].
   */

  class Contravariance[-A]

  "Contravariance" should "not assign a List[Object] to List[String]" in {
    // Scala's List is Covariance, thus List[+A], the following is illegal (type mismatch),
    // the required type is List[String] and not List[Object]
    "val xs: List[String] = List.empty[Object]" shouldNot compile
  }

  it should "Contravariance[String] can be assigned to Contravariance[String]" in {
    "val x: Contravariance[String] = new Contravariance[String]" should compile
  }

  it should "Contravariance[Object] can be assigned to Contravariance[String]" in {
    "val x: Contravariance[String] = new Contravariance[Object]" should compile
    // being contravariant, only A or the supertypes of A can be assigned, in this case
    // as far as we know, only java.lang.Object is the only type that can be assigned
  }
}
