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

class FunctorTest extends TestSpec {

  // Now for some Functors :)
  // see: http://eed3si9n.com/learning-scalaz/Functor+Laws.html

  /**
   * Functors are transformations from one category to another that can also transform and preserve morphisms.
   * A morphism is the changing of one value in a category to another in the same category. In the example of the
   * category of cats, a morphism would be akin to a box that takes a dim cat and converts it into a neon glowing cat.
   *
   * In the category of types, the most commonly used in computer science, a morphism is a function that converts
   * from one type to another type. The functor would be something that converts cats into dogs. The functor would be able
   * to convert dim cats into dim dogs and glowing cats into glowing dogs. The functor could also convert the box so
   * that it can convert dim dogs into glowing dogs.
   */

  /**
   * ==Overview==
   * A Functors is a design pattern that exhibit certain kinds of
   * functor-like properties and behaviors.
   *
   * ==Property 1==
   * A functor should, for any type `A` in the original category,
   * construct a type `T[A]` in the new category.
   *
   * ==Property 2==
   * A functor must, given a type `T[A]` in the new category,
   * and a morphism (A => B) in the original category,
   * create a value `T[B]` in the new category.
   */
  trait Functor[T[_]] {
    def apply[A](x: A): T[A]
    def map[A, B](x: T[A])(f: A => B): T[B]
  }

  // example

  class Box[+A](x: A) {
    def get: A = x
    def map[B](f: A => B): Box[B] = Box(f(x))
    def flatMap[B](f: A => Box[B]) = f(x)
  }

  object Box {
    def apply[A](x: A) = new Box(x)
  }

  class BoxAsFunctor extends Functor[Box] {
    override def apply[A](x: A): Box[A] = Box(x)
    override def map[A, B](x: Box[A])(f: (A) => B): Box[B] = x.map(f)
  }
}

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
