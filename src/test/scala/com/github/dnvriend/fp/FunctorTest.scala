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
}

/**
 * A Functors is a design pattern that exhibit certain kinds of functor-like properties and behaviors.
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
}

// the map definition of Functor must comply with the following rules...
trait FunctorLaws {
  // 1st law: If we map the id function over a functor, the functor that
  // we get back should be the same as the original functor.
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]): Boolean =
    F.map(fa)(a ⇒ a) == fa

  // 2nd law: Composing two functions and then mapping the resulting function over a functor
  // should be the same as first mapping one function over the functor and then mapping the other one.
  def composition[F[_], A, B, C](fa: F[A], f: A ⇒ B, g: B ⇒ C)(implicit F: Functor[F]): Boolean =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

// to get the test classpath on the sbt REPL, type:
// test:console
object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) ⇒ B): List[B] = fa.map(f)
  }
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) ⇒ B): Option[B] = fa.map(f)
  }
}
