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

package com.github.dnvriend

import cats.Foldable
import cats.Monoid
import cats.instances.all._
import cats.syntax.foldable._

import scala.language.higherKinds

class FoldableTest extends TestSpec {
  def fold[A: Monoid, F[_]](fa: F[A])(implicit foldable: Foldable[F]): A = foldable.fold(fa)

  it should "fold a List of Int" in {
    fold(List(1, 2)) shouldBe 3
    fold(List.empty[Int]) shouldBe 0 // due to the Monoid[Int].empty
  }

  it should "fold a List of String" in {
    fold(List("a", "b")) shouldBe "ab"
    fold(List.empty[String]) shouldBe "" // due to the Monoid[String].empty
  }

  it should "fold an Option of Int" in {
    fold(Option(1)) shouldBe 1
    fold(Option.empty[Int]) shouldBe 0 // due to the Monoid[Int].empty
  }

  it should "combineAll for Int" in {
    List(1, 2).combineAll shouldBe 3
    List.empty[Int].combineAll shouldBe 0
  }
}
