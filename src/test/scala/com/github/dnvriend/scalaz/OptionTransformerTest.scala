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

package com.github.dnvriend.scalaz

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._
import scalaz.OptionT._
import scala.concurrent.Future

class OptionTransformerTest extends TestSpec {

  it should "compose Future(None) and Future(None)" in {
    val f: Future[Option[Int]] = (for {
      a ← optionT(Future.successful(Option.empty[Int]))
      b ← optionT(Future.successful(Option.empty[Int]))
    } yield a + b).run
    f.futureValue should not be 'defined
  }

  it should "compose Future(None) and Future(Option(2))" in {
    val f: Future[Option[Int]] = (for {
      a ← optionT(Future.successful(Option.empty[Int]))
      b ← optionT(Future.successful(2.some))
    } yield a + b).run
    f.futureValue should not be 'defined
  }

  it should "compose Future(Option(1)) and Future(None)" in {
    val f: Future[Option[Int]] = (for {
      a ← optionT(Future.successful(1.some))
      b ← optionT(Future.successful(Option.empty[Int]))
    } yield a + b).run
    f.futureValue should not be 'defined
  }

  it should "compose Future(Option(1)) and Future(Option(2))" in {
    val f: Future[Option[Int]] = (for {
      a ← optionT(Future.successful(1.some))
      b ← optionT(Future.successful(2.some))
    } yield a + b).run
    f.futureValue.value shouldBe 3
  }

  it should "map Future(Some)" in {
    def determine[A]: PartialFunction[Option[A], Future[Unit]] = {
      case Some(_) ⇒ Future.successful(println("some-1"))
      case _       ⇒ Future.successful(println("none-1"))
    }
    for {
      x ← Future.successful(Some(1))
      y ← determine(x)
    } yield ()
  }

  it should "map Future(None)" in {
    def determine[A]: PartialFunction[Option[A], Future[Unit]] = {
      case Some(_) ⇒ Future.successful(println("some-2"))
      case _       ⇒ Future.successful(println("none-2"))
    }
    for {
      x ← Future.successful(Option.empty)
      y ← determine(x)
    } yield ()
  }
}