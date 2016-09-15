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

package com.github.dnvriend.futureops

import com.github.dnvriend.TestSpec

import scala.concurrent.Future
import scalaz._
import Scalaz._

class FutureOpsTest extends TestSpec {

  it should "put a value into a Context (here a Future) using 'point'" in {
    1.point[Future] shouldBe a[Future[Int]]
  }

  it should "attempt the future, should return a Future[Disjunction[Throwable, A]]" in {
    1.point[Future].attempt.futureValue shouldBe 1.right
    Future(1).attempt.futureValue shouldBe 1.right
  }

  it should "be mapped over" in {
    1.point[Future].map(_ => 2).futureValue shouldBe 2
  }
}
