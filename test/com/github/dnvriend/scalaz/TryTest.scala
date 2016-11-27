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

import scala.util.{ Success, Try }
import scalaz.Scalaz._
import scalaz._
import scalaz.outlaws.std.utilTry._

/**
 * You can use the same sequence function if you have the Try monad from scalaz-outlaws in scope.
 *
 * (As explained in the comments, this code violates the functor composition laws and so might lead
 * to unexpected behaviour when used with some functions. It should be fine for your use case though)
 *
 * Q: could you explain why converting List[Try[A]] to Try[List[A]] break the laws? I can't find any comment in this thread.
 *
 * A: It doesn't - that's what I meant by "your use case".
 * But in other cases the Try monad violates the laws.
 */
class TryTest extends TestSpec {
  it should "sequence a list of Try" in {
    val m = implicitly[Monad[Try]]
    m.sequence(List(Try("foo"), Try("bar"))) shouldBe Success(List("foo", "bar"))
    List(Try("foo"), Try("bar")).sequence shouldBe Success(List("foo", "bar"))
  }
}
