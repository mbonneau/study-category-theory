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

package com.github.dnvriend.custom

import com.github.dnvriend.TestSpec

import scala.concurrent.{ ExecutionContext, Future }

case class ABFuture[+A](future: Future[Option[A]]) extends AnyVal {
  def flatMap[B](f: A => ABFuture[B])(implicit ec: ExecutionContext): ABFuture[B] = {
    val newFuture = future.flatMap {
      case Some(a) => f(a).future
      case None    => Future.successful(None)
    }
    ABFuture(newFuture)
  }

  def map[B](f: A => B)(implicit ec: ExecutionContext): ABFuture[B] = {
    ABFuture(future.map(option => option.map(f)))
  }
}

class CustomFutureFunctorTest extends TestSpec {

}
