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

package com.github.dnvriend.disjunction

import com.github.dnvriend.TestSpec
import org.typelevel.scalatest.DisjunctionMatchers

import scalaz._

class DisjunctionMatchersTest extends TestSpec with DisjunctionMatchers {
  // "org.typelevel" %% "scalaz-scalatest" % "1.0.0" % Test

  // TypeLevel provides a project called scalaz-scalatest
  // see: https://github.com/typelevel/scalaz-scalatest
  // that provides Scalatest bindings for Scalaz, to be more
  // specific, it provides bindings for:

  // - Disjunction
  // - Validation

  // The disjunction matcher provides the following methods:

  // - beLeft(element: E)
  // - left[E]
  // - beRight[T](element: T)
  // - right[T]

  it should "beRight with value in disjunction test" in {
    Disjunction.right("Hello World") should beRight("Hello World")
  }

  it should "beLeft with value in disjunction test" in {
    Disjunction.left("Hello World") should beLeft("Hello World")
  }

  it should "right, not care whats inside" in {
    Disjunction.right("Foo") should be(right)
  }

  it should "left, not care whats inside" in {
    Disjunction.left("Foo") should be(left)
  }
}
