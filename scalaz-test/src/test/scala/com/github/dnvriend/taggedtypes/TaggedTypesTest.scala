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

package com.github.dnvriend.taggedtypes

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._

class TaggedTypesTest extends TestSpec {
  // see: http://eed3si9n.com/learning-scalaz/Tagged+type.html
  // see: http://etorreborre.blogspot.nl/2011/11/practical-uses-for-unboxed-tagged-types.html
  // see: http://timperrett.com/2012/06/15/unboxed-new-types-within-scalaz7/
  /**
   * Tagged Types in Scalaz
   *
   * When looking at value classes or case classes for that matter, the values are wrapped and to get to those values you'll
   * have to call the public accessor method of that value, which is no fun... well, Scalaz Tagged Types to the rescue!
   */

  sealed trait KiloGram
  def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
  def kiloGramToValue(mass: Double @@ KiloGram): Double @@ KiloGram = mass

  it should "create a new tagged type using the method 'Kilogram'" in {
    val mass: Double @@ KiloGram = KiloGram(20.0)
    2 * Tag.unwrap(mass) shouldBe 40.0
    kiloGramToValue(KiloGram(20.0)) shouldBe 20.0
  }

  // lets define another Tagged Type
  sealed trait JoulePerKiloGram
  def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)

  // note: Tag.unsubst removes the Tag, leaving A
  def relativisticEnergy(mass: Double @@ KiloGram): Double @@ JoulePerKiloGram =
    JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unsubst[Double, Id, KiloGram](mass))

  it should "calculate relativisticEnergy using correct types" in {
    relativisticEnergy(KiloGram(20.0)) shouldBe 1.79751035747363533E18
  }
}
