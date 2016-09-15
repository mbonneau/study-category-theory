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

package com.github.dnvriend.collection

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._

class CollectionTest extends TestSpec {

  it should 'option in {
    Option(1) ++ Option(2) shouldBe
      List(1, 2)

    Option(1) ++ None shouldBe
      List(1)

    None ++ Option(2) shouldBe
      List(2)

    (Option(1) |+| Option(2)) shouldBe
      Option(3)

    (Option(1) |@| Option(2))(_ + _) shouldBe
      Option(3)

    Option(List(1, 2)).sequenceU shouldBe
      List(Option(1), Option(2))
  }

  it should 'list in {
    List(1) :+ 2 shouldBe
      List(1, 2)

    List(1) ++ List(2) shouldBe
      List(1, 2)

    (List(1) |+| List(2)) shouldBe
      List(1, 2)

    (List(1) |@| List(2))(_ + _) shouldBe
      List(3)

    // sequence with an unapply
    List(Set(1, 2, 3)).sequenceU shouldBe
      Set(1, 2, 3)

    List(Option(1), Option(2), Option(3)).sequenceU shouldBe
      Option(List(1, 2, 3))

    List(Option(1), Option.empty[Int], Option(3)).sequenceU shouldBe
      None

    List(Option.empty[Int], Option.empty[Int], Option(3)).sequenceU shouldBe
      None
  }

  it should 'set in {
    Set(1) + 2 shouldBe
      Set(1, 2)

    Set(1) ++ Set(2) shouldBe
      Set(1, 2)

    (Set(1) |+| Set(2)) shouldBe
      Set(1, 2)
  }

  it should 'map in {
    Map(1 → 1) + (1 → 2) shouldBe
      Map(1 → 2)

    Map(1 → 1) ++ Option(1 → 2) shouldBe
      Map(1 → 2)

    Map(1 → 1) ++ None shouldBe
      Map(1 → 1)

    (Map(1 → 1) |+| Map(1 → 2)) shouldBe
      Map(1 → 3)

    Map(1 → List(1)) + (1 → List(2)) shouldBe
      Map(1 → List(2))

    Map(1 → List(1)) ++ Map(1 → List(2)) shouldBe
      Map(1 → List(2))

    (Map(1 → List(1)) |+| Map(1 → List(2))) shouldBe
      Map(1 → List(1, 2))

    (Map(1 → List(1), 2 → List(2)) |@| Map(1 → List(2), 2 → List(3)))(_ ++ _) shouldBe
      Map(1 → List(1, 2), 2 → List(2, 3))

    (Map(1 → List(1)) |@| Map(1 → List(2)) |@| Map(1 → List(3)))(_ ++ _ ++ _) shouldBe
      Map(1 → List(1, 2, 3))

    (Map(1 → List(1)) |@| Map(1 → List(2)) |@| Map(2 → List(3)))(_ ++ _ ++ _) shouldBe
      Map.empty[Int, List[Int]]

    (Map(1 → List(1)) |@| Map(2 → List(2)))(_ ++ _) shouldBe
      Map.empty[Int, List[Int]]

    Map(1 → List(1, 2, 3)).sequenceU shouldBe
      List(Map(1 → 1), Map(1 → 2), Map(1 → 3))
  }

}
