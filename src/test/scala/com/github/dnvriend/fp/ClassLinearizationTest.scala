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

/**
 * see: http://tech.pro/blog/2114/scala-linearization-technique-to-avoid-multiple-inheritance
 */
class ClassLinearizationTest extends TestSpec {

  class BaseClass {
    def print(): List[String] = List("BaseClass")
  }

  trait Trait1 extends BaseClass {
    override def print(): List[String] = "Trait1" :: super.print()
  }

  trait Trait2 extends BaseClass {
    override def print(): List[String] = "Trait2" :: super.print()
  }

  trait Trait3 extends Trait2 {
    override def print(): List[String] = "Trait3" :: super.print()
  }

  "Class Linearization" should "derived1" in {
    // For Derived1, because it extends 1 trait, the lineair form is simple
    // and can be intuitively calculated:
    //
    // L(Derived1) = Derived, Trait1, BaseClass
    //
    class Derived1 extends Trait1 { // Trait1 extends BaseClass
      override def print(): List[String] = "Derived1" :: super.print()
    }
    new Derived1().print() shouldBe List("Derived1", "Trait1", "BaseClass")
  }

  it should "choose which super to get the result from" in {
    class Derived2 extends Trait2 { // Trait2 extends BaseClass
      override def print(): List[String] = "Derived2" :: super[BaseClass].print() ::: super[Trait2].print()
    }
    println(new Derived2().print())
  }

  final val Name = "Test" // it will be inlined by the compiler just like a final static String TEST = "test"

  //  it should "derived2" in {
  //    // The same for Derived2:
  //    //
  //    // L(Derived2) = Derived2, Trait2, BaseClass
  //    class Derived2 extends Trait2 { // Trait2 extends BaseClass
  //      override def print(): List[String] = "Derived2" :: super.print()
  //    }
  //    new Derived2().print() shouldBe List("Derived2", "Trait2", "BaseClass")
  //  }
  //
  //  it should "derived3" in {
  //    // For derived3, it is a bit more complex, but still doable without a lot of calculation:
  //    //
  //    // L(Derived3) = Derived3, Trait3, Trait2, BaseClass
  //    class Derived3 extends Trait3 { // Trait3 extends Trait2 (which extends BaseClass)
  //      override def print(): List[String] = "Derived3" :: super.print()
  //    }
  //    new Derived3().print() shouldBe List("Derived3", "Trait3", "Trait2", "BaseClass")
  //  }
  //
  //  it should "derived4" in {
  //    // For Derived4 it is a bit more complex because it mixes in two traits:
  //
  //    // please note that the class linearization formula is the following:
  //
  //    // L(C) = C,L(Cn) +: .... +: L(C1)
  //
  //    // Also C1... Cn denotes the inherited classes/traits in order they are declared
  //    // for the class from left to right.
  //
  //    class Derived4 extends Trait1 with Trait2 {
  //      override def print(): List[String] = "Derived4" :: super.print()
  //    }
  //
  //    // let's calculate:
  //
  //    // C = Derived4
  //    // n = 2 (two super classes declared)
  //    // C1 = Trait1
  //    // C2 = Trait2
  //
  //    // The class linearization formula is:
  //    // L(Derived4) = Derived4 :+ L(Trait2) :+ L(Trait1)
  //
  //    // Note the order in which the formula sets the super classes, Cn (so the last one declared)
  //    // will be calculated first, then the second etc until the first one declared, a bit counter intuitive..
  //
  //    // Lets linearize Trait2:
  //    // L(Trait2) = Trait2 -> BaseClass
  //
  //    // Lets linearize Trait1:
  //    // L(Trait1) = Trait1 -> BaseClass
  //
  //    // Now, lets linearize all classes/traits:
  //    // L(Derived4)         = Derived4 +: L(Trait2) :+ L(Trait1)
  //    // replaced L(Trait2) => Derived4, Trait2, BaseClass
  //    // replaced L(Trait1) => Derived4, Trait2, BaseClass :+ Trait1, BaseClass
  //
  //    // Now to simplify, the following rule: when the right side has the same member as the left side
  //    // the one at the left side is removed, so the right side has BaseClass, and the left side also has
  //    // BaseClass, so BaseClass at the left side will be removed.
  //
  //    // The final linear form of Derived4 will become:
  //    // L(Derived4) = Derived4, Trait2, Trait1, BaseClass
  //    new Derived4().print() shouldBe List("Derived4", "Trait2", "Trait1", "BaseClass")
  //  }
  //
  //  // let's swap the super classes
  //
  //  it should "Derived5" in {
  //    class Derived5 extends Trait2 with Trait1 {
  //      override def print(): List[String] = "Derived5" :: super.print()
  //    }
  //
  //    // Lets calculate:
  //
  //    // C = Derived5
  //    // n = 2 (two super classes declared)
  //    // C1 = Trait2
  //    // C2 = Trait1
  //
  //    // The class linearization formula is:
  //    // L(Derived5) = Derived5 :+ L(Trait1) :+ L(Trait2)
  //
  //    // Linearize all classes/traits:
  //    // L(Derived5)         = Derived5 +: L(Trait1) :+ L(Trait2)
  //    // replaced L(Trait1) => Derived5, Trait1, BaseClass
  //    // replaced L(Trait2) => Derived5, Trait1, BaseClass :+ Trait2, BaseClass
  //
  //    // After simplification and removing the BaseClass left side member, the final linear
  //    // form of Derived5 will become:
  //    // L(Derived5) = Derived5, Trait1, Trait2, BaseClass
  //
  //    new Derived5().print() shouldBe List("Derived5", "Trait1", "Trait2", "BaseClass")
  //  }
  //
  //  it should "Derived6" in {
  //    // notition:
  //    // Derived6 extends BaseClass
  //    // Trait1 extends BaseClass
  //    // Trait2 extends BaseClass
  //    // Trait3 extends Trait2 (which extends BaseClass)
  //    class Derived6 extends BaseClass with Trait1 with Trait3 {
  //      override def print(): List[String] = "Derived6" :: super.print()
  //    }
  //
  //    // this one is a bit more complex. It is taken from the site
  //    // http://tech.pro/blog/2114/scala-linearization-technique-to-avoid-multiple-inheritance so I
  //    // would recommend reading the explanation there, but the thing is, both
  //
  //    // Lets calculate:
  //
  //    // C = Derived6
  //    // n = 3 (three super classes declared)
  //    // C1 = BaseClass
  //    // C2 = Trait1
  //    // C3 = Trait3
  //
  //    // The class linearization formula is:
  //    // L(Derived6) = Derived6 :+ L(Trait3) :+ L(Trait1) :+ L(BaseClass) (yes the website has it wrong)
  //
  //    // Calculate L(Trait3)
  //    // L(Trait3)          = Trait3 :+ L(Trait2)
  //    // replace L(Trait2) => Trait3, Trait2, BaseClass
  //
  //    // Linearize all classes/traits:
  //    // L(Derived6)        = Derived6 +: L(Trait3) :+ L(Trait1) :+ L(BaseClass)
  //    // replace L(Trait3)  => Derived6, Trait3, Trait2, BaseClass :+ L(Trait1) :+ L(BaseClass)
  //    // replace L(Trait1)  => Derived6, Trait3, Trait2, BaseClass :+ Trait1, BaseClass :+ BaseClass
  //    // removing BaseClass => Derived6, Trait3, Trait2, Trait1, BaseClass :+ BaseClass
  //    // removing BaseClass => Derived6, Trait3, Trait2, Trait1, BaseClass
  //
  //    // After simplification and removing members from the left side, that are already available at
  //    // the right side, the final form of Derived6 will become:
  //    // L(Derived6) = Derived6, Trait3, Trait2, Trait1, BaseClass
  //
  //    new Derived6().print() shouldBe List("Derived6", "Trait3", "Trait2", "Trait1", "BaseClass")
  //    // also note that Derived6 stated out with 'extends BaseClass', but it is called last; the lineair
  //    // form of the mixins is not as per definition; as scala does not support multiple inheritence and
  //    // calculates a single lineair form. The lineair form can change how we define the mixin sequence,
  //    // and so it is important to know how the calculation works.
  //  }
}
