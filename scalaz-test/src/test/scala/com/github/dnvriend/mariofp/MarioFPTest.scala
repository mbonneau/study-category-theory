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

package com.github.dnvriend.mariofp

import com.github.dnvriend.TestSpec

import scalaz.Functor

class MarioFPTest extends TestSpec {
  // see: https://www.youtube.com/watch?v=3Ycp55QEbGM

  /**
   * As you may (or may not) know, Mario has some enemies. One of which
   * is a Goomba. Such a creature can be defeated by stomping him at which
   * Mario receives a coin which can be modelled like:
   */

  final case class Coin(points: Int)
  final case class Goomba()

  // stomp is a function from Goomba => Coin
  def stomp(g: Goomba): Coin = {
    g.hashCode()
    Coin(10)
  }

  def withGoomba(f: Goomba => Unit): Unit =
    f(Goomba())

  def withFullCage(f: Cage[Goomba] => Unit): Unit =
    f(FullCage(Goomba()))

  def withEmptyCage(f: Cage[Goomba] => Unit): Unit =
    f(EmptyCage[Goomba]())

  "Mario" should "receive a coin after stomping a Goomba" in withGoomba { goomba =>
    stomp(goomba) shouldBe Coin(10)
  }

  object Vacuum {
    def collect(g: Goomba): Coin = stomp(g)
  }

  "Luigi" should "receive a coin after vacuuming a Goomba " in withGoomba { goomba =>
    Vacuum.collect(goomba) shouldBe Coin(10)
  }

  it should "throw a NPE when the Goomba escapes (darn -> BOOM!)" in withGoomba { goomba =>
    val escapedGoomba = null
    intercept[NullPointerException] {
      Vacuum.collect(escapedGoomba)
    }
  }

  // NPE!! What to do...

  // I know!! Lets put the Goomba in a cage!

  sealed trait Cage[A]
  case class FullCage[A](value: A) extends Cage[A]
  case class EmptyCage[A]() extends Cage[A]
  object Cage {
    // factory method for Cages
    // a.k.a. "the way to lock the Goomba in the cage"
    def apply[A](a: A): Cage[A] =
      if (a == null) EmptyCage[A]() else FullCage[A](a)
  }

  // now we have a cage, how to stomp the Goomba..

  // Luigi knows how; he's building a better Vacuum!

  object BetterVacuum {
    def collect(cage: Cage[Goomba]): Cage[Coin] = cage match {
      case EmptyCage()      => EmptyCage[Coin]()
      case FullCage(goomba) => FullCage(stomp(goomba))
    }
  }

  it should "use the better vacuum with full cage; it should not BOOM-BOOM!" in withFullCage { fullCage =>
    BetterVacuum.collect(fullCage) shouldBe a[FullCage[Coin]]
  }

  it should "use the better vacuum with empty cage; it should not BOOM-BOOM!" in withEmptyCage { emptyCage =>
    BetterVacuum.collect(emptyCage) shouldBe EmptyCage[Coin]()
  }

  // now we've got stomping Goomba's beat, what about stomping the Functor theory a.k.a. can we generalize
  // "putting Goomba's in the cage and modeling the effect of an empty or full cage and the behavior that goes
  // with these two states"? Well, lets try it then!

  // lets start by refactoring our Vacuum to make it more general. Lets start by renaming the 'stomping-operation'
  // to 'function: A => B' and forgetting about stomping Goombas all together :)

  object VacuumRefactor {
    def collect[A, B](cage: Cage[A], f: A => B): Cage[B] = cage match {
      case EmptyCage() => EmptyCage[B]()
      case FullCage(a) => FullCage(f(a))
    }
  }

  // we have a Vacuum that can operate on whatever is in the cage for example:

  it should "put anything in the cage and operate on it eg. an Int" in {
    VacuumRefactor.collect(Cage(4), (_: Int) + 1) shouldBe FullCage(5)
  }

  // we should be able to generalize some more, eg. put the generic 'collect' operation in a trait:

  trait Collector {
    def collect[A, B](cage: Cage[A], f: A => B): Cage[B]
  }

  object VacuumCollector extends Collector {
    override def collect[A, B](cage: Cage[A], f: (A) => B): Cage[B] = cage match {
      case EmptyCage() => EmptyCage[B]()
      case FullCage(a) => FullCage(f(a))
    }
  }

  // can we generalize the trait some more? Yes we can, we can parameterize the trait with
  // a 'type-constructor' which looks like this: 'F[_]'. The type-constructor creates higher-kinded-types
  // which is the 'F' with a type which must be given by us (the developer). The type we can give the type
  // constructor is eg. Int, String, Goomba and so on.
  //
  // In the type-constructor 'F[_]', the underscore means "I don't care what the type is, as long as you give me
  // a type, I can construct the higher kinded type". The type of type-constructor must be a higher-kinded-type which
  // is something that has a context like 'List', 'Option', 'Future', 'Either', 'Cage' and so on. Its a type that can
  // contain some other type, like the 'Cage', which is a Cage[A]. To put it another way, "its something that takes
  // type parameters" which is eg. a List, Cage and so on.
  //

  trait CollectorWithTypeConstructor[F[_]] {
    def collect[A, B](c: F[A], f: A => B): F[B]
  }

  object VacuumCollectorWithTypeConstructor extends CollectorWithTypeConstructor[Cage] {
    override def collect[A, B](c: Cage[A], f: (A) => B): Cage[B] = c match {
      case EmptyCage() => EmptyCage[B]()
      case FullCage(a) => FullCage(f(a))
    }
  }

  it should "put anything in the Cage using a VacuumCollectorWithTypeConstructor" in {
    VacuumCollectorWithTypeConstructor.collect(Cage(4), (_: Int) + 1) shouldBe FullCage(5)
  }

  // what have be build until now? Basically a thing that can be 'mapped-over'. Has such a "thing" a name?
  // Good question and the answer is yes! Its called a 'Functor'.

  // How convenient! We have Scalaz on the classpath and that library has support for the Functor. Can we define
  // our Cage using Scalaz's Functor? Yes we can!

  implicit object CageFunctor extends Functor[Cage] {
    override def map[A, B](cage: Cage[A])(f: (A) => B): Cage[B] = cage match {
      case EmptyCage() => EmptyCage[B]()
      case FullCage(a) => FullCage(f(a))
    }
  }

  // lets see how this works! (but first lets define the CageFunctor in implicit scope to let
  // the scalaz.syntax.functor._ do its work

  it should "use the cage functor to stomp Goombas" in withFullCage { fullCage =>
    CageFunctor.map(fullCage)(stomp) shouldBe FullCage(Coin(10))
    import scalaz.syntax.functor._
    fullCage.map(stomp) shouldBe FullCage(Coin(10))
  }

  // Yup, the Functor sure is something that can be "mapped-over". The Functor implementation takes a higher-kinded-type
  // like the Cage[A] trait, which has two subtypes (so in total three classes). These three classes capture the effect
  // of Cage[A], which can be an EmptyCage[A] or a FullCage[A], which can be "mapped-over" to become either a
  // FullCage[B] or an EmptyCage[B].

  // Now we are all Functor experts, err.. well.. kinda...
  // There is more to Functors, there are err.. some laws that must apply (you know, the small print..)
  //
  // 1. Mapping preserves identity
  //    This means: if we map ID over a functor, the functor that we get back
  //    should be the same as the original functor
  //
  // 2. Mapping respects composition
  //    This means: Composing two functions first (eg. f: A => B and g: B => C, so f andThen g) then mapping should be
  //    the same as first mapping f over the functor, and then mapping g.
}
