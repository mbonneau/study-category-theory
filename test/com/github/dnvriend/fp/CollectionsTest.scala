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

import scalaz._
import Scalaz._

class CollectionsTest extends TestSpec {

  /* scala has the following collection hierarchy
   see: http://stackoverflow.com/questions/1722137/scala-2-8-collections-design-tutorial

                     Traversable
                         |
                         |
                      Iterable
                         |
      +------------------+---------------------------+
     Map                Set                         Seq
      |                  |                           |
      |             +----+----+         +-----------------------+
    Sorted Map  SortedSet   BitSet   IndexedSeq              LineairSeq
                               (Vector, Range, String)  (List, Stream, Queue, Stack)
   */

  "IndexedSeq (Vector)" should "have map, flatMap and Filter" in {
    // IndexedSeq collections are optimized for contant-time or near constant-time access
    // and length computations. It provides random access and updates in constant time, as
    // well as very fast append and prepend.
    val xs: Vector[Int] = Vector(1, 2, 3, 4, 5)
    xs.head shouldBe 1
    xs.headOption shouldBe Some(1)
    xs.tail shouldBe Vector(2, 3, 4, 5)
    xs.last shouldBe 5
    xs.count(_ == 2) == 1
    xs.diff(Vector(2, 3, 4)) shouldBe Vector(1, 5)
    xs.exists(_ > 3) shouldBe true
    xs.intersect(Vector(2, 3)) shouldBe Vector(2, 3)
    xs.union(Vector(2, 3)) shouldBe Vector(1, 2, 3, 4, 5, 2, 3)
    xs.union(Vector(2, 3)).distinct shouldBe Vector(1, 2, 3, 4, 5)
    xs.drop(1) shouldBe Vector(2, 3, 4, 5)
    xs.drop(1).dropRight(1) shouldBe Vector(2, 3, 4)
    xs.dropWhile(_ <= 2) shouldBe Vector(3, 4, 5)
    xs.filter(_ % 2 == 0) shouldBe Vector(2, 4)
    xs.filterNot(_ % 2 == 0) shouldBe Vector(1, 3, 5)
    xs.map(_ * 2).find(_ == 5) shouldBe None
    xs.map(_ * 2).find(_ == 10) shouldBe Some(10)

    // do *all* the elements conform to the predicate?
    xs.forall(_ < 10) shouldBe true

    xs.foldLeft(1)(_ + _)
    xs.sum shouldBe 1 + 2 + 3 + 4 + 5

    val xx = for (x <- xs) yield x * 2
    xx shouldBe Vector(2, 4, 6, 8, 10)

    // the Vector can also be created like this
    val xy = for (x <- 1 to 5) yield x * 2
    xy shouldBe Vector(2, 4, 6, 8, 10)

    val xz = for (x <- 1 to 5 if x > 2) yield x
    xz shouldBe Vector(3, 4, 5)

    xs.foldLeft(1)(_ * _) shouldBe 120
    xs.product shouldBe 120

    // reduceLeft is a special case of foldLeft, in which the type of the
    // resulting value is the same type or a supertype of the collection's type.
    // with foldLeft the result type doesn't even *have to be related to* the collection type,
    // it could be a String // see: http://stackoverflow.com/questions/7764197/difference-between-foldleft-and-reduceleft-in-scala
    xs.reduceLeft(_ + _) shouldBe 15

    Vector(1, 2) ++ Vector(3, 4) shouldBe Vector(1, 2, 3, 4)

    Vector(1, 2, 3) zip Vector(4, 5, 6) shouldBe Vector((1, 4), (2, 5), (3, 6))

    //    Vector(1, 2, 1, 3, 3, 4, 5, 5).groupBy(identity) should contain allOf (
    //      5 → Vector(5, 5), 1 → Vector(1, 1), 2 → Vector(2), 3 → Vector(3, 3), 4 → Vector(4)
    //    )

    xs.mkString(", ") shouldBe "1, 2, 3, 4, 5"
    xs.partition(_ < 2) shouldBe (Vector(1), Vector(2, 3, 4, 5))
    xs.slice(2, 4) shouldBe Vector(3, 4)
    xs.zip(Vector(5, 6, 7)) shouldBe Vector((1, 5), (2, 6), (3, 7))
    Vector(5, 6, 7).zipWithIndex shouldBe Vector((5, 0), (6, 1), (7, 2))
  }

  "Lineair Seq (List)" should "have map, flatMap and Filter" in {
    // Lists are optimized for Sequential Scan (it is a LineairSeq)
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    xs.head shouldBe 1
    xs.last shouldBe 5
    xs.drop(1) shouldBe List(2, 3, 4, 5)
    xs.drop(1).dropRight(1) shouldBe List(2, 3, 4)
    xs.filter(_ % 2 == 0) shouldBe List(2, 4)
    xs.filterNot(_ % 2 == 0) shouldBe List(1, 3, 5)
    xs.map(_ * 2).find(_ == 5) shouldBe None
    xs.map(_ * 2).find(_ == 10) shouldBe Some(10)

    xs.sum shouldBe 1 + 2 + 3 + 4 + 5

    val xx = for (x <- xs) yield x * 2
    xx shouldBe List(2, 4, 6, 8, 10)

    val xz = for (x <- 1 to 5 if x > 2) yield x
    xz shouldBe List(3, 4, 5)

    xs.foldLeft(1) { _ * _ } shouldBe 120
  }

  "Arrays (mutable)" should "have map, flatMap and Filter" in {
    // an Array is a *mutable* indexed collection of values, and is 100%
    // compatible with Java's Array, the T[]
    val xs: Array[Int] = Array(1, 2, 3, 4, 5)
    xs.head shouldBe 1
    xs.last shouldBe 5
    xs.drop(1) shouldBe Array(2, 3, 4, 5)
    xs.drop(1).dropRight(1) shouldBe Array(2, 3, 4)
    xs.filter(_ % 2 == 0) shouldBe Array(2, 4)
    xs.filterNot(_ % 2 == 0) shouldBe Array(1, 3, 5)
    xs.map(_ * 2).find(_ == 5) shouldBe None
    xs.map(_ * 2).find(_ == 10) shouldBe Some(10)

    xs.sum shouldBe 1 + 2 + 3 + 4 + 5

    val xx = for (x <- xs) yield x * 2
    xx shouldBe Array(2, 4, 6, 8, 10)

    val xz = for (x <- 1 to 5 if x > 2) yield x
    xz shouldBe Array(3, 4, 5)

    xs.foldLeft(1) { _ * _ } shouldBe 120
  }

  // manual map, flatMap and filter

  def filter[A](seq: Seq[A])(p: A => Boolean): Seq[A] = {
    def helper(src: Seq[A], xs: Seq[A]): Seq[A] = src match {
      case Nil          => xs
      case head +: tail => if (p(head)) helper(tail, xs :+ head) else helper(tail, xs)
    }
    helper(seq, Nil)
  }

  def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = {
    def helper(src: Seq[A], xs: Seq[B]): Seq[B] = src match {
      case Nil          => xs
      case head +: tail => helper(tail, xs :+ f(head))
    }
    helper(seq, Nil)
  }

  def flatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = {
    def helper(src: Seq[A], xs: Seq[B]): Seq[B] = src match {
      case Nil          => xs
      case head +: tail => helper(tail, xs ++ f(head))
    }
    helper(seq, Nil)
  }

  "manual map" should "evaluate correctly" in {
    map(Seq(1, 2, 3)) { _.toDouble } shouldBe Seq(1, 2, 3).map(_.toDouble)
  }

  "manual flatMap" should "evaluate correctly" in {
    flatMap(Seq(1, 2, 3)) { _ => Seq(1, 2, 3) } shouldBe Seq(1, 2, 3).flatMap(_ => Seq(1, 2, 3))
  }

  "manual filter" should "evaluate correctly" in {
    filter(Seq(1, 2, 3)) { _ % 2 == 0 } shouldBe Seq(1, 2, 3).filter(_ % 2 == 0)
  }

  /**
   * see: http://www.47deg.com/blog/adventures-with-scala-collections
   *
   * Seq, List or Vector, that is the question
   *
   * In most projects, we have to deal with sequences of objects (User from the database, Shop Products, etc.).
   * However, sometimes as developers, we don’t think about which one is a better fit for the topology of our algorithms,
   * so we don’t see this question arise before development begins. We’d like to change that by putting the various options
   * on the table to motivate people to discuss which option is the best course early on.
   *
   * Let’s figure out whether Seq, List or Vector better match our needs, based on these four main points:
   *
   * - Mutability
   * - Performance
   * - Usability (Pattern matching)
   * - Law Abiding
   */

  /**
   * Seq: A base trait for sequences. There isn’t a lot that needs to be said so, let’s discuss our key points.
   *
   * Seq - Mutability:
   * The first thing to realize is that the default version you’ll be using if you don’t import anything special
   * will be the Seq trait located on scala.collections.Seq. In fact, if you look at the code within the scala package
   * object, we can find:
   *
   * type Seq[+A] = scala.collection.Seq[A]
   * val Seq = scala.collection.Seq
   *
   * Keeping this in mind, let’s put together an example where we’re designing our API and have a method that is
   * receiving a Seq parameter:
   *
   * def proccessSequence(coll: Seq[Int]) = ???
   *
   * As you can imagine, our proccessSequence function can receive both Seq versions, the immutable one,
   * scala.collection.immutable.Seq and also the mutable one, scala.collection.mutable.Seq.
   *
   * Therefore, if we assume we’re defining an API that’s trying to preserve the immutability principle,
   * we’d be incorrect because we’ll need to add the import scala.collection.immutable.Seq along all the Scala
   * files to guarantee this immutability principle. I know, this looks like a boilerplate. That’s why we often
   * assume that Seq is mutable by default.
   *
   * In my humble opinion, that’s not a good thing. Even I would suggest skipping this section and going on to the next one.
   * I’m kidding! This is an imperative feature, and you need to be aware of the default behavior. The responsibility lies
   * on our shoulders as developers, and the import should be completed if we want to use the immutable version.
   *
   * The reason for this behavior is explained in detail in the Effective Scala Docs:
   *
   * "The default Traversable, Iterable and Seq types in scope – defined in scala.package – are the scala.collection versions,
   * as opposed to Map and Set – defined in Predef.scala – which are the scala.collection.immutable versions. This means that,
   * for example, the default Seq type can be both the immutable and mutable implementations. Thus, if your method relies on a
   * collection parameter being immutable, and you are using Traversable, Iterable or Seq, you must specifically require/import
   * the immutable variant, otherwise someone may pass you the mutable version."
   *
   * Seq - Performance:
   * In general, we can say that Seq is inefficient by nature because it is considered as an infinite collection,
   * and, added with O(n) time complexity, most operations won’t terminate. In other words, the use of Seq is not
   * safe in every case because it’s not lazy like Stream is for example.
   *
   * Also, it’s important to mention that, Seq has a few limitations which make it an inadequate collection
   * for parallel programming.
   *
   * Seq - Usability:
   * Seq is usable in pattern matching.
   *
   * Seq - Law Abiding
   * If we’re thinking in purely functional programming terms, it’s important to mention that Seq doesn’t obey every
   * algebraic law. We might assume that the Seq is mutable, and in that case, it’s impossible to define lawful typeclass
   * instances for mutable structures because reference identity matters. Scala functional libraries such as Scalaz and Cats,
   * don’t have any instances for the Seq collections since they have so many inference problems, and algebraic laws are unmet.
   *
   * In addition, I would like to add one of Rob Morris’ statements from this discussion, which fits in perfectly here:
   *
   * Even if you stick with collection.immutable you find that implementations are very unconstrained; some may obey a given
   * typeclass’s laws and others may not. For instance you could define Functor[immutable.Iterable], but it wouldn’t be lawful
   * for Set or Map. I think this is a general problem with defining instances for non-final data types. Now, you might want
   * to argue that any sensible immutable.Iterable/Seq will end up being an okay Foldable since this typeclass has very weak
   * constraints.
   *
   * Seq - Conclusions
   * As I’m sure you’ve figured out, Seq is mutable by default and due to the inefficient and significant features,
   * it’s enough to lose trust in Seq.
   */

  /**
   * List
   * A List is a class used for linked lists representing ordered collections of elements of a specific type.
   * List is an abstract class (recall Seq is a trait), subtype of LinearSeq, as you can see in the previous images.
   *
   * List - Mutability
   * Scala List is immutable by default; that’s the main difference in respect to Scala Seq. If we, once again, look at
   * the code within the scala package object, we can see that the default definition is the immutable version:
   *
   * type List[+A] = scala.collection.immutable.List[A]
   * val List = scala.collection.immutable.List
   *
   * For those of you coming from Java, ArrayList is not the equivalent implementation of Scala List,
   * though it might be easy to think that. As we’ve pointed out previously, Java doesn’t have indistinguishable
   * immutable collections. Therefore, the better match for ArrayList would be the scala.collection.mutable.ArrayBuffer.
   * With that in mind, the Java LinkedList can be considered as the equivalent (and mutable version) of the Scala List collection.
   *
   * List - Law Abiding
   * In both, Cats and Scalaz, we can find instances for the List collection. Both instances are lawful and meet the laws
   * for Traverse, Monoidal, Monad and CoflatMap type classes.
   *
   * List - Conclusions
   * Use List if all you need are fast prepends and the List will be used properly (see the pitfalls section).
   */

  /**
   * Vector
   * Vector is a general-purpose, immutable data structure. It provides random access and updates in effectively constant time,
   * as well as very fast append and prepend. Because vectors strike a good balance between fast random selections and fast random
   * functional updates, they are currently the default implementation of immutable indexed sequences. It is backed by a
   * little endian bit-mapped vector trie with a branching factor of 32. Locality is very good, but not contiguous, which
   * is good for very large sequences.
   *
   * Vector - Law Abiding
   * As with List in the previous section, Vector collection has instances for both Cats and Scalaz libraries. Therefore,
   * both instances in their functional libraries are lawful and meet the laws for Traverse, Monoidal, Monad and CoflatMap type classes.
   *
   * Vector - Conclusions
   * It’s probably obvious that all of these arguments are pointing to a definite recommendation of breaking the habit
   * of using List, and starting to use the Vector collection, as it’s a better choice in most cases.
   */

  /**
   * Alternatives
   * Scanning the Scala Standard Library, we can find a few alternatives that can be helpful depending on our needs.
   * Here is a reference from the Scalaz library:
   * - IList: safe, invariant alternative to stdlib List. Most methods on List have a sensible equivalent here,
   *          either on the IList interface itself or via typeclass instances (which are the same as those defined for stdlib List).
   *          All methods are total and stack-safe.
   * - DList: difference lists, a data structure for O(1) append on lists. Based on Data.DList, a Haskell library by Don Stewart.
   *          A difference list is a function that given a list, returns the original contents of the difference list prepended
   *          at the given list. This structure supports O(1) append and snoc operations on lists, making it very useful for
   *          append-heavy uses, such as logging and pretty printing.
   * - NonEmptyList: A singly-linked list that is guaranteed to be non-empty.
   * - ListT: ListT monad transformer, useful because Monads do not compose.
   */

  "NonEmptyList (immutable)" should "have map, flatMap and Filter" in {
    val nel: NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
    nel.head shouldBe 1
    nel.last shouldBe 5
    val xs: Vector[Int] = nel.toVector
    val ys: List[Int] = nel.toList
  }

  /**
   * Conclusion
   * To safeguard your code, you should make your variables and collection immutable, preventing uncontrolled side effects.
   *
   * All things considered, utilizing Vector is the best choice unless you have specific needs where another solution is apparent.
   * Business logic dictates that you should generally, return the most specific immutable types you can (List or Vector),
   * this would be the same rule to consider for typeclasses. In addition to this, avoid both Seq and Iterable because it’s assumed
   * that they’re both mutable (avoid all the interfaces if you will).
   */
}
