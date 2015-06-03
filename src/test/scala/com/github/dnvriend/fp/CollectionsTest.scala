package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class CollectionsTest extends TestSpec {

  /* scala has the following collection hierarchy

                     Traversable
                         |
                         |
                      Iterable
                         |
      +------------------+--------------------+
     Map                Set                  Seq
      |                  |                    |
      |             +----+----+         +-----+------+
    Sorted Map  SortedSet   BitSet   IndexedSeq   LineairSeq
   */

  "IndexedSeq" should "have map, flatMap and Filter" in {
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

    xs.foldLeft(1)(_+_)
    xs.sum shouldBe 1 + 2 + 3 + 4 + 5

    val xx = for (x <- xs) yield x * 2
    xx shouldBe Vector(2, 4, 6, 8, 10)

    // the Vector can also be created like this
    val xy = for(x <- 1 to 5) yield x * 2
    xy shouldBe Vector(2, 4, 6, 8, 10)

    val xz = for(x <- 1 to 5 if x > 2) yield x
    xz shouldBe Vector(3, 4, 5)

    xs.foldLeft(1)(_*_) shouldBe 120
    xs.product shouldBe 120

    // reduceLeft is a special case of foldLeft, in which the type of the
    // resulting value is the same type or a supertype of the collection's type.
    // with foldLeft the result type doesn't even *have to be related to* the collection type,
    // it could be a String // see: http://stackoverflow.com/questions/7764197/difference-between-foldleft-and-reduceleft-in-scala
    xs.reduceLeft(_ + _) shouldBe 15

    Vector(1, 2) ++ Vector(3, 4) shouldBe Vector(1, 2, 3, 4)

    Vector(1, 2, 3) zip Vector(4, 5, 6) shouldBe Vector((1, 4), (2, 5), (3, 6))

    Vector(1, 2, 1, 3, 3, 4, 5, 5).groupBy(identity) should contain allOf (
      5 -> Vector(5, 5), 1 -> Vector(1, 1), 2 -> Vector(2), 3 -> Vector(3, 3), 4 -> Vector(4)
    )

    xs.mkString(", ") shouldBe "1, 2, 3, 4, 5"
    xs.partition(_ < 2) shouldBe (Vector(1), Vector(2, 3, 4, 5))
    xs.slice(2, 4) shouldBe Vector(3, 4)
    xs.zip(Vector(5, 6, 7)) shouldBe Vector((1, 5), (2, 6), (3, 7))
    Vector(5, 6, 7).zipWithIndex shouldBe Vector((5, 0), (6, 1), (7, 2))
  }

  "Arrays" should "have map, flatMap and Filter" in {
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

    val xz = for(x <- 1 to 5 if x > 2) yield x
    xz shouldBe Array(3, 4, 5)

    xs.foldLeft(1) { _ * _ } shouldBe 120
  }

  "List" should "have map, flatMap and Filter" in {
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

    val xz = for(x <- 1 to 5 if x > 2) yield x
    xz shouldBe List(3, 4, 5)

    xs.foldLeft(1) { _ * _ } shouldBe 120
  }
}
