package com.github.dnvriend.scalaz

import com.github.dnvriend.TestSpec

import scalaz._
import scalaz.std.list._
import scalaz.std.option._ // syntax for the Bind type class (and its parents)

class Quickstart extends TestSpec {
  "Functions and list" should "apply" in {
    Apply[Option].apply2(some(1), some(2))((a, b) => a + b) shouldBe Option(3)
    Traverse[List].traverse(List(1, 2, 3))(i => some(i)) shouldBe Option(List(1, 2, 3))
  }
}
