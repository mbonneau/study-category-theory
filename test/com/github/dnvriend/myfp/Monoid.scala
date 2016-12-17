package com.github.dnvriend.myfp

import scala.util.{Success, Try}
import scala.collection.immutable._

object Monoid {

}

trait Monoid[A] {
  def zero: A
  def append(f1: A, f2: A): A
}


