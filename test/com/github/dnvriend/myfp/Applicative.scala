package com.github.dnvriend.myfp

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

/**
  * The Applicative type class defines the methods 'pure/point'
  * 'pure/point' takes a value and puts it in a minimal context
  * that still yields that value
  */
trait Applicative[F[_]] {
  def point[A](a: => A): F[A]
  def pure[A](a: => A): F[A] = point(a)
}

object Applicative {
  implicit def list = new Applicative[List] {
    override def point[A](a: => A): List[A] = List(a)
  }

  implicit def option = new Applicative[Option] {
    override def point[A](a: => A): Option[A] = Option(a)
  }

  implicit def seq = new Applicative[Seq] {
    override def point[A](a: => A): Seq[A] = Seq(a)
  }
}

class ApplicativeTest extends FlatSpec with Matchers {
  it should "put values in an Option context" in {
    Applicative.option.point(1) shouldBe Option(1)
  }

  it should "put values in a List context" in {
    Applicative.list.point(1) shouldBe List(1)
  }

  it should "put values in a Seq context" in {
    Applicative.seq.point(1) shouldBe Seq(1)
  }
}
