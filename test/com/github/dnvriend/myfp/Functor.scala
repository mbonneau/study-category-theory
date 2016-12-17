package com.github.dnvriend.myfp

/**
  * The Functor allows application of a pure function to a value in a context
  */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def optionInstance
}
