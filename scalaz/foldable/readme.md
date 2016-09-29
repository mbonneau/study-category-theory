# Foldable
see: http://eed3si9n.com/learning-scalaz/7.0/Foldable.html

Because there are so many data structures that work nicely with folds, the Foldable type class was introduced. 
Much like Functor is for things that can be mapped over, Foldable is for things that can be folded up!

# Scala List
```scala
scala> val ys = List(1, 2, 3)
ys: List[Int] = List(1, 2, 3)

scala> val intMonoid = implicitly[Monoid[Int]]
intMonoid: scalaz.Monoid[Int] = scalaz.std.AnyValInstances$$anon$5@129a66ef

scala> Foldable[List].foldLeft(ys, intMonoid.zero)(_ |+| _)
res4: Int = 6

scala> ys.foldLeft(intMonoid.zero)(_ |+| _)
res5: Int = 6

scala> Foldable[List].fold(ys)
res8: Int = 6

scala> Foldable[List].fold(List.empty[Int])
res10: Int = 0

scala> Foldable[List].foldLeft(List.empty[Int], intMonoid.zero)(_ |+| _)
res13: Int = 0

// scalaz supports getting instances with the following syntax
scala> Monoid[Int]
res15: scalaz.Monoid[Int] = scalaz.std.AnyValInstances$$anon$5@129a66ef

scala> Foldable[List].foldLeft(List.empty[Int], Monoid[Int].zero)(_ |+| _)
res16: Int = 0
```

# NonEmptyList
```scala
scala> val xs = NonEmptyList(1, 2, 3)
xs: scalaz.NonEmptyList[Int] = NonEmpty[1,2,3]

scala> Foldable1[NonEmptyList].foldLeft1(xs)(_ |+|_)
res0: Int = 6

scala> import scalaz.syntax.foldable1._
import scalaz.syntax.foldable1._

scala> xs.foldLeft1(_ |+| _)
res1: Int = 6
```
