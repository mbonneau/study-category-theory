# Foldable
see: http://eed3si9n.com/learning-scalaz/7.0/Foldable.html

Because there are so many data structures that work nicely with folds, the Foldable type class was introduced. 
Much like Functor is for things that can be mapped over, Foldable is for things that can be folded up!

# Scala List
```scala
scala> val xs = List(1, 2, 3)
ys: List[Int] = List(1, 2, 3)

// when we have an available Monoid for the parameterized type (List), 
// we can collapse the Foldable using the monoid:
scala> Foldable[List].fold(xs)
res1: Int = 6

scala> Foldable[List].foldLeft(xs, Monoid[Int].zero)(_ |+| _)
res2: Int = 6

// We can import syntax for foldable, 
// allowing us to "enhance" the foldable (List) 
// with the new methods:
scala> import scalaz.syntax.foldable._
import scalaz.syntax.foldable._


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
