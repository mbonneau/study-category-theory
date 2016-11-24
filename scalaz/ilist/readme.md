# IList (Invariant List)
An IList is a safe, invariant alternative to Scala's standard `List`. Most methods on `List` have a sensible equivalent
here, either on the `IList` interface itself or via typeclass instances (which are the same as
those defined for stdlib `List`). All methods are total and stack-safe.

An IList is invariant by type but covariant in nature.  

```scala
scala> val xs: List[Some[Int]] = List(Some(1), Some(2), Some(3))
xs: List[Some[Int]] = List(Some(1), Some(2), Some(3))

// a standard Scala List can be ascribed as a List[Option[Int]], which makes it Covariant (+A), because the 
// List is parameterized as List[Some[Int]], and it can be ascribed as a List[Option[Int]]. Some[Int] <: Option[Int]
// so the values can be put in a List of type Option[Int], but only because the standard list is Covariant (+A)
scala> xs: List[Option[Int]]
res0: List[Option[Int]] = List(Some(1), Some(2), Some(3))

// an IList is Invariant, so only A
scala> val ys: IList[Some[Int]] = IList.fromList(xs)
ys: scalaz.IList[Some[Int]] = [Some(1),Some(2),Some(3)]

// when we try to ascribe the values of ys, which are all Some[Int], to an IList of Option[Int], this cannot
// be done because the IList is invariant in type A. 
scala> ys: IList[Option[Int]]
<console>:32: error: type mismatch;
 found   : scalaz.IList[Some[Int]]
 required: scalaz.IList[Option[Int]]
Note: Some[Int] <: Option[Int], but class IList is invariant in type A.
You may wish to define A as +A instead. (SLS 4.5)
       ys: IList[Option[Int]]
       ^

// we can however widen the type of IList to Option[Int]...
scala> ys.widen[Option[Int]]
res3: scalaz.IList[Option[Int]] = [Some(1),Some(2),Some(3)]
```