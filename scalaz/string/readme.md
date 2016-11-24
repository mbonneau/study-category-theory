# StringOps

```scala
scala> import scalaz._
import scalaz._

scala> import std.string._
import std.string._

scala> plural("foo", 1)
res1: String = foo

scala> plural("foo", 2)
res2: String = foos

scala> charsNel("")
res3: Option[scalaz.NonEmptyList[Char]] = None

scala> charsNel("foo")
res4: Option[scalaz.NonEmptyList[Char]] = Some(NonEmpty[f,o,o])

scala> parseBoolean("true")
res5: scalaz.Validation[IllegalArgumentException,Boolean] = Success(true)

scala> "true".parseBoolean
res6: scalaz.Validation[IllegalArgumentException,Boolean] = Success(true)

scala> "1".parseLong
res7: scalaz.Validation[NumberFormatException,Long] = Success(1)

scala> parseLong("1")
res8: scalaz.Validation[NumberFormatException,Long] = Success(1)
```