# applicative builder
The applicative builder is the _pipe-at-pipe_-sign or _pinkey-pie_ which is `|@|` is very handy when working 
with multiple applicative types like multiple Option[T], multiple Future[T], multiple Disjunction[T] and so on:

The applicative builder has the following syntactic style:

```
(F1[A] |@| F2[B] |@| Fn[Cn])((A, B, Cn) => D): F[D]
```
Which means that the result will be the same type as the functor we started with, so eg. all Option types which will return 
an Option but with the function applied but only when all F1..Fn are eg. a Some.

```scala
scala> (1.some |@| "foo".some |@| false.some)((x: Int, y: String, z: Boolean) => s"$x-$y-$z")
res18: Option[String] = Some(1-foo-false)

scala> (1.some |@| none[String] |@| false.some)((x: Int, y: String, z: Boolean) => s"$x-$y-$z")
res19: Option[String] = None
```

There is also another style that uses the _apply_ or _applicative_ symbol which is `^`. The applicative syntactic style is:

```
^(f1, f2) {...}
``` 

```scala
scala> ^(1.some, 2.some)((x: Int, y: Int) => x + y)
res21: Option[Int] = Some(3)
```

You should use multiple `^^` if you have more parameters, up to 22, which makes the applicative builder syntax that uses `|@|` a lot 
easier when refactoring of expressing your intention:

```scala
scala> ^^(1.some, 2.some, 3.some)((x: Int, y: Int, z: Int) => x + y + z)
res22: Option[Int] = Some(6)
```

Also the applicative style (the `^` syntax) does not seem to work for functions or any type constructor with two parameters, so basically
we can always use the applicative builder.

It provides a way to use applicative behavior without having to lift the function into a functor, which can be ackward: 

```scala
scala> 1.some <*> ((_: Int) + 2).some
res17: Option[Int] = Some(3)
```