# Apply
see: https://github.com/scalaz/scalaz/blob/series/7.3.x/example/src/main/scala/scalaz/example/ApplyUsage.scala



## How to understand traverse, traverseU and traverseM
see: http://stackoverflow.com/questions/26602611/how-to-understand-traverse-traverseu-and-traversem

Sequence is used to gather together applicative effects. More concretely, it lets you "flip" `F[G[A]]` to `G[F[A]]`,
provided `G` is `Applicative` and `F` is `Traversable`.

So we can use it to "pull together" a bunch of Applicative effects (note all Monads are Applicative)

```scala
scala> List(Future.successful(1), Future.successful(2)).sequence : Future[List[Int]]
res1: scala.concurrent.Future[List[Int]] = Future(<not completed>)
```

The operation `traverse` is equivalent to `map-then-sequence`, so you can use it when you have a function that
returns an Applicative and you want to just get a single instance of your Applicative rather than a list of them.

```scala
scala> def fetchPost(post: Int): Future[String] = Future.successful(post.toString)
scala> List(1, 2).traverse(fetchPost)
```

The operation `traverseU` is the same operation as `traverse`, just with the types expressed differently
so that the compiler can infer them more easily.

The operation `traverseM(f)` is equivalent to `traverse(f).map(_.join)`, where `join` is the scalaz name for `flatten`.
It's useful as a kind of "lifting flatMap":


http://labs.bench.co/blog/2014/11/10/scalaz-for-dummies