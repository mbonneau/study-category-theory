# Algebraic Data Types
Taken from the fantastic book: [The Type Astronaut's Guide to Shapeless - Underscore](https://github.com/underscoreio/shapeless-guide)

Algebraic data types (ADTs) are a functional programming concept with a fancy name but a very simple meaning.
They are an idiomatic way of representing data using "ANDs" and "ORs".

For example:

- a shape is a rectangle __or__ a circle,
- a rectangle has a with __and__ a height,
- a circle a radius

In ADT terminology, "and" types such as rectangle and circle are called products and "or" types such as shape
are called coproducts. In Scala we typically represent products using case classes and coproducts using sealed traits:

```scala
scala> sealed trait Shape
defined trait Shape

scala> final case class Rectangle(width: Double, height: Double) extends Shape
defined class Rectangle

scala> final case class Circle(radius: Double) extends Shape
defined class Circle

scala> val rect: Shape = Rectangle(3.0, 4.0)
rect: Shape = Rectangle(3.0,4.0)

scala> val circ: Shape = Circle(1.0)
circ: Shape = Circle(1.0)
```

The beauty of ADTs is that they are completely type safe. The compiler has complete knowledge of the algebras we define,
so it can help us write complete, correctly typed methods involving our types:

```scala
scala> def area(shape: Shape): Double =
     |   shape match {
     |     case Rectangle(w, h) => w * h
     |     case Circle(r)       => math.Pi * r * r
     |   }
area: (shape: Shape)Double

scala> area(rect)
res0: Double = 12.0

scala> area(circ)
res1: Double = 3.141592653589793
```

## Alternative encodings
Sealed traits and case classes are undoubtedly the most convenient encoding of ADTs in Scala. However, they aren’t the
only encoding. For example, the Scala standard library provides generic products in the form of Tuples and a generic
coproduct in the form of Either. We could have chosen these to encode our Shape:

```scala
scala> type Rectangle2 = (Double, Double)
defined type alias Rectangle2

scala> type Circle2 = Double
defined type alias Circle2

scala> type Shape2 = Either[Rectangle2, Circle2]
defined type alias Shape2

scala> val rect2: Shape2 = Left((3.0, 4.0))
rect2: Shape2 = Left((3.0,4.0))

scala> val circ2: Shape2 = Right(1.0)
circ2: Shape2 = Right(1.0)
```

While this encoding is less readable than the case class encoding above, it does have some of the same desirable properties.
We can still write completely type safe operations involving Shape2:

```scala
scala> def area2(shape: Shape2): Double =
     |   shape match {
     |     case Left((w, h)) => w * h
     |     case Right(r)     => math.Pi * r * r
     |   }
area2: (shape: Shape2)Double

scala> area2(rect2)
res2: Double = 12.0

scala> area2(circ2)
res3: Double = 3.141592653589793
```

Importantly, Shape2 is a more generic encoding than Shape. Any code that operates on a pair of Doubles will be able to
operate on a Rectangle2 and vice versa. As Scala developers we tend to prefer semantic types like Rectangle and Circle
to generic ones like Rectangle2 and Circle2 precisely because of their _specialised_ nature. However, in some cases
generality is desirable. For example, if we’re serializing data to disk, we don’t care about the difference between a
pair of Doubles and a Rectangle2. We just write two numbers and we’re done.