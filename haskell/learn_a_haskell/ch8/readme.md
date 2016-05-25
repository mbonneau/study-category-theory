# Making Our Own Types and Typeclasses
See: [Learn you a Haskell - Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)

# Algebraic Data Types (ADT)
To make new types in Haskell we use the __data__ keyword:

```haskell
data Bool = False | True deriving Show
```

Which is must more concise than Scala's sealed trait/case class combo:

```scala
sealed trait Bool
case object False extends Bool
case object True extends Bool
```

## A 'Shape' ADT
Say for example we want to create types that represent a  _Shape_ for example a _Circle_ and _Rectangle_ type we can do the following:

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
```

In Haskell, when we construct a type we use a _value constructor_ which are functions that ultimately return a value of a data type. In Scala this is not the case. 

In Scala:

```scala
sealed trait Shape
final case class Circle(x: Double, y: Double, r: Double) extends Shape
final case class Rectangle(x1: Double, y1: Double, x2: Double, y2: Double) extends Shape
```

In Haskell, to create functions that work with the _Shape_ type we can pattern match on it:

```haskell
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```
 
In Scala we can just add the appropriate method to the type:

```scala
sealed trait Shape
final case class Circle(x: Double, y: Double, r: Double) extends Shape {
    def surface: Double = scala.math.Pi * scala.math.pow(r, 2)
}
final case class Rectangle(x1: Double, y1: Double, x2: Double, y2: Double) extends Shape {
    def surface: Double = scala.math.abs(x2 - x1) * scala.math.abs(y2 - y1)
}
```

## Record Syntax
Haskell supports using the [Record Syntax](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax): 

```haskell
data Car = Car { company :: String, model :: String, year :: Int } deriving Show
```

By using the record syntax, Haskell creates functions that lookup fields in the data type. In Scala, case classes are the way to define ADTs, and they always use the record syntax:

```scala
scala> final case class Car(company: String, model: String, year: Int)
coolCar: Car = Car(Ford,Mustang,1967)

scala> val coolCar = Car("Ford", "Mustang", 1967)
coolCar: Car = Car(Ford,Mustang,1967)

scala> coolCar.year
res1: Int = 1967
``` 

## Type Parameters
When we wish to create a type that takes a type parameter, eg, when we wish to create a type _Vector_ that can be used by multiple numeric types, it looks like this:

```haskell
data Vector a = Vector a a a deriving Show
``

The left side of the __=__, the `data Vector a` part is the _type constructor_. The right side of the __=__, the `Vector a a a deriving Show` part is the _value constructor_. 

