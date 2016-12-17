# typeclasses
A small study project on type classes

## Introduction
(from: )

Type classes are a programming pa ern borrowed from Haskell (the word “class” has nothing to do with classes in object oriented programming). 
We encode them in Scala using traits and implicits. 

A type class is a parameterised trait, with a single parameter `A`, representing some sort of general functionality 
that we would like to apply to a wide range of types. Using [simulacrum](https://github.com/mpilquist/simulacrum) creating
typeclasses is a breeze:

```scala
import simulacrum._

@typeclass trait CsvEncoder[A] {
  def encode(value: A): List[String]
}
```

We implement our type class with instances for each type we care about. If we want the instances to automatically 
be in scope we can place them in the type class’ companion object. Otherwise we can place them in the companion object
of type `A` if possible or put them in a separate library object for the user to import manually. Here we show
the typeclass being put in the companion object.

```scala
object CsvEncoder {
    implicit val employeeEncoder: CsvEncoder[Employee] =
    new CsvEncoder[Employee] {
        def encode(e: Employee): List[String] =
        List(
            e.name,
            e.number.toString,
            if(e.manager) "yes" else "no") 
    }
}
```

We mark each instance with the keyword implicit, and define one or more entry point methods that accept an implicit parameter 
of the corresponding type.

Next we can create a generic method that can write List[A] types to CSV using the CsvEncoder that knows how to turn an A into a CSV.
When we call writeCsv, the compiler calculates the value of the type parameter `A` and searches for an 
implicit CsvWriter of the corresponding type.

```scala
def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")
```