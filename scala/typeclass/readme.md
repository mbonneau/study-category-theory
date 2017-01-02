# Type classes
Type classes are a programming pattern borrowed from Haskell (the word “class” has nothing to do with classes in
object oriented programming). We encode them in Scala using traits and implicits. A type class is a parameterised
trait representing some sort of general functionality that we would like to apply to a wide range of types:

```scala
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}
```

We implement our type class with instances for each type we care about. If we want the instances to automatically
be in scope we can place them in the type class’ companion object. Otherwise we can place them in a separate library
object for the user to import manually.

The type class pattern is a ubiquitous pattern in Scala, its function is to provide a behavior for some type.

## Video
- [From Simulacrum to Typeclassic - Michael Pilquist](https://www.youtube.com/watch?v=Crc2RHWrcLI)

## Resolving instances
Type classes are very flexible but they require us to define instances for every type we care about. Fortunately,
the Scala compiler has a few tricks up its sleeve to resolve instances for us given sets of user-defined rules.

When all the parameters to an implicit def are themselves marked as implicit, the compiler can use it as a resolution
rule to create instances from other instances. Given a set of rules encoded as implicit vals and implicit defs,
the compiler is capable of searching for combinations to give it the required instances. This behaviour, known
as “implicit resolution”, is what makes the type class pattern so powerful in Scala.

Idioma c type class defini ons
The commonly accepted idioma c style for type class defini ons includes a companion object containing some standard methods:

```
import shapeless.Generic

case class UserWithAge(name: String, age: Int)
val gen = Generic[UserWithAge]
val u = UserWithAge("Julien", 30)

val h = gen.to(u) // returns Julien :: 30 :: HNil
gen.from(h) // return UserWithAge("Julien", 30)
```

