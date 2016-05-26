# Scalaz
An extension to the core Scala library for functional programming

---

## Modularity
Scalaz has a modular architecture:

* scalaz-core: Type class hierarchy, data structures, type class instances for the Scala and Java standard libraries, implicit conversions / syntax to access these.
* scalaz-effect: Data structures to represent and compose IO effects in the type system.
* scalaz-concurrent: Actor and Future implementation
* scalaz-iteratee: Experimental new Iteratee implementation

---

## Imports
```scala
import scalaz._ // imports type names
import scalaz.Id.Id // imports Id type alias
import scalaz.std.option._ // imports instances, converters, and functions related to `Option`
import scalaz.std.AllInstances._ // imports instances and converters related to standard types
import scalaz.std.AllFunctions._ // imports functions related to standard types
import scalaz.syntax.monad._ // injects operators to Monad
import scalaz.syntax.all._ // injects operators to all typeclasses and Scalaz data types
import scalaz.syntax.std.boolean._ // injects operators to Boolean
import scalaz.syntax.std.all._ // injects operators to all standard types
import scalaz._, Scalaz._ // all the above
```

# Links
- [Scalaz cheat sheet](http://eed3si9n.com/learning-scalaz/scalaz-cheatsheet.html)

