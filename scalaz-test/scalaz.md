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

---

## Uber import
```scala
// imports *all* implicit conversions that provide *syntax* 
// as well as type class instances and other functions
// aka  the uber import, to be backwards compatible with Scalaz 6

import scalaz.Scalaz._  
```

# Links
- [Scalaz cheat sheet](http://eed3si9n.com/learning-scalaz/scalaz-cheatsheet.html)
- [David R. MacIver - Existential types in Scala](http://www.drmaciver.com/2008/03/existential-types-in-scala/)
- [TypeLevel - How can we map a Set?](http://typelevel.org/blog/2014/06/22/mapping-sets.html)
- [James Iry - Scala Option will save you from null](http://james-iry.blogspot.nl/2010/08/why-scalas-and-haskells-types-will-save.html)

# Scalaz none[A]
None has type None.type. scalaz provides none[A] as a convenience method for None as an Option[A].

```scala
import scalaz._, Scalaz._
scala> None
res0: None.type = None

scala> None: Option[Int]
res1: Option[Int] = None

scala> Option.empty[Int]
res2: Option[Int] = None

scala> none[Int]
res3: Option[Int] = None
```

# scalaz some
Some(1) returns a Some[Int]. 1.some returns an Option[Int], which is probably what you actually want.

```scala
scala> Option(1)
res20: Option[Int] = Some(1)

scala> Some(1)
res21: Some[Int] = Some(1)

scala> 1.some
res22: Option[Int] = Some(1)
```

# scalaz type class apply method
scalaz type classes provide apply methods in their companion objects that are shortcuts to grab the implicit instance

```scala
scala> implicitly[Equal[Int]].equal(1, 3)
res23: Boolean = false

scala> Equal[Int].equal(1, 3)
res24: Boolean = false

scala> implicitly[Functor[IList]].map(IList(1, 2, 3))(_ + 1)
res25: scalaz.IList[Int] = [2,3,4]
              ^
scala> Functor[IList].map(IList(1, 2, 3))(_ + 1)
res26: scalaz.IList[Int] = [2,3,4]
```

# Monoid#instance gives you a way of concisely creating a Monoid instance:

```scala
scala> case class Foo(bar: Int)
defined class Foo                                                                                     ^

scala> implicit val fooMonoid: Monoid[Foo] = Monoid.instance( { (x, y) => Foo(x.bar + y.bar) }, Foo(0))
fooMonoid: scalaz.Monoid[Foo] = scalaz.Monoid$$anon$6@12ef69cf

scala> (1 to 5).map(Foo.apply).toList.suml
res27: Foo = Foo(15)
```

# Scalaz StringOps
String's toInt, toDouble etc throw exceptions. Scalaz provides nifty String parse methods that return validations

Hoi! weer een klein stukje Scalaz! Stel dat we een tekst willen parsen en we doen dat met Scala, dan kunnen we de methoden .toInt, .toLong, toDouble, toFloat, toBoolean gebruiken. Dit zijn standaard methoden die je terug kunt vinden op het String object, dus nog niks Scalaz.. 

scala> "1".toLong                                                                                                                                                                                                                                                                
res27: Long = 1

Voor het happy pad is dit prima, we hebben een "1" omgezet naar een Long value, prachtig, maar wat nu als we het volgende doen:

scala> "".toLong                                                                                                                                                                                                                                                                 
java.lang.NumberFormatException: For input string: ""                                                                                                                                                                                                                            

Ah darn, een exception, wat zoveel wil zeggen dat ons programma nu stopt. What to do?? I know! Scalaz!

Scalaz geeft ons een aantal methoden die we kunnen gebruiken op het String object om een String te parsen (so to speak) waaronder: parseBoolean, parseByte, parseShort, parseInt, parseLong, parseFloat, parseDouble, parseBigInt en parseBigDecimal. Poe, een boel methoden. Laten we eens kijken wat ze doen!

scala> "1".parseLong                                                                                                                                                                                                                                                             
res29: scalaz.Validation[NumberFormatException,Long] = Success(1)

Het happy pad! Goed zeg, we hebben een resultaat, maar wat nu? Onze return type is geen Long, maar een rare scalaz.Validation. Laten we even snel naar de fout situatie kijken dan komen we hier op terug (I promise).

scala> "".parseLong                                                                                                                                                                                                                                                              
res30: scalaz.Validation[NumberFormatException,Long] = Failure(java.lang.NumberFormatException: For input string: "")

Oke een fout situatie, we kunnen de String niet parsen en we krijgen een Failure met daarin een fout. In de goed situatie krijgen we een Success. Misschien is het opgevallen, maar de Failure en Success is niet van het type scala.util.Failure of scala.util.Success maar is een subtype van scalaz.Validation. Een detail maar wel goed om te weten.

Omdat we met Scalaz werken zal het je niet verbazen dat we met hogere functies bezig zijn, met andere woorden we zijn (heavy) lifters :) Wat kunnen we met zo'n validation failure of validation success doen? 

Wat we kunnen doen met zo'n validation is de waarde uit de validatie halen met bijvoorbeeld een default waarde stel het volgende:

```
scala> "".parseLong.getOrElse(0)                                                                                                                                                                                                                                                 
res38: AnyVal = 0                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                 
scala> "1".parseLong.getOrElse(0)                                                                                                                                                                                                                                                
res39: AnyVal = 1
```

Met andere woorden, als de validatie faalt, dan hebben we een default waarde van 0, en als de validatie lukt, dan hebben we de waarde.

Stel dat je programma met Scala's Option type werkt (en dat is prima btw), dan kun je een Validation omzetten in een Option:

```
scala> "".parseLong.toOption                                                                                                                                                                                                                                                     
res40: Option[Long] = None                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                 
scala> "1".parseLong.toOption                                                                                                                                                                                                                                                    
res41: Option[Long] = Some(1)
```

Wat kunnen we nog meer met een Validation? De validation snapt de welbekende 'map' operatie, dus we kunnen het volgende doen:

````
scala> "1".parseLong.map(_ + 1)                                                                                                                                                                                                                                                  
res45: scalaz.Validation[NumberFormatException,Long] = Success(2)                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                 
scala> "".parseLong.map(_ + 1)                                                                                                                                                                                                                                                   
res46: scalaz.Validation[NumberFormatException,Long] = Failure(java.lang.NumberFormatException: For input string: "")
````

Dus de map zal worden uitgevoerd als het parsen is gelukt, en anders niet. Nice! 

Als we gebruik maken van control structures in onze code, dan zullen we ook willen weten of het parsen is gelukt of niet, ook dat kunnen we vragen aan Validation:

```
scala> "1".parseLong.isSuccess                                                                                                                                                                                                                                                   
res58: Boolean = true                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                 
scala> "1".parseLong.isFailure                                                                                                                                                                                                                                                   
res59: Boolean = false
```

We kunnen ook side effects uitvoeren op het success resultaat van het parsen bijvoorbeeld:

```
scala> "1".parseLong.foreach(println)                                                                                                                                                                                                                                            
1                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                 
scala> "".parseLong.foreach(println)
```

Validation heeft een linker en een rechterkant, de Failure (links) en de Success (rechts). We kunnen expliciet aangeven dat we een map operatie willen uitvoeren
aan de linker of rechter kan van het resultaat door middel van leftMap en rightMap. 

```
scala> "1".parseLong.rightMap(_ + 1)                                                                                                                                                                                                                                             
res70: scalaz.Validation[NumberFormatException,Long] = Success(2)                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                 
scala> "foo".parseLong.leftMap { t: Throwable => "Error while parsing to Long: " + t.getMessage }                                                                                                                                                                                
res71: scalaz.Validation[String,Long] = Failure(Error while parsing to Long: For input string: "foo")
```

Met de map operatie kunnen we de type veranderen van bijvoorbeeld de linker kant. De Throwable kunnen we veranderen in een String wat handig is als we een duidelijke fout melding willen loggen of willen communiceren aan de client.