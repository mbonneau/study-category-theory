# Products and Coproducts
From [my-scala stream notes](https://aappddeevv.gitbooks.io/test_private_book/content/foundations/coproducts.html)

# Products
Scala uses the concept of a Product already as a base class for many objects. If you think about a scala tuple, its a subclass of a Product.

```scala
scala> (1,2)
(1,2)
res41: (Int, Int) = (1,2)
```

This is pretty important. If you take the product of two types, such as creating a tuple of two Ints, 
you get back an object that includes both of those types and the ability to capture values of both types
at once in the same data structure. 

That's what a tuple can for you. In scala's case for a tuple, it just sticks the two values together as a 
TupleN where N represents the arity. A Product is the type of an object, like a Tuple, that allows you to 
keep values from each type together.

Sometimes, you need to store only one object from multiple types of objects. For example, if you need an 
object to store a String, Int or a Float, then a Coproduct of these types could store a single value 
from any one those types. 

That's what Either does in the scala standard library and what disjunction (\/) does in the scalaz library. 
How the object stores the values of different types really depends on how you have programmed your language or type.

A Coproduct is different than a Product because it can only store a value with one of the types at any given time. 

Either is the Coproduct of two types that you specify. For example, you may have the need for an object that could 
be one of several different types: a log message, a value, or a function that reads from the console. You can think of a 
Coproduct as a disjoint union of the types. 

Obviously, there are a number of issues that could come up when defining operations that operate on a Coproduct with types 
that do not mesh well together. If the types in your coproduct do not support + that may limit the expressiveness of your program. 

You should think of a Coproduct as a heterogenous container. The scala libary shapeless has a nice Coproduct class that allows you 
to put together more than two types. With scalaz, you need to build that up using a Coproduct created from a disjunction, 
so it only handles two types at a time. 

A Coproduct requires a way to "inject" values into the Coproduct. In standard scala, Left and Right inject a value into Either.

```
scala> val c = Coproduct[List, Option, String](\/-(Some("blah")))
c: scalaz.Coproduct[List,Option,String] = Coproduct(\/-(Some(blah)))

scala> c.map(_+"!")
res47: scalaz.Coproduct[List,Option,String] = Coproduct(\/-(Some(blah!)))
```

Which really just demonstrates scalaz's disjunction. A Coproduct allows out to avoid having scala cast a structure to Any. You already 
know that if you create a List with disjoint types, scala automatically promotes the type to Any:

```
scala> List("blah", 1.0, 2)
res49: List[Any] = List(blah, 1.0, 2)
```

A Coproduct allows you to preserve type information throughout your operations. Anytime you see Coproduct, think of Either with more than two 
possible types. According to Runar's presentation mentioned above, you can build multiple types into a Coproduct two types at a time. 
Check out his presentation for an example of that.

