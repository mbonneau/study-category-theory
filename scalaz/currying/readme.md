# Function Currying
See: [Function Currying in Scala](http://www.codecommit.com/blog/scala/function-currying-in-scala)

What is Function Currying?
Function currying transforms a function of two (or more) parameters into a function of one parameter 
which returns a function of one parameter, and depending on how much parameters we have curried 
will keep returning another function that takes one parameter, until all parameters are known and a result
can be calculated and returned.  

To get started, lets look at a simple method, a member of a class that defines behavior, that does not use currying:

```scala
scala> def add (x: Int, y: Int): Int = x + y                                                                                         
add: (x: Int, y: Int)Int                                                                                                             
                                                                                                                                     
scala> add(1, 2)                                                                                                                     
res0: Int = 3 
```

Let's look at the same method that does currying:

```scala
scala> def add(x: Int): Int => Int = (y: Int) => x + y                                                                              
add: (x: Int)Int => Int                                                                                                              
                                                                                                                                     
scala> add(1)(2)                                                                                                                     
res1: Int = 3
```
In the first sample, the add method takes two parameters and returns the result of adding the two.  The second sample redefines 
the add method so that it takes only a single Int as a parameter and returns a Function as a result. The type of the method
is Int => Int.

Scala has syntax to define curried functions like so:

```scala

)
scala> def add(x: Int)(y: Int): Int = x + y                                                                                          
add: (x: Int)(y: Int)Int                                                                                                             
                                                                                                                                     
scala> add(1)(2)                                                                                                                     
res2: Int = 3 
``` 

It is also possible to curry existing functions, when working with an example code base:

```scala
scala> def add(x: Int, y: Int): Int = x + y                                                                                          
add: (x: Int, y: Int)Int

scala> add _                                                                                                                         
res12: (Int, Int) => Int = $$Lambda$1288/2006368475@2558386f                                                                         
                                                                                                                                     
scala> res12.curried                                                                                                                 
res13: Int => (Int => Int) = scala.Function2$$Lambda$1286/755940296@56d145db                                                         
                                                                                                                                     
scala> res13(1)(2)                                                                                                                   
res15: Int = 3      
``` 

The underscore after the 'add' method name is Scala syntax which tells the compiler to treat 'add' as a function value, 
rather than a method to be invoked. Calling '.curried' on the function will create a curried function.

We can also uncurry the function:

```scala
scala> Function.uncurried(res13)                                                                                                     
res21: (Int, Int) => Int = scala.Function$$$Lambda$1494/1345807683@6aa17b74
``` 

# Making a case for Currying 
The best rationale for using currying has to do with general and specialized functions.  It would be nice to define a 
function for the general case, but allow users to specialize the function and then used the specialized version on different
data sets.  Take the following code as an example: