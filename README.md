# study-category-theory
Study Functional Programming and Category Theory with [Scala](http://www.scala-lang.org/).

# Disclaimer
Quoting the great [Brendan McAdams](https://twitter.com/rit) "I am by no means a Scala/Scalaz expert. I'm a beginner that has made a lot of progress." 
which also applies to me I guess. So don't learn from me, but maybe, get inspired and go look at Functional Programming and Scalaz. 

# Books to read
The following are books to read if you want to learn Scala and Functional Programming

 - [Scala in Depth - Joshua D. Suereth](https://www.manning.com/books/scala-in-depth) - You should know this, it's your Scala, so go buy and read this, now!
 - [Functional Programming in Scala - Paul Chiusano and Rúnar Bjarnason](https://www.manning.com/books/functional-programming-in-scala) - You should buy and read this one to learn Functional Programming,
 - [Functional Reactive Domain Modelling - Debasish Ghosh](https://www.manning.com/books/functional-and-reactive-domain-modeling) - Teaches you how to create domain models using Functional Programming, 
 - [My Scalaz stream notes](https://www.gitbook.com/@aappddeevv) - Free for download its a good introduction to Scalaz

# Definitions
Note: The following is in context of programming and not mathematically correct, and should 
- __Functional Programming__: The practice of composing programs using functions [Wikipedia](https://en.wikipedia.org/wiki/Functional_programming),
- __Category Theory__: the study of collections of concepts (types) and arrows (functions/morphisms) and the relationships between them [Wikipedia](https://en.wikipedia.org/wiki/Category_theory), 
- __Concept__: a concept is a type, like String, Int, and so on,
- __Arrow__: an arrow is a morphism between concepts (types), something that converts from one concept (type) to another. Usually a morphism, 
which is a function defined against two types, that converts one type to another type, 
- __Category__: a category is a grouping of concepts (types) and arrows (functions/morphisms)
- __Functor__: functors are transformations from one category (grouping of types) to another category (grouping of types), 
that can also transform and preserve morphisms. A functor would be something that convert cats into dogs.
- __Morphism__: A morphism is the changing of one value in a category (grouping of types) to another in the same category 
(grouping of types), thus a morphism is a function that converts from one type to another. For example, a morphism is 
something that can change a fat cat into a slim cat [Wikipedia](https://en.wikipedia.org/wiki/Morphism)

## System Design
It is very important to share the vocabulary that we will be using in further discussions. We will be using the vocabulary that
consists of `containers`, `components` and `classes/code`. 

 * A software system is a hierarchy of simple building blocks,
 * A software system is made up of one or more `containers` each of which contains one or more `components` which in turn are implemented by one or more `classes`,
 * A `container` can be a web applications, mobile apps, standalone applications, databases, file systems, Microservice, shell script, etc,
 * A `component` is a grouping or `related functionality` encapsulated behind a well defined `interface`,
 * A `component` will be implemented by one or more `classes` so a component contains one or more classes,
 * In Object Oriented Programming: a component is made up of `classes` and `interfaces`,
 * In Functional Programming: a component could be a `module` which is a logical grouping of related `functions`, `types`, etc,
 * In Scala, which is an OO/FP hybrid, we will be using `classes` and `interfaces` but also `modules` which groups related `functions`, `types`, etc. 
 We will separate the pure from the impure, so we will structure our code to have a pure functional kernel and an impure boundary. All the side effects will happen on the outside (the class) and all the 
 pure will happen in the inside (the module). Every class in Scala can have a companion object (the module), so the impure can happen in the class, and the pure in the module, which is just a structure. 
 * In JavaScript: A component could be a JavaScript `module`, which is made up of a number of `objects` and `functions`,
 * In a RDBMS: A component could be a logical grouping of functionality; based upon a number of tables, views, stored procedures, functions, triggers, etc,
 
## Why use Scalaz? 
That's a good question, let me get back on that.

 - Validation of input,
 - Aggregating results in a simple well structured way,
 - Message passing between modules using for comprehension,