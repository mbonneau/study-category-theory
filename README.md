# study-category-theory
Study Functional Programming and Category Theory with [Scala](http://www.scala-lang.org/).

# Type Systems
- [Type Theory](http://en.wikipedia.org/wiki/Type_theory)
- [Type System](http://en.wikipedia.org/wiki/Type_system)
- [Structural Type System](http://en.wikipedia.org/wiki/Structural_type_system)
- [Covariance](http://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))
- [Scala lang reference](http://www.scala-lang.org/docu/files/ScalaReference.pdf)
- [Value classes](http://docs.scala-lang.org/sips/completed/value-classes.html)
- [Reactive Programming](https://medium.com/reactive-programming/what-is-reactive-programming-bc9fa7f4a7fc)
- [Scala collection](http://docs.scala-lang.org/overviews/collections/performance-characteristics.html)
- [Concrete Immutable Collection Classes](http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#vectors)
- [Heiko - Seq is not immutable](http://hseeberger.github.io/blog/2013/10/25/attention-seq-is-not-immutable/)
- [F-Bound Polymorphism](http://work.tinou.com/2009/07/wtf-is-fbounded-polymorphism.html)

// notes
Seq.apply creates a list, they have poor chars (append, reverse, all O(n)); use it to traverse it once, head/tail
Vector has good chars
- Do use Map and Set apply methods
//

# Category Theory
## Slides
- [Category Theory for Beginners](http://www.slideshare.net/kenbot/category-theory-for-beginners)
- [Your data structures are made of math](http://www.slideshare.net/kenbot/your-data-structures-are-made-of-maths)

## Video
- [Scaladays - SF - Improving Correctness with Types](https://www.parleys.com/tutorial/improving-correctness-with-types)
- [ScalaDays - SF - Type level programming 101](https://www.parleys.com/tutorial/type-level-programming-scala-101)
- [Scala Typeclassopedia with John Kodumal](https://www.youtube.com/watch?v=IMGCDph1fNY)
- [Dan Rosen - Scala Monads: Declutter Your Code With Monadic Design](https://www.youtube.com/watch?v=Mw_Jnn_Y5iA)
- [Youtube - Category Theory Foundations, Lecture 1](https://www.youtube.com/watch?v=ZKmodCApZwk)
- [Youtube - Category Theory Foundations, Lecture 2](https://www.youtube.com/watch?v=TQYjekxqw-Q)
- [Youtube - Category Theory Foundations, Lecture 3](https://www.youtube.com/watch?v=BOynNljjbeg)
- [Youtube - Category Theory Foundations, Lecture 4](https://www.youtube.com/watch?v=8fZmdhLLgs4)

## Links
- [Functor](http://www.encyclopediaofmath.org/index.php/Functor)

## Monads
- [Youtube - Brian Beckman: Don't fear the Monad](https://www.youtube.com/watch?v=ZhuHCtR3xq8)
- [Youtube - Brian Beckman: The Zen of Stateless State - The State Monad](https://www.youtube.com/watch?v=XxzzJiXHOJs)
- [Youtube - Expert to Expert: Brian Beckman and Erik Meijer - Inside the .NET Reactive Framework (Rx)](https://www.youtube.com/watch?v=looJcaeboBY)
- [Youtube - Monads and Gonads](https://www.youtube.com/watch?v=b0EF0VTs9Dc)

# Functional Programming
> Functional programming is a programming paradigm - a style of building the structure and elements of computer programs - that treats computation as the evaluation of mathematical functions and avoids changing state and mutable data. It is a declarative programming paradigm, which means programming is done with expressions. In functional code, the output value of a function depends only on the arguments that are input to the function, so calling a function f twice with the same value for an argument x will produce the same result f(x) each time. Eliminating side effects, i.e. changes in state that do not depend on the function inputs, can make it much easier to understand and predict the behavior of a program, which is one of the key motivations for the development of functional programming.
-- <quote>[Wikipedia](http://en.wikipedia.org/wiki/Functional_programming)</quote>

> Functional programming is a restriction on how we write programs, but not on what programs we can write - we can still use side-effects. (ie. write to a file, printing to the console, reading user input, drawing on a screen, alter state). As it turns out that accepting this restriction is tremendously beneficial because of the increase in that we gain `modularity` from programming with pure functions. Because of their modularity, pure functions are easier to test, to reuse, to parallelize, to generalize, and to reason about. 
-- <quote>[Functional Programming in Scala](http://www.manning.com/bjarnason/)</quote>

# Video
- [YouTube - Introduction to Type-Level Programming](https://www.youtube.com/watch?v=WZOzxAP8NpI)
- [YouTube - Type-Level Programming: The Subspace of Scala](https://www.youtube.com/watch?v=MjzBPIvgB24)
- [YouTube - PNWS 2014 - What every (Scala) programmer should know about category theory](https://www.youtube.com/watch?v=W67LYX_1J_M)
- [YouTube - Miles Sabin - Shapeless: Exploring Generic Programming in Scala](https://www.youtube.com/watch?v=GDbNxL8bqkY)
- [YouTube - Jared Roesch - Demystifying Shapeless: An Exploration of Dependent Types in Scala](https://www.youtube.com/watch?v=VF-ISUiXIY0)
- [YouTube - Scalaz - The good parts](https://www.youtube.com/watch?v=jPdHQZnF56A)

# Blogs
- [CakeSolutions - Category theory patterns in Scala](http://www.cakesolutions.net/teamblogs/category-theory-patterns-in-scala)

# Slides
- [Category theory for beginners](http://www.slideshare.net/kenbot/category-theory-for-beginners)

# Scala
> Scala is an object-functional programming language for general software applications. Scala has full support for functional programming and a very strong static type system. This allows programs written in Scala to be very concise and thus smaller in size than other general-purpose programming languages. Many of Scala's design decisions were inspired by criticism of the shortcomings of [Java]().
-- <quote>[Wikipedia](http://en.wikipedia.org/wiki/Scala_(programming_language))</quote>

# Scalaz
> Scalaz is a Scala library for functional programming. It provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. Functor, Monad) and corresponding instances for a large number of data structures.
-- <quote>[Scalaz](https://github.com/scalaz/scalaz)</quote>

## Links
- [Learning Scalaz](http://eed3si9n.com/learning-scalaz/) -  [PDF](http://eed3si9n.com/learning-scalaz/learning-scalaz.pdf)

## Video
- [Parleys - Scala Days San Fransisco 2015 - A Skeptic's Look at scalaz' "Gateway Drugs"](https://www.parleys.com/tutorial/a-skeptics-look-scalaz-gateway-drugs) - [Brendan McAdams Twitter](https://twitter.com/rit) - [Brendan McAdams Github](https://github.com/bwmcadams)
- [Vimeo - Scalaz Presentation - Nick Partridge](https://vimeo.com/10482466)
- [Youtube - Learning Scalaz](https://www.youtube.com/watch?v=jyMIvcUxOJ0)
- [Youtube - Scalaz - The good parts](https://www.youtube.com/watch?v=jPdHQZnF56A)
- [Youtube - Scalaz - The State Monad](https://www.youtube.com/watch?v=Jg3Uv_YWJqI)
- [Youtube - Scalaz for the rest of us at Yelp](https://www.youtube.com/watch?v=kcfIH3GYXMI)
- [Youtube - Scalaz for the rest of us](https://www.youtube.com/watch?v=J3Mrwp_BzrI)
- [Youtube - Explorations in Variance](https://www.youtube.com/watch?v=VZWLRepyNvo)


# Shapeless
> Shapeless is a type class and dependent type based generic programming library for Scala.
-- <quote>[Shapeless](https://github.com/milessabin/shapeless)</quote>

## HList
> HList provides many operations to create and manipulate heterogenous lists (HLists) whose length and element types are known at compile-time.

## Youtube 
- [Youtube - Shapeless: Exploring Generic Programming in Scala](https://www.youtube.com/watch?v=GDbNxL8bqkY)

## Blogs
- [Cool Monday: HList and Shapeless](http://www.edofic.com/posts/2012-10-29-hlist-shapeless.html)

# Scala
## Blogs
- [Daniel Westheide - The Neophyte(Beginner)'s Guide to Scala](http://danielwestheide.com/scala/neophytes.html)

## Video
- [Scala Variance](https://www.youtube.com/watch?v=pgCD10nu_30)
- [Dick Wall - Effective Scala (3hrs)](https://www.parleys.com/tutorial/effective-scala-2)

## Class Linearization
> To support inheritance Scala has introduced a concept called [trait](http://www.artima.com/pins1ed/traits.html) almost similar to Java's [interface](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html). But unlike Java interfaces - which is not true anymore because of [default methods](https://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html) - Scala traits can actually define any concrete methods. From this it seems apparently that Scala supports [multiple inheritance](http://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem); but that is not the case. To avoid multiple inheritance Scala uses a technique called [linearization](http://www.artima.com/pins1ed/traits.html#12.6) to flatten the calls to super classes. -- <quote>[Tech Pro](http://tech.pro/blog/2114/scala-linearization-technique-to-avoid-multiple-inheritance)</quote>
 
## Value Classes
> Value classes are a new mechanism in Scala to avoid allocating runtime objects. This is accomplished through the definition of new AnyVal subclasses.
-- <quote>[Scaladoc](http://docs.scala-lang.org/overviews/core/value-classes.html)</quote>

- [Boldradius - Value classes in Scala](http://boldradius.com/blog-post/VUEhpCgAACcrIm1S/value-classes-in-scala)
 
### Sources
- [Scala Language Spec 5.1.2 - Class Linearization](http://www.scala-lang.org/files/archive/spec/2.11/05-classes-and-objects.html#class-linearization)
- [Programming in Scala - Traits](http://www.artima.com/pins1ed/traits.html)
- [Tech Pro - Dipta Pratim Banerjee - Scala: Linearization technique to avoid multiple inheritance](http://tech.pro/blog/2114/scala-linearization-technique-to-avoid-multiple-inheritance)
- [Jim McBeath - Class Linearization](http://jim-mcbeath.blogspot.nl/2009/08/scala-class-linearization.html)

# Akka
- [Akka patterns](http://www.slideshare.net/romantimushev/akka-patterns)
- [Jonas Boner Presentations](http://www.slideshare.net/jboner?utm_campaign=profiletracking&utm_medium=sssite&utm_source=ssslideview)
- [Jonas Boner - Akka: Simpler Scalability, Fault-Tolerance, Concurrency & Remoting through Actors](http://www.slideshare.net/jboner/akka-simpler-scalability-faulttolerance-concurrency-remoting-through-actors)
- [Patrik Norwal - Akka persistence webinar](http://www.slideshare.net/patriknw/akka-persistence-webinar)
- [Konrad Malawski - Akka persistence == event sourcing in 30 minutes](http://www.slideshare.net/ktoso/akka-persistence-event-sourcing-in-30-minutes)

## Distributed
- [Distributed Systems Are a UX Problem](http://bravenewgeek.com/distributed-systems-are-a-ux-problem/)
- [There is no Now](https://queue.acm.org/detail.cfm?id=2745385)
- [Paper Trail - A Brief Tour of FLP Impossibility](http://the-paper-trail.org/blog/a-brief-tour-of-flp-impossibility/)
- [The FLP paper - Impossibility of Distributed Consensuswith One Faulty Process](http://cs-www.cs.yale.edu/homes/arvind/cs425/doc/fischer.pdf)
- [Google Research - Spanner](http://research.google.com/archive/spanner.html)
<<<<<<< HEAD

## Heiko Seeberge's blog
- [Attention: Seq is not immutable](http://hseeberger.github.io/blog/2013/10/25/attention-seq-is-not-immutable/)

## Scala Collections
- [Scala Collection Performance Chars](http://docs.scala-lang.org/overviews/collections/performance-characteristics.html)
- [Scala Immutable Collections](http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#vectors)

=======
>>>>>>> e9cbd68aa4c13597acfb1f4761b7fd5ca3680dd9
