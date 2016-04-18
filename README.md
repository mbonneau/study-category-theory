# study-category-theory
This resource is for my study on Functional Programming and Category Theory. I will be using Haskell and Scala/Scalaz.

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
- __Object__: an alternative name for a concept,
- __Type__: an alternative name for a concept,
- __Arrow__: an arrow is a morphism (function) between concepts (types), something that converts from one concept (type) to another. Usually a morphism, which is a function defined against two types, that converts one type to another type, 
- __Composability__: Arrows are composable, this is the most important part of arrows,
- __Category__: a category is a bunch of objects/grouping of objects (concepts/types) and a bunch of arrows (functions/morphisms) connecting these objects and an object is abstract. Objects don't have any structure. The only way you can think about objects is how objects connect to other objects by means of arrows. Its like a graph, but it might have infinitely many objects and infinately many arrows between any two objects. It can also be two dots with one arrow, but it can also be no dots and no arrows - [Bartosz](https://youtu.be/dgrucfgv2Tw?t=20m20s)
- Nodes: objects can have a name, 
- __Functor__: functors are transformations from one category (grouping of types) to another category (grouping of types), 
that can also transform and preserve morphisms. A functor would be something that convert cats into dogs.
- __Morphism__: A morphism is the changing of one value in a category (grouping of types) to another in the same category (grouping of types), thus a morphism is a function that converts from one type to another. For example, a morphism is something that can change a fat cat into a slim cat [Wikipedia](https://en.wikipedia.org/wiki/Morphism)
- __Isomorphism__: [Isomorphism](http://mathworld.wolfram.com/Isomorphism.html) is a very general concept that appears in several areas of mathematics. The word derives from the Greek **iso**, meaning **"equal,"** and **morphosis**, meaning **"to form"** or **"to shape."**.  An isomorphism is a map that preserves sets and relations among elements,
- __Homomorphism__: similarity of form,
- __Proposal__: something proposed/an assumption,
- __Proposition__: A statement that affirms or denies something and is either true or false,
- __Axiom__: An axiom is a proposition regarded as self-evidently true without proof,
- __Associative__: (a math operation) yielding an equivalent result independent of the grouping of the terms,

# Types
- [Lambda Days 2016 - Truth about Types by Bartosz Milewski](https://www.youtube.com/watch?v=dgrucfgv2Tw)

> We write our programs in a way is composable, split a huge problem in small problems, solve the small pieces seperately and compose the solutions to these problems into one bigger program. - [Bartosz](https://youtu.be/dgrucfgv2Tw?t=5m40s)

> In category theory, an object in a category corresponds to a type corresponds to a proposition. - [Bartosz](https://youtu.be/dgrucfgv2Tw?t=18m55s)

> There are different kinds of category, a Set is one kind of category in which 'objects' are sets, and arrows are functions that go from one set to another set. - [Bartosz](https://youtu.be/dgrucfgv2Tw?t=27m28s)

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
 
# Haskell
[GHC](https://www.haskell.org/ghc/) (Glorious Glasgow Haskell Compilation System), is a state-of-the-art, open source, 
compiler and interactive environment (ghci) for the functional language [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)).

## Installing
Using brew:

```bash
brew install ghc cabal-install
brew link ghc
brew link --overwrite ghc
cabal update
cabal install ghc-mod
```
 
Downloading, installing and compiling/building `ghc-mod` can take some time and heat up your CPU a little. 
 
Launch the interactive console:
 
```bash
ghci
```

For a very cool tutorial (buy the book): [Learn you a Haskell for great good!](http://learnyouahaskell.com/). You can also
[read the whole book online for free](http://learnyouahaskell.com/chapters).
 
## Cabal 
[Cabal](https://www.haskell.org/cabal/) (Common Architecture for Building Applications and Libraries) is a system for building and packaging 
Haskell libraries and programs. It defines a common interface for package authors and distributors to easily build their applications in a 
portable way. Cabal is part of a larger infrastructure for distributing, organizing, and cataloging Haskell libraries and programs. 

* [Cabal Documentation](https://downloads.haskell.org/~ghc/7.0.3/docs/html/Cabal/index.html)

## ghc-mod and ghc-modi
The [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/ghc-mod.html) command and [ghc-modi](http://www.mew.org/~kazu/proj/ghc-mod/en/ghc-modi.html) command 
are backend commands to enrich Haskell programming on editors including Emacs, Vim, and Sublime. 
 
# GHCi
[GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-introduction) is GHC's interactive environment, in which Haskell expressions can be interactively evaluated and programs can be interpreted. Let's configure it. Create the file `~/.ghci` and put the following in it:

```bash
:set prompt "ghci> "
:set +t
```
 
Now launch the GHC interactive console: 
 
```bash
ghci
```

GHCi supports commands. GHCi commands all begin with ‘:’ and consist of a single command name followed by zero or more parameters. For an overview please read the [GHCi users guide - Commands](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci-commands.html).

Let's [learn us a Haskell](http://learnyouahaskell.com/).

## Haskell video

- For the book [Learn you a Haskell for Great Good](http://learnyouahaskell.com/chapters), [Peter Drake has created a video series that give additional information about the subjects](https://www.youtube.com/watch?v=NBKnY7Z_w3I&list=PLS6urCrsYES24Fwzg5-Uga1QEbNm9kiU_). 
- [Lambda Days 2015 - A Year of Haskell by Justin Leitgeb](https://www.youtube.com/watch?v=wZ0RQG3mFPw)
- [LambdaConf 2015 - Modeling Data in Haskell for Beginners Chris Allen](https://www.youtube.com/watch?v=p-NBJm0kIYU)
- [Linux.conf.au 2016 - Haskell is Not For Production and Other Tales by Katie Miller](https://www.youtube.com/watch?v=mlTO510zO78)
- [Strange Loop - "Writing a game in Haskell" by Elise Huard](https://www.youtube.com/watch?v=1MNTerD8IuI)
- [FunctionalConf 2014 - Haskell for Everyday Programmers by Venkat Subramaniam](https://www.youtube.com/watch?v=VGCE_3fjzU4)
## Must see videos

- [Haskell 3: Types and Type Classes by Peter Drake](https://www.youtube.com/watch?v=x3uF7fcQwWE) 

## Haskell books
The following are **free** resources online to learn Haskell:

- [Learn You a Haskell for Great Good! by Miran Lipovača](http://learnyouahaskell.com/chapters)
- [Real World Haskell by Bryan O'Sullivan, Don Stewart, and John Goerzen](http://book.realworldhaskell.org/read/)

The following are **non free** books:

- [($12,-) Happy Learn Haskell Tutorial Volume 1 by Andreas Lattka](https://leanpub.com/happylearnhaskelltutorialvol1)
- [($59,-) Haskell Programming from First Principles by Christopher Allen and Julie Moronuki](https://gumroad.com/l/haskellbook)
- [($12,-) Game programming in Haskell by Elise Huard](https://leanpub.com/gameinhaskell)

If you are an absolute beginner like me, pick up [Happy Learn Haskell Tutorial](https://leanpub.com/happylearnhaskelltutorialvol1) and [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters),
just to support the authors. Both not too expensive and in my honest opinion, great material!

# Embedded Haskell
It is also possible to use Haskell on the [Arduino](http://arduino.cc/) a popular open-source single-board microcontroller, with an Atmel AVR processor and on-board input/output support. For more information see the 
[Arduino section on HaskellWiki](https://wiki.haskell.org/Arduino).

# Frege - A Haskell for the JVM
[Frege](https://github.com/Frege/frege) is a Haskell for the JVM. The online [Frege REPL](http://try.frege-lang.org/). [Frege](https://github.com/Frege) consists of a number of projects for example, the [compiler/runtime](https://github.com/Frege/frege), the [Frege REPL](https://github.com/Frege/frege-repl) and a lot more projects and tools.

## Installation
For now we are only interested in the [Frege REPL](https://github.com/Frege/frege-repl). Download the latest version in `zip` format from the [releases](https://github.com/Frege/frege-repl/releases) page and unzip it and launch `$FREGE_HOME/bin/frege-repl`. 

## Video
The following are Frege video's:

- [Frege, a Haskell for the JVM by Dierk König](https://www.youtube.com/watch?v=1P1-HXNfFPc)
- [JavaOne 2015 - Frege: Purely Functional Programming for the JVM](https://www.youtube.com/watch?v=Svz-_ujrSX4)
- [Lambda Days 2016 - Getting Started with Frege by Lech Glowiak](https://www.youtube.com/watch?v=YxUCZ4DDNmk)

# Why use Scalaz? 
That's a good question, let me get back on that, first let me [learn a Haskell](http://learnyouahaskell.com/chapters) and at the same
time let me [learn Scalaz](http://eed3si9n.com/learning-scalaz/). 

 - Validation of input,
 - Disjunctions (useful in for-comprehensions),
 - Message passing between modules using for comprehension,
 - Monad transformers :)
 
