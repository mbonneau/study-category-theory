# study-category-theory
This resource is for my study on Functional Programming and Category Theory. I will be using Haskell and Scala/Scalaz.

# Disclaimer
Quoting the great [Brendan McAdams](https://twitter.com/rit) "I am by no means a Haskell/Scala/Scalaz expert. I'm a beginner that has made a lot of progress." which also applies to me I guess. So don't learn from me, but maybe, get inspired and go look at Functional Programming, Haskell, Scala and Scalaz. 

# Books to read
The following are books to read if you want to learn Scala and Functional Programming

 - [Scala in Depth - Joshua D. Suereth](https://www.manning.com/books/scala-in-depth) - You should know this, it's your Scala, so go buy and read this, now!
 - [Functional Programming in Scala - Paul Chiusano and Rúnar Bjarnason](https://www.manning.com/books/functional-programming-in-scala) - You should buy and read this one to learn Functional Programming,
 - [Functional Reactive Domain Modelling - Debasish Ghosh](https://www.manning.com/books/functional-and-reactive-domain-modeling) - Teaches you how to create domain models using Functional Programming, 
 - [My Scalaz stream notes](https://www.gitbook.com/@aappddeevv) - Free for download its a good introduction to Scalaz

# Category Theory Video

- [Category Theory, The essence of interface-based design by Erik Meijer](https://www.youtube.com/watch?v=JMP6gI5mLHc)
- [Channel 9 - Don't fear the Monad by Brian Beckman (2012)](https://www.youtube.com/watch?v=ZhuHCtR3xq8)
- [Channel 9 - Functional Programming by Erik Meijer (2012)](https://www.youtube.com/watch?v=z0N1aZ6SnBk)
- [Functional Programming from First Principles by Erik Meijer](https://www.youtube.com/watch?v=a-RAltgH8tw)
- [Category Theory by Tom LaGatta](https://www.youtube.com/watch?v=o6L6XeNdd_k)
- [Category Theory Lulz by Ken Scambler](https://www.youtube.com/watch?v=jDhMDgU7Koc)
- [Monads and Gonads by Douglas Crockford](https://www.youtube.com/watch?v=b0EF0VTs9Dc)
- [London Haskell Group - Why Do Monads Matter? by Derek Wright](https://www.youtube.com/watch?v=3q8xYFDYLeI)
- [London Haskell Group - The Algebra of Algebraic Data Types by Chris Taylor](https://www.youtube.com/watch?v=YScIPA8RbVE)

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
[GHC](https://www.haskell.org/ghc/) (Glorious Glasgow Haskell Compilation System), is a state-of-the-art, open source, compiler and interactive environment (ghci) for the functional language [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)).

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
[Cabal](https://www.haskell.org/cabal/) (Common Architecture for Building Applications and Libraries) is a system for building and packaging Haskell libraries and programs. It defines a common interface for package authors and distributors to easily build their applications in a portable way. Cabal is part of a larger infrastructure for distributing, organizing, and cataloging Haskell libraries and programs. 

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

- [Channel 9 - C9 Lectures - Functional Programming Fundamentals by Eric Meijer](https://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals) [on youtube](https://www.youtube.com/watch?v=UIUlFQH4Cvo&list=PLoJC20gNfC2gpI7Dl6fg8uj1a-wfnWTH8)
- [Lambda Days 2015 - A Year of Haskell by Justin Leitgeb](https://www.youtube.com/watch?v=wZ0RQG3mFPw)
- [LambdaConf 2015 - Modeling Data in Haskell for Beginners Chris Allen](https://www.youtube.com/watch?v=p-NBJm0kIYU)
- [Linux.conf.au 2016 - Haskell is Not For Production and Other Tales by Katie Miller](https://www.youtube.com/watch?v=mlTO510zO78)
- [Strange Loop - "Writing a game in Haskell" by Elise Huard](https://www.youtube.com/watch?v=1MNTerD8IuI)
- [FunctionalConf 2014 - Haskell for Everyday Programmers by Venkat Subramaniam](https://www.youtube.com/watch?v=VGCE_3fjzU4)
- [The Road to Running Haskell at Facebook Scale - Jon Coens](https://www.youtube.com/watch?v=sl2zo7tzrO8)
- [HaskellCast](https://www.youtube.com/channel/UC0pv4sIiJ404ubqUJ2e4WzA)
- ["Coder Decoder: Functional Programmer Lingo Explained, with Pictures" by Katie Miller](https://www.youtube.com/watch?v=uwrCQmpZ8Ts)

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

# Online documentation
- [A tour of the Haskell prelude](http://teaching.csse.uwa.edu.au/units/CITS3211/lectureNotes/tourofprelude.html#init)
- [C2 - Haskell Language](http://c2.com/cgi/wiki?HaskellLanguage)
- [WikiBooks - Haskell](https://en.wikibooks.org/wiki/Haskell)

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

# F-sharp
[F-sharp](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)) can be run [cross platform](http://fsharp.org/guides/mac-linux-cross-platform/). F# comes preinstalled with the mono brew formula:

```fsharp
$ brew install mono
$ fsharpi
F# Interactive for F# 4.0 (Open Source Edition)
Freely distributed under the Apache 2.0 Open Source License

For help type #help;;

>
```

## Introduction
[F-sharp](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)) is a strongly typed, multi-paradigm programming language that encompasses functional, imperative, and object-oriented programming techniques. F# is most often used as a cross-platform CLI language, but can also be used to generate JavaScript and GPU code.

F# is developed by the F# Software Foundation, Microsoft and open contributors. An open source, cross-platform compiler for F# is available from the F# Software Foundation. F# is also a fully supported language in Visual Studio and [Xamarin Studio](https://www.xamarin.com/studio). Other tools supporting F# development include [Mono](http://fsharp.org/use/mac/), MonoDevelop, SharpDevelop, MBrace and WebSharper.

F# originated as .NET implementation of a core of the [OCaml](https://en.wikipedia.org/wiki/OCaml) programming language, and has also been influenced by C#, Python, Haskell, Scala and Erlang.

## Using the F-sharp REPL
The REPL is called 'F# Interactive' and can be run by executing `fsharpi`.  It accepts F# code, compiles and executes it, then prints the results. This allows you to quickly and easily experiment with F# without needing to create new projects or build a full application to test the results of a code snippet. 

The REPL accepts F# code until you terminate the input with `;;` and a newline (enter). If the REPL evaluates an expression that was not assigned to a value, it will instead assign it to the name `it`, just like Haskell does. 

For example:

```fsharp
$ fsharpi
F# Interactive for F# 4.0 (Open Source Edition)
Freely distributed under the Apache 2.0 Open Source License

For help type #help;;

> let x = 42;;

val x : int = 42

> x * 2;;
val it : int = 84
>
```
The REPL will process input and displays the name, type, and value of the identifiers. For example, in the example above the value x was introduced with type int and value 42.

You can also type multiple expressions, just terminate the input with `;;` and a newline (enter).

```fsharp
> let x = 1
- let y = 2.3
- float x + y
- let cube x = x * x * x;;

val x : int = 1
val y : float = 2.3
val cube : x:int -> int

> cube 4;;
val it : int = 64
```

# LINQ
[Language Integrated Query](https://en.wikipedia.org/wiki/Language_Integrated_Query) (LINQ, pronounced "link") is a Microsoft .NET Framework component that adds native data querying capabilities to .NET languages, although ports exist for Java, PHP, JavaScript and ActionScript.

[LINQ](https://msdn.microsoft.com/en-us/library/bb397926.aspx) extends the language by the addition of query expressions, which are akin to SQL statements, and can be used to conveniently extract and process data from arrays, enumerable classes, XML documents, relational databases, and third-party data sources. Other uses, which utilize query expressions as a general framework for readably composing arbitrary computations, include the construction of event handlers or monadic parsers.

- [Typesafe’s Slick is Not About SQL](https://blog.jooq.org/2013/12/30/typesafes-slick-is-not-about-sql/)
- [LINQ analogues in Scala?](http://stackoverflow.com/questions/3785413/linq-analogues-in-scala)

# Erlang
[Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)) is a general-purpose, concurrent, functional programming language. It is also a garbage-collected runtime system. The sequential subset of Erlang supports eager evaluation, single assignment, and dynamic typing. Erlang is known for its designs that are well suited for systems with the following characteristics:

- Distributed
- Fault-tolerant
- Soft real-time,
- Highly available, non-stop applications
- Hot swapping, where code can be changed without stopping a system.

## Documentation
- [Erlang Documentation](http://www.erlang.org/docs)

## Erlang tutorials
- [University of Kent - Erlang Massive Open Online Course (MOOC)](https://www.youtube.com/watch?v=yZ-e6ZT4G6U&list=PLlML6SMLMRgAooeL26mW502jCgWikqx_n)
- [University of Kent - Erlang Master Classes](https://www.youtube.com/watch?v=YZjAHRu4oF8&list=PLlML6SMLMRgCaVx42utIleC2aerD504qj)

## Installing Erlang
Erlang can be installed with the brew formula:

```bash
$ brew install erlang
$ erl
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.3  (abort with ^G)
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.50752066>
2> Fun1(2).
3
```

# Logic in Action
[Logic in Action](http://www.logicinaction.org/) is an open source project and aims at the development of elementary and intermediate courses in logic in electronic form. All material is freely available. Further interactive educational support is continuously being developed. The project provides individual chapters of our self-contained introduction to logic. Some of the material below comes from [Wikipedia](https://en.wikipedia.org) and/or [Logic in Action (note its non secure http)](http://www.logicinaction.org/).

## David Hilbert (1862 – 1943)
[David Hilbert](https://en.wikipedia.org/wiki/David_Hilbert) was a German mathematician. He is recognized as one of the most influential and universal mathematicians of the 19th and early 20th centuries. Hilbert discovered and developed a broad range of fundamental ideas in many areas, including invariant theory and the axiomatization of geometry. He also formulated the theory of Hilbert spaces,[3] one of the foundations of functional analysis.

Hilbert's work had started logic on this course of clarification; the need to understand [Gödel's](https://en.wikipedia.org/wiki/Kurt_G%C3%B6del) work then led to the development of recursion theory and then mathematical logic as an autonomous discipline in the 1930s. The basis for later theoretical computer science, in Alonzo Church and Alan Turing, also grew directly out of this 'debate'.

## John Von Neumann (1903 – 1957)
[John Von Neumann](https://en.wikipedia.org/wiki/John_von_Neumann) was a Hungarian-American pure and applied mathematician, physicist, inventor, computer scientist, and polymath. Von Neumann was a founding figure in computing, he wrote several algoritms, consulted for the Eniac and wrote the paper on the single-memory, stored program architecture is commonly called von Neumann architecture.The architecture's description was based on the work of J. Presper Eckert and John William Mauchly, inventors of the ENIAC computer at the University of Pennsylvania.

- [John Von Neumann Documentary](https://www.youtube.com/watch?v=VTS9O0CoVng)
- [Von Neumann Architecture](https://www.youtube.com/watch?v=RZMh7pASX_o)

## Alan Turing (1912 – 1954)
[Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing) was a pioneering English computer scientist, mathematician, logician, cryptanalyst and theoretical biologist. He was highly influential in the development of theoretical computer science, providing a formalisation of the concepts of algorithm and computation with the Turing machine, which can be considered a model of a general purpose computer. Turing is widely considered to be the father of theoretical computer science and artificial intelligence.

- [Alan Turing - BBC Horizon Documentary](https://www.youtube.com/watch?v=GH1WYUKP3hk)
- [Alan Turing: Pioneer of the Information Age](https://www.youtube.com/watch?v=p7Lv9GxigYU)
- [Turing Machines](https://www.youtube.com/watch?v=gJQTFhkhwPA)

## Alonzo Church (1903 – 1995) (LAMBDA-CALCULUS)
[Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) was an American mathematician and logician who made major contributions to mathematical logic and the foundations of theoretical computer science. He is best known for the lambda calculus.

## John McCarthy (1927 - 2011) (LISP)
An [American computer scientist](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) and the inventor of the LISP programming language.

- [John McCarthy, on Philosophy of AI](https://www.youtube.com/watch?v=K13_sWm_gZw)
- [John McCarthy, on Artificial Intelligence](https://www.youtube.com/watch?v=Ozipf13jRr4)

## Peter Landin (1930 – 2009) (ISWIM)
[Peter John Landin](https://en.wikipedia.org/wiki/Peter_Landin) was a British computer scientist. He was one of the first to realize that the lambda calculus could be used to model a programming language, an insight that is essential to development of both functional programming and denotational semantics.

Peter Landin is responsible for inventing the SECD machine, the first abstract process virtual machine ever defined, and the [ISWIM](https://en.wikipedia.org/wiki/ISWIM) programming language, defining the Landin off-side rule and for coining the term 'syntactic sugar'. The off-side rule allows bounding scope declaration by use of white spaces as seen in languages such as Miranda, Haskell, Python and F# (using the "light" syntax).

## John Backus (1924 - 2007) (FORTRAN)
[John Warner Backus](https://en.wikipedia.org/wiki/John_Backus) was an American computer scientist. He directed the team that invented the first widely used high-level programming language [FORTRAN](https://en.wikipedia.org/wiki/Fortran) and was the inventor of the Backus-Naur form (BNF), a widely used notation to define formal language syntax. He also did research in function-level programming and helped to popularize it.

Backus later worked on a "function-level" programming language known as [FP](https://en.wikipedia.org/wiki/FP_(programming_language)) which was described in his Turing Award lecture "Can Programming be Liberated from the von Neumann Style?". Sometimes viewed as Backus's apology for creating FORTRAN, this paper did less to garner interest in the FP language than to spark research into functional programming in general. 

- [History of John Backus by Grady Booch and and John Backus (2006)](https://www.youtube.com/watch?v=dDsWTyLEgbk)

## Robin Milner (1934 - 2010) (ML)
[Robin Milner](https://en.wikipedia.org/wiki/Robin_Milner) was a British computer scientist, and a Turing Award winner. Milner is generally regarded as having made three major contributions to computer science. He developed LCF, one of the first tools for automated theorem proving. The language he developed for LCF, [ML](https://en.wikipedia.org/wiki/ML_(programming_language)), was the first language with polymorphic type inference and type-safe exception handling. In a very different area, Milner also developed a theoretical framework for analyzing concurrent systems, the calculus of communicating systems (CCS), and its successor, the pi-calculus. At the time of his death, he was working on bigraphs, a formalism for ubiquitous computing subsuming CCS and the pi-calculus.

- [Life and Work of Dr.Robin Milner by Dr. Navin Kabra](https://www.youtube.com/watch?v=A-sKvgLc-Ik)

### Hindley–Milner type system 
In type theory and functional programming, [Hindley–Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) (HM), also known as Damas–Milner or Damas–Hindley–Milner, is a classical type system for the lambda calculus with parametric polymorphism, first described by J. Roger Hindley and later rediscovered by Robin Milner. Luis Damas contributed a close formal analysis and proof of the method in his PhD thesis.

Among HM's more notable properties is completeness and its ability to deduce the most general type of a given program without the need of any type annotations or other hints supplied by the programmer. __Algorithm W__ is a fast algorithm, performing type inference in almost linear time with respect to the size of the source, making it practically usable to type large programs. Hindley-Milner is preferably used for functional languages. It was first implemented as part of the type system of the programming language ML. Since then, HM has been extended in various ways, most notably by constrained types as used in Haskell.

## David Turner (1946)
David A. Turner (born 1946) is a British computer scientist. He is best known for inventing combinator graph reduction and for designing and implementing three seminal (lazy) functional programming languages [SASL](https://en.wikipedia.org/wiki/SASL_(programming_language)) (untyped purely functional lazy/non-strict), [KRC](https://en.wikipedia.org/wiki/Kent_Recursive_Calculator) and [Miranda](https://en.wikipedia.org/wiki/Miranda_(programming_language)) (pure, non-strict, polymorphic, higher order functional programming language), the last of which was awarded a medal for Technical Achievement by the British Computer Society (BCS Awards, 1990).

- [David Turner - Designer of Miranda, SASL and KRC](https://www.youtube.com/watch?v=jbThtwzPB5U)

## James Gosling (1955)
[James Arthur Gosling](https://en.wikipedia.org/wiki/James_Gosling) is a Canadian computer scientist, best known as the father of the Java programming language.

- [James Gosling - Thoughts for Students](https://www.youtube.com/watch?v=r19P3y1VBiw)
- [James Gosling on Apple, Apache, Google, Oracle and the Future of Java](https://www.youtube.com/watch?v=9ei-rbULWoA)

## Grady Booch (1955)
[Grady Booch](https://en.wikipedia.org/wiki/Grady_Booch) is an American software engineer, best known for developing the Unified Modeling Language with Ivar Jacobson and James Rumbaugh. He is recognized internationally for his innovative work in software architecture, software engineering, and collaborative development environments.

- [An Interview with Grady Booch](https://www.youtube.com/watch?v=4wEl0q9YW48)

## Martin Odersky (1958)
[Martin Odersky](https://en.wikipedia.org/wiki/Martin_Odersky) is a German computer scientist and professor of programming methods at EPFL in Switzerland. He specializes in code analysis and programming languages. He designed the Scala programming language and Generic Java (and Pizza before) both with others, and built the current generation of javac, the Java compiler. In 2011, he founded Lightbend Inc. (formerly Typesafe), a company to support and promote Scala, and he currently serves as the chairman and chief architect.

- [Interview with Martin Odersky](https://www.youtube.com/watch?v=py_grf33h90)
- [Scala - the Simple Parts by Martin Odersky](https://www.youtube.com/watch?v=ecekSCX3B4Q)
- [Scala with Style by Martin Odersky](https://www.youtube.com/watch?v=kkTFx3-duc8)
- [Martin Odersky Panel on the Past, Present, and Future of Scala](https://www.youtube.com/watch?v=xUWNcL7NRxg)
- [Martin Odersky: Scala 2.12 is all about Java 8 Compatibility](https://www.youtube.com/watch?v=3xMl9QEWZug)
- [Spark—The Ultimate Scala Collections by Martin Odersky](https://www.youtube.com/watch?v=yzfCTNukfl8)

## Joshua Bloch (1961) 
[Joshua J. Bloch](https://en.wikipedia.org/wiki/Joshua_Bloch) is a software engineer and a technology author, formerly employed at Sun Microsystems and Google. He led the design and implementation of numerous Java platform features, including the Java Collections Framework, the java.math package, and the assert mechanism. He is the author of the programming guide Effective Java (2001), which won the 2001 Jolt Award, and is a co-author of two other Java books, Java Puzzlers (2005) and Java Concurrency In Practice (2006).

- ["Java: The Good, Bad, and Ugly Parts" by Josh Bloch](https://www.youtube.com/watch?v=hcY8cYfAEwU)

## Robert Cecil Martin (Uncle Bob) (SOLID principles)
Robert Cecil Martin (colloquially known as Uncle Bob) is an American software engineer and author. 

- [Functional Programming; What? Why? When?](https://www.youtube.com/watch?v=7Zlp9rKHGD4)

## Carl Hewitt ()
Carl Eddie Hewitt is a computer scientist who designed the Planner programming language for automated planning and the Actor model of concurrent computation, which have been influential in the development of both logic programming and object-oriented programming. 

- [Interview with Carl Hewitt](https://www.youtube.com/watch?v=aSu3M51sTH8)
- [Hewitt, Meijer and Szyperski: The Actor Model (everything you wanted to know...)](https://www.youtube.com/watch?v=7erJ1DV_Tlo)
- [Carl Hewitt - Actors for CyberThings](https://www.youtube.com/watch?v=DNbJY333vUs)

## Joe Armstrong (Erlang)

- [Joe Armstrong - On Erlang, OO, Concurrency, Shared State and the Future, Part 1](https://www.youtube.com/watch?v=7Rg7kxT9Qyw)
- ["The Mess We're In" by Joe Armstrong](https://www.youtube.com/watch?v=lKXe3HUG2l4)
- [K things I know about building Resilient Reactive Systems by Joe Armstrong](https://www.youtube.com/watch?v=rQIE22e0cW8)
- [The How and Why of Fitting Things Together - Joe Armstrong](https://www.youtube.com/watch?v=ed7A7r6DBsM)
- [A Few Improvements to Erlang - Joe Armstrong](https://www.youtube.com/watch?v=h8nmzPh5Npg)

## Erik Meijer (1963)
[Erik Meijer](https://en.wikipedia.org/wiki/Erik_Meijer_(computer_scientist)) is a Dutch computer scientist. From 2000 to early 2013 he was a software architect for Microsoft where he headed the Cloud Programmability Team. He then founded Applied Duality Inc. in 2013. Before that, he was an associate professor at Utrecht University. He received his Ph.D. from Nijmegen University in 1992. Erik Meijer's research has included the areas of functional programming (particularly Haskell) compiler implementation, parsing, programming language design, XML, and foreign function interfaces. His work at Microsoft included C#, Visual Basic, LINQ, Volta, and the Reactive programming framework (Reactive Extensions) for .NET.

- [Interview with Erik Meijer](https://www.youtube.com/watch?v=y3FTPaQaVXo)
- [One Hacker Way by Erik Meijer](https://www.youtube.com/watch?v=FvMuPtuvP5w)
- [Functional Programming from First Principles by Erik Meijer](https://www.youtube.com/watch?v=a-RAltgH8tw)
- [What does it mean to be Reactive? by Erik Meijer](https://www.youtube.com/watch?v=sTSQlYX5DU0)
- [Erik Meijer and Bart De Smet - LINQ-to-Anything](https://www.youtube.com/watch?v=gT0a-BXDufE)
- [Erik Meijer and Dave Thomas - Objects, Functions, Virtual Machines, IDEs and More](https://www.youtube.com/watch?v=O7Wokgvq0Dc)

## Roland Kuhn ()
After earning a PhD in high-energy particle physics and while working as a systems engineer in the space business, Roland came in contact with Akka. He started contributing to the open-source project in 2010 and has been employed by Typesafe since 2011 where he has been leading the Akka team since November 2012.

- [Interview with Roland Kuhn](https://www.youtube.com/watch?v=oEl8-EwVN-w)
- [Roland Kuhn - Akka Typed: Between Session Types and the Actor Model - Curry On](https://www.youtube.com/channel/UC-WICcSW1k3HsScuXxDrp0w)
- [Go Reactive: Blueprint for Future Applications - Roland Kuhn - Trivento Summercamp](https://www.youtube.com/watch?v=auYuWBudVt8)
- [Reactive Design Patterns - Roland Kuhn](https://www.youtube.com/watch?v=r4bJqgqpsIQ)

# Why use Scalaz? 
That's a good question, let me get back on that, first let me [learn a Haskell](http://learnyouahaskell.com/chapters) and at the same
time let me [learn Scalaz](http://eed3si9n.com/learning-scalaz/). 

 - Validation of input,
 - Disjunctions (useful in for-comprehensions),
 - Message passing between modules using for comprehension,
 - Monad transformers :)
 
