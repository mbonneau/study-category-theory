# study-category-theory
This resource is for my study on Functional Programming and Category Theory. I will be using Haskell and Scala/Scalaz.

# Disclaimer
Quoting the great [Brendan McAdams](https://twitter.com/rit) "I am by no means a Haskell/Scala/Scalaz expert. I'm a beginner that has made a lot of progress." which also applies to me I guess. So don't learn from me, but maybe, get inspired and go look at Functional Programming, Haskell, Scala and Scalaz. 

# Category Theory Video
- [Introduction to Category Theory 1 - Steven Roman](https://www.youtube.com/watch?v=If6VUXZIB-4)
- [Introduction to Category Theory 2 - Steven Roman](https://www.youtube.com/watch?v=leFJdbZy7Ys)
- [Introduction to Category Theory 3 - Steven Roman](https://www.youtube.com/watch?v=-ocEgjnQMgg)
- [Introduction to Category Theory 4 - Steven Roman](https://www.youtube.com/watch?v=ZUnThAu1ORU)
- [Introduction to Category Theory 5 - Steven Roman](https://www.youtube.com/watch?v=1_b7U62_uEw)
- [Normal Subgroups, Quotient groups and Congruence Relations - Steven Roman](https://www.youtube.com/watch?v=7exk-zI2-3g)

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

# Logic in Action
[Logic in Action](http://www.logicinaction.org/) is an open source project and aims at the development of elementary and intermediate courses in logic in electronic form. All material is freely available. Further interactive educational support is continuously being developed. The project provides individual chapters of our self-contained introduction to logic. Some of the material below comes from [Wikipedia](https://en.wikipedia.org) and/or [Logic in Action (note its non secure http)](http://www.logicinaction.org/).

# International Conference on Functional Programming
- [ICFP - 2009](https://vimeo.com/album/126865)
- [ICFP - 2010](https://vimeo.com/album/1453306)
 
# Visual Studio Code
[Visual Studio Code](https://code.visualstudio.com/) a.k.a. __'Code'__ is a lightweight but powerful source code editor which runs on your desktop
and is available for Windows, OS X and Linux. Code is an open source source code editor developed by Microsoft. It comes with built-in support for 
JavaScript, TypeScript, Node.js, Markdown and git and has a rich ecosystem of extensions for other languages and runtimes that are available on 
the [Visual Studio Code Marketplace](https://marketplace.visualstudio.com/VSCode).

## Installation
Code can be installed using brew:

```bash
$ brew cask install visual-studio-code 
```

After installation, the editor can be found with Spotlight by typing: `code`.

## Haskell support
Code has support for Haskell by means of [Haskell extensions](https://marketplace.visualstudio.com/search?term=haskell&target=VSCode&sortBy=Relevance). 
These extensions can be installed in Code by typing (⌘+P) and pasting:

```
ext install language-haskell
```

## Dart support
Code has support for Dart and can be installed by typing (⌘+P) and pasting:

```
ext install dart
```