# study-category-theory
This resource is for my study on Functional Programming, Generic Programming, Typelevel Programming and Category Theory.
I will be using Haskell and Scala/Scalaz and whatever else comes my way.

## Disclaimer
Quoting the great [Brendan McAdams](https://twitter.com/rit) "I am by no means a Haskell/Scala/Scalaz/Cats expert.
I'm a beginner that has made a lot of progress." which also applies to me I guess. So don't learn from me,
but maybe, get inspired and go look at Functional Programming, [Haskell](https://www.haskell.org/),
[Scala](http://www.scala-lang.org/), [Scalaz](https://github.com/scalaz), [the typelevel project](https://github.com/typelevel),
[Cats](https://github.com/typelevel/cats), [Scala Hamsters](https://github.com/scala-hamsters/hamsters),
[Shapeless](https://github.com/milessabin/shapeless) and overall create happy and therefor easy to reason about
composable functions that, when applied, are called programs.

# Category Theory Video
- [(0'31 hr) A practical introduction to category theory - Daniela Sfregola](https://www.youtube.com/watch?v=DsACb1HCcEA)
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

## Functional Programming Videos
- [g ∘ f patterns by Mario Fusco](https://www.youtube.com/watch?v=zmU_ULBzZTQ)
- [(0'51 hr) Javaslang - Functional Java Done Right - Grzegorz Piwowarek](https://www.youtube.com/watch?v=NxbatgLY3V4)
- [(0'48 hr) Functional data structures in Java - Oleg Šelajev](https://www.youtube.com/watch?v=NWV5IrEg3c0)

## Books
- [Steve Awodey - Category Theory Second Edition](https://www.amazon.com/Category-Theory-Oxford-Logic-Guides/dp/0199237182/ref=asap_bc?ie=UTF8)

What is Category Theory? It is the abstract theory of functions, and because functions are _everywhere_ and everywhere that functions are,
there are categories. Maybe Category Theory should have been called _Abstract Function Theory_ (AFT)!

The the notion of a category arose in order to define a functor. 
The notion of a functor arose to define natural transformations. 
The notion of natural transformations arose to define adjoints. 

## Definitions
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
 
## Yoneda Lemma
In mathematics, specifically in category theory, the [Yoneda lemma](https://ncatlab.org/nlab/show/Yoneda+lemma) is an abstract result on functors of the type morphisms into a fixed object.

## Modern ideas in FP
The two most important ideas in modern Functional programming are:

1. __Data first:__ Functional programming is all about putting data first. First define what kinds of data we have in a problem domain 
and what kind of transformations we want on them. Then we are building up the data structures and the code to do the transformations.

2. __Managing of side-effects:__ Functional programming is not (only) about pure funcions any more. Eventually programs will produce side-effects
and side-effect management is a puzzle in many parts.

So its about data management __and__ controlling side-effects.

Data is represented by _Structure_ and Types are represented by _functions_. 

# History of programming languages
- [History of programming languages](http://cdn.oreillystatic.com/news/graphics/prog_lang_poster.pdf) 
 
## Visual Studio Code
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

## Haskell lint support
You'll first have to install [hlint](https://github.com/ndmitchell/hlint)

```bash
$ cabal install hlint
```

Cabal will install hlint to `/Users/your-user-name/Library/Haskell/bin`.

Now install the [hlint extension](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter) (⌘+P) and pasting:

```
ext install haskell-linter
```

### Configuring hlint
Set the following (⌘+P) + settings.json: 

```json
{
	"haskell.hlint.executablePath": "/Users/your-user-name/Library/Haskell/bin/hlint",
	"haskell.hlint.run": "onType", // also: "onSave", "never"
    "haskell.hlint.logLevel": "log"
}
```

__Note:__ Please replace __your-user-name__ with the directory name of your system.

### Setting hits
HLint's hints can be [configured](https://github.com/ndmitchell/hlint#choosing-a-package-of-hints) (⌘+P) + settings.json:

```json
{
	"haskell.hlint.hints": ["Default", "Dollar", "Generalise"],
    "haskell.hlint.ignore": ["Redundant do"],
}
```

## Haskell build
Code has a build option (SHIFT+CMD+B), but this has to be configured (F1: Configure Task Runner), replace the code with the following:

```json
{
	"version": "0.1.0",
	"command": "runghc",
	"isShellCommand": true,
	"args": ["${file}"],
	"showOutput": "always"
}
```

You can now run a haskell file when opened, but note that you'll have to have `main` in scope like so:

```haskell
main :: IO ()
main = putStrLn "Hello World, this is Haskell!"
```

## Show spaces and tabs in editor
Set the following (⌘+P) + settings.json: 

```json
{
	"editor.renderWhitespace": true,
	"editor.insertSpaces": true
}
```

## Autosave
Set the following (⌘+P) + settings.json:

```json
{
	"files.autoSave": "afterDelay",
	"files.autoSaveDelay": 1000,
}
```

## Dart support
Code has support for Dart and can be installed by typing (⌘+P) and pasting:

```
ext install dart
```

https://inoio.de/blog/2014/07/20/type-class-101-monoid/
