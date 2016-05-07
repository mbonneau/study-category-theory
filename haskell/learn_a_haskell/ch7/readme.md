# Modules
A Haskell module is a collection of related functions, types and typeclasses. A Haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something.

You cannot have multiple modules in the same file, that's too bad.. 

## What is loosly coupled?
Code that is separated into self-contained modules which don't rely on each other too much so that they can be re-used later on. Splitting up a library into modules, each module containing function and types that are somehow related and serve some common purpose.

## Haskell standard library
In haskell, the standard library contains a module for manipulating lists, a module for concurrent programming, a module for dealing with 
complex numbers, etc. To see which modules are available in the standard library use this [haskell library reference](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/). 

A great way to pick up new Haskell knowledge is to just click through the standard library reference and explore the modules and their functions. You can also view the Haskell source code for each module. Reading the source code of some modules is a really good way to learn Haskell and get a solid feel for it.

To search for functions or to find out where they're located, use [Hoogle](http://haskell.org/hoogle). It's a really awesome Haskell search engine, you can search by name, module name or even type signature.

## Composing functions
Composing __length__ and __nub__ by doing __length . nub__ produces a function that's the equivalent of:

```haskell
\xs -> length (nub xs)
```

## Haskell Prelude default imports
In Haskell, the [Prelude module](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html) is imported by default. The Prelude module contains functions, types and typeclasses. It basically defines the standard behavior and types of Haskell.

## Importing modules
The syntax for importing modules in a Haskell script is __import <module name>__. This must be done before defining any functions, so imports are usually done at the top of the file. To import all functions from a module do __import Data.List__  

## Importing single functions from Modules
f you just need a couple of functions from a module, you can selectively import just those functions. If we wanted to import only the nub and sort functions from Data.List, we'd do __import Data.List (nub, sort)__  

## Excluding functions from import
You can also choose to import all of the functions of a module except a few select ones. That's often useful when several modules export functions with the same name and you want to get rid of the offending ones do __import Data.List hiding (nub)__

## Qualified imports
Another way of dealing with name clashes is to do qualified imports. The Data.Map module, which offers a data structure for looking up values by key, exports a bunch of functions with the same name as Prelude functions, like filter or null. So when we import Data.Map and then call filter, Haskell won't know which function to use do __import qualified Data.Map__ but now you'll have to qualify the filter function from Data.Map with __Data.Map.filter__, this way when calling just _filter_ it will refer to the default Prelude version.

## Renaming qualified imports
Typing out __Data.Map__ in front of every function from that module is kind of tedious. That's why we can rename the qualified import to something shorter: __import qualified Data.Map as M__  To reference Data.Map's filter function, we just use __M.filter__.  