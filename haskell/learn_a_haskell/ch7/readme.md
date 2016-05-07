# Modules
A Haskell module is a collection of related functions, types and typeclasses. A Haskell program is a collection of modules where the 
main module loads up the other modules and then uses the functions defined in them to do something.

## What is loosly coupled?
Code that is separated into self-contained modules which don't rely on each other too much so that they can be re-used later on.
Splitting up a library into modules, each module containing function and types that are somehow related and serve some common purpose.

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


