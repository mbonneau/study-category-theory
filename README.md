# study-category-theory
Study Functional Programming and Category Theory with [Scala](http://www.scala-lang.org/) and [Haskell](https://www.haskell.org/).

# Haskell
> Haskell is a standardized, general-purpose purely functional programming language, with non-strict semantics and strong static typing. It is named after logician [Haskell Curry](http://en.wikipedia.org/wiki/Haskell_Curry) (September 12, 1900 – September 1, 1982), was an American mathematician and logician. Curry is best known for his work in combinatory logic as well as the concept of currying, a technique used for transforming functions in mathematics and computer science. 
-- <quote>[Wikipedia](http://en.wikipedia.org/wiki/Haskell_(programming_language))</quote>

## Links
- [Haskell - Documentation](https://www.haskell.org/documentation)
- [Haskell - Download the platform for Windows/Mac/Linux](https://www.haskell.org/platform/windows.html)
- [Learn you a Haskell](http://learnyouahaskell.com/)

## Install on the mac
If you have a Mac, and use [Homebrew](http://brew.sh/) for easy packet management, and use the [Homebrew Cask](https://github.com/caskroom/homebrew-cask) extension, installation is easy, just type: 
`brew cask install ghc`.

Add the following to your `~/.bash_profile`:

```
export GHC_DOT_APP="/opt/homebrew-cask/Caskroom/ghc/7.10.1-r0/ghc-7.10.1.app"
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi
```

You can now launch GHC with `CMD+SPACE`, type `ghc` and launch it (enter). 

## GHC Command line interface
I use [iTerm](https://www.iterm2.com) on the Mac to interact with the terminal. Its a great replacement for the default OSX terminal and I highly recommand you use it as well. You can install it by typing: `brew cask install iterm2`. 

When you have installed Haskell by tying: `brew cask install ghc` you should be able to launch the Haskell REPL by typing `ghci` on the terminal and typing `:quit` to quit the REPL. Please note the colon `:`.

```bash
$ ghci
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
Prelude> :quit
Leaving GHCi.
```

## Sublime text and Haskell
To install sublime text on a Mac, type: `brew cask install sublime-text`

Follow the installation instructions on [SublimeHaskell](https://github.com/SublimeHaskell/SublimeHaskell).

# GHC - Glasgow Haskell Compiler
> The Glorious Glasgow Haskell Compilation System, more commonly known as the Glasgow Haskell Compiler or simply GHC, is an open source native code compiler for the functional programming language Haskell. It provides a cross-platform environment for the writing and testing of Haskell code and it supports numerous extensions, libraries, and optimizations that streamline the process of generating and executing code. The lead developers are Simon Peyton Jones and Simon Marlow. It is distributed along with the Haskell Platform.
-- <quote>[Wikipedia](http://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler)</quote>

## Links
- [GHC](http://ghcformacosx.github.io/)

# Scala
> Scala is an object-functional programming language for general software applications. Scala has full support for functional programming and a very strong static type system. This allows programs written in Scala to be very concise and thus smaller in size than other general-purpose programming languages. Many of Scala's design decisions were inspired by criticism of the shortcomings of [Java]().
-- <quote>[Wikipedia](http://en.wikipedia.org/wiki/Scala_(programming_language))</quote>

# Scalaz
- [Learning Scalaz](http://eed3si9n.com/learning-scalaz/)

# Video
- [Youtube - Brian Beckman: Don't fear the Monad](https://www.youtube.com/watch?v=ZhuHCtR3xq8)
- [Youtube - Brian Beckman: The Zen of Stateless State - The State Monad](https://www.youtube.com/watch?v=XxzzJiXHOJs)
- [Youtube - Expert to Expert: Brian Beckman and Erik Meijer - Inside the .NET Reactive Framework (Rx)](https://www.youtube.com/watch?v=looJcaeboBY)
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

# Shapeless
## HList
> HList provides many operations to create and manipulate heterogenous lists (HLists) whose length and element types are known at compile-time.

## Blogs
- [Cool Monday: HList and Shapeless](http://www.edofic.com/posts/2012-10-29-hlist-shapeless.html)
