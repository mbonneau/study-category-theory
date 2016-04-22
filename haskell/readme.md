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