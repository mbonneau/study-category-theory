# Week 1 - Introduction
[Channel 9 Functional Programming Fundamentals Week 1 by Erik Meijer](https://www.youtube.com/watch?v=UIUlFQH4Cvo)

# The book
[Programming in Haskell by Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/book.html) ISBN10: __0521692695__ or ISBN13: __9780521692694__ at [bol.com](https://www.bol.com/nl/p/programming-in-haskell/1001004002961226/) or at [Amazon.co.uk](https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695/ref=sr_1_1?s=books&ie=UTF8&qid=1461235210&sr=1-1&keywords=programming+in+haskell). A second edition of Programming in Haskell has been released 2016-09-01 and is 
available as a Kindle and Paperback version.

# Quicksort example

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> :l quicksort.hs
[1 of 1] Compiling Main             ( quicksort.hs, interpreted )
Ok, modules loaded: Main.

ghci> f [5,1,2,4,6,1,7]
[1,1,2,4,5,6,7]
it :: (Num t, Ord t) => [t]
```