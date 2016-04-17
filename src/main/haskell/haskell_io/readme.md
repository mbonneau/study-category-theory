# Haskell and side effects
Its funny, I want to learn Haskell, and sure enough I feel the need to
do some side effects with it, like say a Hello World!
 
Skimming through [Introduction to Haskell IO/Actions](https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions)
I found that we must use Actions to manipulate the world. To create a 
program that does side effects, just create a file `hello.hs` and put
the following in it:

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

We can compile this small little program to native code (how cool is that)
using the `ghc` compiler:

```bash
$ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

This will create the following files:

```bash
-rwxr-xr-x   1 dennis  dennis   1,3M 15 apr 22:06 hello
-rw-r--r--   1 dennis  dennis   657B 15 apr 22:06 hello.hi
-rw-r--r--   1 dennis  dennis    37B 15 apr 22:06 hello.hs
-rw-r--r--   1 dennis  dennis   1,9K 15 apr 22:06 hello.o
```

We can now launch the binary `hello`:

```bash
$ ./hello
Hello World!
```

# Writing to files
The book [Real World Haskell - Chapter 7. I/O](http://book.realworldhaskell.org/read/io.html) which
is available online for free has an example on how to write to a file. To write to a file we should
use the [writeFile](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:writeFile) [IO computation](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:IO).

> A computation is a concept that does not result at all, until you run it. So a computation must be run. - [Haskell Mailing List: Computation vs Function](https://mail.haskell.org/pipermail/beginners/2009-April/001568.html)

> A value of type `IO` a is a computation (also known as IO Action) which, when performed, does some I/O before returning a value of type a. - [Haskell Documentation: Basic Input and output](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:IO)
    
Create a file `writefile.hs` and put the following in it:

```haskell
import Data.Char(toUpper)
main :: IO ()
main = do
       let name = "dennis"
       writeFile "writefile.txt" (map toUpper name)
```

Compile and run it:

```bash
$ ghc writefile.hs
[1 of 1] Compiling Main             ( writefile.hs, writefile.o )
Linking writefile ...
$ ./writefile
$ cat writefile.txt
DENNIS
```

Nice it works :)

# Haskell and I/O
Haskell strictly separates pure code from code that could cause things to occur in the world. That is, it provides a 
complete isolation from side-effects in pure code. Besides helping programmers to reason about the correctness of their code, 
it also permits compilers to automatically introduce optimizations and parallelism.

IO is a computation, it needs a way to be run. More generally speaking, IO is a mathematical structure called a monad. 
For now we can think of a monad, as a mathematical structure that is design pattern that helps function composability (combining functions).
When we read the [Haskell Documentation about IO](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:IO) it states
that:

> IO is a monad, so IO actions can be combined using either the do-notation or the >> and >>= operations from the Monad class. - [Haskell Documentation: Basic Input and output](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:IO)

Another way to think about monads is that they give an execution context to a function, a way for the function to manipulate a world, the context, and in case of the 
IO monad, it manipulates the world of the computer using I/O semantics. 

# Basic Input/Output
Most of the time we need to sequence the manipulations we do to the outside world for example:

1. We need to greet the user and ask for their name,
2. We need to capture the users input
3. We need to greet the user with their name.

This is a sequence of steps that can be modelled using Haskells IO monad using a sequence of IO actions.
Each IO action is an IO monad. The monad design pattern is such that monads can be sequenced just like the 
description. 

Lets create a simple program that asks for the user's name, capture the input and greets the user with their name.
Create a file `basicio.hs` and put the following code in it:

```Haskell
main :: IO ()
main = do
       putStrLn "Greetings! What is your name?"
       inpStr <- getLine
       putStrLn ("Welcome to Haskell, " ++ inpStr ++ "!")
```

Let's use another command that can interpret our Haskell code:

```bash
$ runghc basicio.hs
Greetings! What is your name?
Dennis
Welcome to Haskell, Dennis!
```

You can see that [putStrLn](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:putStrLn) writes out a String, followed by an end-of-line character. 
[getLine](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:getLine) reads a line from standard input. The `<-` syntax binds the result from executing 
an I/O action to a name. We use the simple list concatenation operator `++` to join the input string with our own text.

In Scala it would be something like this:

```scala
scala> for {
     | _ <- Some(println("Greetings! What is your name?"))
     | inpStr <- Option(scala.io.StdIn.readLine())
     | _ <- Some(println("Welcome to Scala, " + inpStr + "!"))
     | } yield ()
Greetings! What is your name?
Welcome to Scala, Dennis!
res1: Option[Unit] = Some(())
```

Please note that I have very much misused to Option monad for it to work...