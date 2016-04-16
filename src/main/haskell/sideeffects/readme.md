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
use the [writeFile](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:writeFile) computation.

> A computation is a concept that does not result at all, until you run it. So a computation must be run. - [Haskell Mailing List: Computation vs Function](https://mail.haskell.org/pipermail/beginners/2009-April/001568.html)

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