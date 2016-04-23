# Creating a Haskell application
- [How to write a Haskell program](https://wiki.haskell.org/How_to_write_a_Haskell_program)

# Creating Hello Haskell
We will create a very simple project, 'hello-haskell'. Let's create a directory 'hello-haskell'.

```bash
$ mkdir hello-haskell
```

We will use the command __cabal init__ to 'cabalise' a project by creating a __.cabal__, __Setup.hs__, and optionally a __LICENSE__ file.

Enter the directory and type `cabal init`:

```bash
$ cd hello-haskell
$ cabal init
Package name? [default: hello-haskell]
Package version? [default: 0.1.0.0]
Please choose a license:
  12) Apache-2.0
 our choice? [default: (none)] 12
Author name? [default: Dennis Vriend]
Maintainer email? [default: dnvriend@gmail.com]
Project homepage URL? https://github.com/dnvriend/hello-haskell
Project synopsis? An hello world application
Project category:
 * 1) (none)
Your choice? [default: (none)]
What does the package build:
   1) Library
   2) Executable
Your choice? 2
What is the main module of the executable:
 * 1) Main.hs (does not yet exist)
   2) Main.lhs (does not yet exist)
   3) Other (specify)
Your choice? [default: Main.hs (does not yet exist)]
What base language is the package written in:
 * 1) Haskell2010
   2) Haskell98
   3) Other (specify)
Your choice? [default: Haskell2010]
Include documentation on what each field means (y/n)? [default: n] n
Source directory:
 * 1) (none)
   2) src
   3) Other (specify)
Your choice? [default: (none)] 2

Guessing dependencies...

Generating LICENSE...
Generating Setup.hs...
Generating hello-haskell.cabal...

You may want to edit the .cabal file and add a Description field.
dennis@MacBook-Pro-van-Dennis ~/projects/hello-haskell $
```

The following directory structure and files have been created:

```
dennis@MacBook-Pro-van-Dennis ~/projects/hello-haskell $ tree
.
├── LICENSE
├── Setup.hs
├── hello-haskell.cabal
└── src
```

Let's create the following `.gitignore` file:

```
dist
dist-*
cabal-dev
*.o
*.hi
*.chi
*.chs.h
*.dyn_o
*.dyn_hi
.hpc
.hsenv
.cabal-sandbox/
cabal.sandbox.config
*.prof
*.aux
*.hp
*.eventlog
.stack-work/
```

Lets initialize a git repo:

```bash
$ git init
$ git add .
$ git commit -m "init"
```

Use your favorite editor and create the following file `/src/Main.hs`:

```haskell
main :: IO ()
main = do
       putStrLn "Greetings! What is your name?"
       inpStr <- getLine
       putStrLn ("Welcome to Haskell, " ++ inpStr ++ "!")
```

We can now run the application:

```bash
$ cabal run
Preprocessing executable 'hello-haskell' for hello-haskell-0.1.0.0...
Running hello-haskell...
Greetings! What is your name?
Dennis
Welcome to Haskell, Dennis!
```
