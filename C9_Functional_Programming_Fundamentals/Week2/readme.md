# Week 2
[C9 Lectures: Dr. Erik Meijer - Functional Programming Fundamentals Chapter 2 by Erik Meijer](https://www.youtube.com/watch?v=Sm7aEU-8enI)

__Object oriented programming:__
The receiver (object) is the most important thing.
- __receiver__.method (arguments) "I am sending this message to that object"

__Functional Programming:__
The function (method) is the most important thing.
- method receiver arguments in which the receiver and the arguments are evenly important.

# The layout rule
The following [white space rules](https://youtu.be/Sm7aEU-8enI?t=42m47s) apply to denote the block structure:

In a sequence of definitions, each definition must begin precisely in the same column this is because layout is used to denote block structure just like Python,

# Variable notation
The following variable notations apply:

- A number will be an `n`,
- A list will be called `xs`,
_ A list of lists will be called `xss`, 

# Function name and type names
A function name starts with a lower case letter and types start with a capital letter.

# The list
The list is the universal data structure used in functional languages. You do everything with lists in functional languages; you never need to create your own data structures. There are a lot of operations defined in `Prelude` to 
support operations based on lists.

# Examples
Operations on list:

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> length [1,2,3]
3
it :: Int
ghci> sum [1,2,3]
6
it :: Num a => a
ghci> drop 1 [1,2,3]
[2,3]
it :: Num a => [a]
ghci> take 2 [1,2,3]
[1,2]
it :: Num a => [a]
ghci> product [1,2,3]
6
it :: Num a => a
ghci> reverse [1,2,3]
[3,2,1]
it :: Num a => [a]
ghci> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]
it :: Num a => [a]
ghci>
```

In F#:

```fsharp
fsharpi

F# Interactive for F# 4.0 (Open Source Edition)
Freely distributed under the Apache 2.0 Open Source License

> List.length [1;2;3];;
val it : int = 3
> List.sum [1;2;3];;
val it : int = 6
> Seq.take 2 [1;2;3];;
val it : seq<int> = seq [1; 2]
```

# My first script
Put the following in the file `my_first_script.hs`:

```haskell
double x = x + x

quadruple = double . double

factorial n = product [1..n]

average xs = sum xs `div` length xs
```

Now load the repl `ghci` and load the file:

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> :l my_first_script.hs
[1 of 1] Compiling Main             ( my_first_script.hs, interpreted )
Ok, modules loaded: Main.
ghci> :t quadruple
quadruple :: Integer -> Integer
ghci> :t double
double :: Num a => a -> a
ghci> quadruple 10
40
it :: Integer
ghci> take (double 2) [1,2,3,4,5,6]
[1,2,3,4]
it :: Num a => [a]
ghci> factorial 10
3628800
it :: (Enum a, Num a) => a
ghci> average [1,2,3,4,5,6,7,8,9,10]
5
it :: Int
```

# Homework
1. Create a function `last'` that selects the last element of a list using the list functions except 'last'.

```haskell
let last' = xs !! (length xs - 1)
```

2. Create a function `init'` that removes the last element from a list using the list functions except 'init'.

```haskell
let init' = take (length xs - 1)
```

