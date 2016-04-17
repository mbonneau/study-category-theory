# Chapter 2 - Your first steps
Haskell has four things:

1. A `value` in Haskell is any data like a number, a text etc. A number like '73' or a text like "My cat drinks milk" are values.

2. [Types are sets of values](https://www.youtube.com/watch?v=x3uF7fcQwWE). Thus a single type is a set of similar values. 
For example, the type of the value **"My cat drinks milk"** is a **List of Char**, because a text is actually a list of characters. 
All integer numbers like 1, 21, 42 are of type **Num**, rational numbers like 0.5 are of type **Fractal** and True is of type **Boolean**. 
Note that type names start with an upper case like 'Num', 'Fractal' and 'Boolean'. 
  
Using `ghci` we can inspect the type of an expression using the `:type` or `:t` command:

```haskell
$ ghci
ghci> :type "My cat drinks milk"
"My cat drinks milk" :: [Char]
ghci> :t "My cat drinks milk"
"My cat drinks milk" :: [Char]
ghci> :t True
True :: Bool
ghci> :t 42
42 :: Num a => a
```

The `::` must be read as **has-type**, so '"My cat drinks milk" :: [Char]' must be read like _'"My cat drinks milk" has type list of char'_,
True has type Boolean but whats up with **42 :: Num a => a**? Let's break it down. The bold part in '42 :: **Num a** => a' means, _there is some 'type a' 
in the typeclass Num_. Let's look further. The bold part in '**42** :: Num a => **a**' means, _'42 is of type a'_. Now 'a' is called a **type variable**.
The value 42 belongs to every type in the Num typeclass including Integer and Double. This is because Haskell didn't commit to a specific type; it's lazy. 
We can force the type by using an explicit type annotation:

```haskell
ghci> let x = 42 :: Int
ghci> :t x
x :: Int
```

This should be read as 'x has type Int'.

Functions also have types. Lets look at the function [head](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:head), that 
extract the first element of a list, which must be non-empty. 

```haskell
ghci> :t head
head :: [a] -> a
```

This means, for any type 'a', head takes a list of 'a', and returns an a.

3. The '=' symbol gives an expression a name. For example `let x = 5` binds the value '5' to the name 'x'.   

4. A function is a relation between one type and another type. If you provide a function with an input value, (by applying the function to the value), 
it will return the corresponding output value from the output type.
   
Putting it all together:

```haskell
main = putStrLn "Polly wants a cracker"
```

1. main is called a 'term',
2. putStrLn is called a 'function',
3. "Polly wants a cracker" is called a 'String',
4. 2 + 3 is called an 'expression'
5. 1 + 4 is called a 'definition'.

Creating a `parrot.hs` file and running it with `runghc parrot.hs` is really easy:

```haskell
main :: IO ()
main = putStrLn "Polly wants a cracker"
```