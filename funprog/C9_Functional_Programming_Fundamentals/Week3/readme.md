# Week 3 - Types and Classes
[C9 Lectures: Functional Programming Fundamentals Chapter 3 by Erik Meijer](https://www.youtube.com/watch?v=4Z6BlLqAqt8)

# What is a type?
A type is a name for a collection of related __values__ eg. the type Boolean has the values True and False.

If expression __e__ would produce a value of type __t__, then e __has type__ t, written:

```haskell
e :: t
```

Every well formed expression has a type, which can be automatically calculated at compile time using a process called __type inference__.

All type errors are found at __compile time__, which makes programs __safer and faster__ by removing the need for type checks at runtime. 

# List types
A list is a sequence of values of the same type. It uses a __type constructor__ that takes a type to create a value, namely the list that will take only certain types. 

Generally [a] is a list that takes values of type 'a', so 'a' is a __type variable__. 

# Tuple types
A tuple is a sequence of values of __different__ types.

# Function types
A __function__ is a mapping from values of one type to values of another type 

```haskell
not :: Bool -> Bool

isDigit :: Char -> Bool
```

In general, t1 -> t2 is the type of functions that map values of type t1 to values of type t2.

__Note:__ 
The arguments and result types are unrestricted. For example, functions with multiple arguments or results are possible using tuples:

```haskell
add (x, y) = x + y

-- curried
add' x y = x + y
```

The functions __add__ and __add'__ produce the same final result, but __add__ takes its two argument at the same time in the form of a tuple whereas __add'__ takes them one at a time.

Functions that take their arguments one at a time are called __curried__ functions, celebrating the work of Haskell Curry on such functions.

# Typeclasses in Haskell
The explanation about type classes starts [here](https://youtu.be/4Z6BlLqAqt8?t=34m1s).

```haskell
sum :: Num a => [a] -> a
```

The `Num a` is a __type constraint__.

`sum` takes a list of a and returns an a. You cannot sum arbitrary types. You can sum up elements of a list provided that an `a` implements the Num interface (a is in the Num (type) class) In Haskell type classes roughly corresponds to Interfaces in an Object Oriented languages. 

Haskell has a number of type classes:

- Num: Numeric types
- Eq: Equality types
- Ord: Ordered types

For example,

```haskell
(+)  :: Num a => a -> a -> a
(==) :: Eq  a => a -> a -> Bool
(<)  :: Ord a => a -> a -> Bool
```

