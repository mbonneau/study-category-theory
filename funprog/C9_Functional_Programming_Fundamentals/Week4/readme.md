# Week 4 - Defining Functions
[C9 Lectures: Dr. Erik Meijer - Functional Programming Fundamentals Chapter 4 by Erik Meijer](https://www.youtube.com/watch?v=fQU99SJdWGY)

# Conditional expressions
As in most programming languages, functions can be defined using __conditional expressions__.

```haskell
abs' n = if n >= 0 then n else -n
```

You must always have an else part. The if __expression__ returns a value.

# Guarded equations
As an alternative to conditionals, functions can also be defined using __guarded equations__.

```haskell
abs n | n >= 0    = n
      | otherwise = -n
```

Guarded equations are like Scala pattern matching with guards:

```scala
def abs(n: Int): Int = n match {
	case _ if n >= 0 => n
	case _           => -n
}
```

Guarded equations can be used to make definitions involving multiple conditions easier to read:

__Note:__ The catch all condition 'otherwise' is defined in Prelude by __otherwise = True__.

# Pattern Matching
Many functions have a particular clear definition using pattern matching on their arguments.

# List patterns
Internally, every non-empty list is constructed by repeated use of the operator (:) called 'cons' that adds an element to the start of a list.

```haskell
let xs = [1,2,3,4]
```

__Note:__
- x:xs patterns only match __non-empty__ lists:

```haskell
ghci> head' []
*** Exception: functions.hs:39:1-15: Non-exhaustive patterns in function head'
```

- x:xs patterns must be __parenthesised__, because application has priority over (:), so the correct definition for head is:

```haskell
head' :: [a] -> a
head' (x:_) = x
```
# n + k patterns
The [n+k patterns are removed from Haskell 2010](http://stackoverflow.com/questions/3748592/what-are-nk-patterns-and-why-are-they-banned-from-haskell-2010) also see [Wikipedia on this subject](https://en.wikipedia.org/wiki/Haskell_%28programming_language%29#Haskell_2010)

## Only for Haskell < 2010
As in mathematics, functions on integers can be defined using __n+k__ patterns, where __n__ is an integer variable and __k>0__ is an integer constant. 

```haskell
pred :: Int -> Int
pred (n+1) = n
```

__Note:__ pred maps any positive integer to its predecessor.

# Lamdba expressions
Functions can be constructed without naming the functions by using __lambda expressions__:

```haskell
-- normal function declaration
sum' :: Num a => a -> a
sum' x = x + x

-- lambda declaration
sum'' :: Num a => a -> a
sum'' = \x -> x + x
```

- Lambda are values.
- The symbol `\` is the greek letter __lambda__ and is the backslash character.
- In mathematics, nameless functions are usually denoted using the __|->__ symbol, as in __x |-> x + x__,
- In Haskell, the use of the \ symbol for nameless functions comes from the __lambda calculus__, the theory of functions on which Haskell is based.

# Why are lambda's useful?
Lambda expressions are useful because they can be used to give a formal meaning to functions defined using __currying__:

```haskell
let add x y = x + y

-- means

let add = \x -> (\y -> x + y)
```

Lambda expressions are also useful when defining functions that return __functions as results__:

```haskell
const :: a -> b -> a
const x _ = x

-- can be more naturally defined as:

const :: a -> (b -> a)
const x = \_ -> x
```

Lambda expressions can be used to avoid naming functions that are only __referenced once__:

```haskell
odds n = map f [0..n-1]
         where
            f x = x * 2 + 1

-- can be simplified to
odds n = map (\x -> x*2 + 1) [0..n-1]
```

# Sections
An (binary) operator written __between__ its two arguments can be converted into a curried function written __before__ its two arguments by using parentheses:

```haskell
ghci> (+) 1 2
3
it :: Num a => a
ghci> 1 + 2
3
it :: Num a => a

-- (+) can become a function
ghci> let plus' = (+)
plus' :: Num a => a -> a -> a
ghci> plus' 1 2
3
it :: Num a => a

-- (1+) can also become a function
ghci> let plusOne = (1+)
plusOne :: Num a => a -> a
ghci> plusOne 1
2
it :: Num a => 

-- (+2) can also become a function
ghci> let plusTwo = (+2)
plusTwo :: Num a => a -> a
ghci> plusTwo 1
3
it :: Num a => a
```
# Why are sections useful?
Useful functions can sometimes be constructed in a simple way using sections. For example:

- (1+) - successor function
- (1/) - reciprocation function (taking the opposite)
- (*2) - doubling function
- (/2) - halving function

# Homework
1. Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail using:
- a conditional expression,
- guarded equations,
- pattern matching.

__Hint:__ the library function null :: [a] -> Bool can be used to test if a list is empty.

2. Give three possible definitions for the logical or operator (||) using pattern matching.

3. Redefine the following version of (&&) using conditionals rather than patterns:

```haskell
True && True = True
_ && _ = False
```

4. Do the same for the following version:

```haskell
True && b = b
False && _ = False
```
