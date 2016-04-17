# Polymorphic types
Watch from [16'35](https://youtu.be/VGCE_3fjzU4?t=16m35s)

Also read: [Type variables instead of concrete types](https://wiki.haskell.org/Type_variables_instead_of_concrete_types)

A polymorphic type (or type variable) is a type that will be bound to a concrete type at a later time, but Haskell doesn't know what the type is at this time.

In Haskell, types have uppercase letters. So Integer, Num, Bool, etc.

![Haskell Classes](https://github.com/dnvriend/study-category-theory/blob/master/img/haskell_classes.gif)

```haskell
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
ghci> let echo a = a
echo :: t -> t
-- notice the lowercase t, that is called a polymorphic type
ghci> echo 1
1
it :: Num t => t
ghci> echo 1.1
1.1
it :: Fractional t => t
ghci> echo "haha"
"haha"
it :: [Char]
ghci>
```