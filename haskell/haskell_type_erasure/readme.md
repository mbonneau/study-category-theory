# Does Haskell erase types?
See [stackoverflow](http://stackoverflow.com/questions/12468722/does-haskell-erase-types)

## Question 
> Does Haskell erase types, and if so, in what ways is this similar/dissimilar to the type erasure that occurs in Java? - [Bittercoder](http://stackoverflow.com/users/4843/bittercoder)

## Answer by [AndrewC](http://stackoverflow.com/users/1598537/andrewc)
**Warning**: experience+inference. Consult someone who works on both compilers for The Truth.

In the sense that type checking is done at compile time, and several complex features of the type system are reduced to much simpler language constructs, yes - edit: Haskell erases types, but in a rather different way to [Java](http://www.beyondjava.net/blog/type-erasure-revisited/).

A type signature creates no runtime overhead. The Haskell compiler is good at program transformation (it has more leeway, because the running order is in many cases not specified by the programmer), and automatically inlines appropriate definitions and specialises haskell-polymorhpic (=java-generic) functions to a particular type etc, as it sees fit, if it helps. That's a similar to Java type erasure, but more-so aspect.

There are in essence no type casts needed in Haskell to ensure type safety, because Haskell is designed to be type-safe from the ground up. We don't resort to turning everything into an Object, and we don't cast them back, because a polymorphic(generic) function genuinely does work on any data type, no matter what, pointer types or unboxed integers, it just works, without trickery. So unlike Java, casting is not a feature of compiling polymorphic(generic) code. Haskell folk tend to feel that if you're doing type casting, you said goodbye to type safety anyway.

For a lovely example of how ensuring the code's static type-correctness at compile time can avoid runtime overhead, there's a [newtype](https://wiki.haskell.org/Keywords#newtype) construct in Haskell which is a type-safe wrapper for an existing type, and it's completely compiled away - all the construction and destruction simply doesn't happen at runtime. The type system ensures at compile time it's used correctly, it can't be got at at runtime except using (type-checked) accessor functions.

Polymorphic(generic) functions don't have polymorphic overheads. Haskell-overloaded functions (Java-interface-instance methods) have a data overhead in the sense that there's an implicit dictionary of functions used for what appears to be late binding to Java programmers, but is in fact, again, determined at compile time.

Summary: yes, even more so than in Java, and no, they were never there at runtime to erase anyway.

## Venkat 
See Venkat Subramaniam explanation in his talk [Haskell for Everyday Programmers - from 12'15 and on](https://youtu.be/VGCE_3fjzU4?t=12m15s)

- **Static typing**: you want to do verification at compile time
- **Strong typing**: the verification happens at runtime

- **Haskell**: Static typing at compile time, strong typing at runtime
- **Java**: Static typing at compile time, strong typing at runtime; you get at ClassCastException at runtime,
- **C++**: Static typing at compile time, weak typing at runtime; after the compiler, at runtime all bets are off,
- **Ruby**: Dynamic typing at compile time, strong typing at runtime,
- **JavaScript**: Dynamic typing, weak typing at runtime.
