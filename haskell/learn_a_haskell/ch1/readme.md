# Learn you a Haskell - Starting out
See: [Learn you a Haskell - Starting out](http://learnyouahaskell.com/starting-out)

## Currying
Answer: [cdsmith](https://www.reddit.com/r/haskell/comments/4hef1u/why_does_this_instance_of_fmap_have_mismatched/) 
At its core, Haskell only has functions of _one_ parameter. To model functions of two parameters, Haskell defines
- __f x y__ as an expression to mean __(f x) y__
- __f x y = blah__ as a definition to mean __f x = \y -> blah__

In other words, what looks like passing two arguments to a function is actually just:
1. Passing the first argument, and getting back a function as the result; then
1. Passing the second argument to that new function to get the final result.