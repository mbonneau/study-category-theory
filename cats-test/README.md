# cats-test
A small study project on [typelevel cats](https://github.com/typelevel/cats).

## Documentation
Cats has great [documentation](http://typelevel.org/cats/)!

## Semigroup
A [Semigroup](http://typelevel.org/cats/typeclasses/semigroup.html) is a binary associative operation that operates
on the same type, lets call that type `A`.

What does this even mean? Well, this means that the operation has two parameters (binary) and must be of the
same type, `A`. The operation must also return an `A`.

```scala
trait Semigroup[A] {
  def combine(x: A, y: A): A
}
```

The rule is, for each instance of `A` the following must be true, and is easy to test:

```scala
combine(x, combine(y, z)) = combine(combine(x, y), z)
```

If you can implement an instance of `Semigroup` where the above rule is true for instances of `A` for x, y and z
then you have successfully implemented a mathemetical structure called a `Semigroup`.

> If a type A can form a Semigroup it has an associative binary operation.

### What is the clue
Well, its not about the Semigroup really, its about the implementation of the semigroup instance for a certain type.

Lets look at an example for the type `Int`:

```scala
def combine*(x: Int, y: Int): Int = x ??? y
```

If we look at the pseudo code above, the `???` represents an mathematical operation like eg. the `+`, `-`, `*` and so on.
These operations are all binary because they need two inputs:

```scsala
// the '+' sign needs two inputs, 1 and 2 and is
// called a 'binary operation'
1 + 2 = 3
```

Alright, are all binary operations in math associative?

```scala
scala> (1 + 2) + 3 == 1 + (2 + 3)
res0: Boolean = true

scala> (1 - 2) - 3 == 1 - (2 - 3)
res1: Boolean = false

scala> (1 * 2) * 3 == 1 * (2 * 3)
res2: Boolean = true
```

It seems like not all binary operations are associative, but the `+` and `*` are.

## Semigroup instances


## Monoid
[Monoid](http://typelevel.org/cats/typeclasses/monoid.html) extends the power of Semigroup by providing an additional empty value.

```scala
```