# Shapeless
Shapeless is a library for 'General Programming in Scala'.

## There is a guide
- [PDF - Shapeless Guide PDF](https://github.com/underscoreio/shapeless-guide/blob/develop/dist/shapeless-guide.pdf)
- [Github - Shapeless Guide](https://github.com/underscoreio/shapeless-guide)

## What does it solve?
When types are too specific (they almost always are) and then work against us for example when we use adapter
(and we use them all the time!)

## Type class derivation:
[Type class derivation](http://typelevel.org/blog/2013/06/24/deriving-instances-1.html)
allows us to create type class instances for any algebraic data type using only a handful of generic rules.

## Generic representations
- The `Generic` type class can produce a generic encoding for any case class or sealed trait.
- The `LabelledGeneric` is a variantion of `Generic` that exposes field and type names as part of its generic representations.

## Calculus
mapping, flat mapping, and folding

## ADTs
Algebraic data types (ADTs)1 are a functional programming concept with a fancy name but a very simple meaning.
They are an idiomatic way of representing data using “ands” and “ors”. For example:

- a shape is a rectangle or a circle
- a rectangle has a width and a height
- a circle has a radius

In ADT terminology, “and” types such as rectangle and circle are called products and “or” types such as shape are
called coproducts. In Scala we typically represent products using case classes and coproducts using sealed traits.

Why the word “algebra”? Well, the symbols we define, such as rectangle and circle; and the rules for manipulating
those symbols, encoded as methods like eg. def area(Shape): Double where Shape can be either Rectangle or Circle.

It is good practise to encode ADTs in Scala using case classes and sealed traits, but there is also another
way using the Scala standard library and use `Tuples` to encode generic products and `Either` to encode a generic coproducts.

## Generic type class
Shapeless provides a type class called `Generic` that allows us to switch back and forth between a concrete ADT
and its generic representation:

```scala
scala> elsa
res1: Cat = Cat(Elsa,18)

scala> val catGen = Generic[Cat]
catGen: shapeless.Generic[Cat]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.HNil]]} = anon$macro$15$1@637bd983

scala> val pgen = Generic[Person]
pgen: shapeless.Generic[Person]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.HNil]]} = anon$macro$18$1@1de33b43

scala> val genericElsa = catGen.to(elsa)
genericElsa: catGen.Repr = Elsa :: 18 :: HNil

scala> val personElsa = pgen.from(genericElsa)
personElsa: Person = Person(Elsa,18)

scala> val tupleGen = Generic[(String, Int)]
tupleGen: shapeless.Generic[(String, Int)]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.HNil]]} = anon$macro$27$1@3e784a24

scala> tupleGen.from(genericElsa)
res2: (String, Int) = (Elsa,18)
```