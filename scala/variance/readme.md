# Variance
see: http://docs.scala-lang.org/tutorials/tour/variances.html

[A] = Invariant
[+A] = Covariant
[-A] = Contravariant

NonEmptyList is defined as NonEmptyList[A] which means it is an invariant parameterized type.

Lets say we have the following ADT:

