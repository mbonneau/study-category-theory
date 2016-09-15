# Eugene Yokota - Scalaz Import Guide

This part I found pure gold:

Scalaz makes heavy use of implicits. Both as a user and an extender of the library, it’s important to have general idea on where 
things are coming from. Let’s quickly review Scala’s imports and implicits!

In Scala, imports are used for two purposes: 

1. To include names of values and types into the scope. 
2. To include implicits into the scope.

Implicits are for 4 purposes that I can think of: 

1. To provide typeclass instances. 
2. To inject methods and operators. (static monkey patching) 
3. To declare type constraints. 
4. To retrieve type information from compiler.

Implicits are selected in the following precedence: 

1. Values and converters accessible without prefix via local declaration, imports, outer scope, inheritance, 
and current package object. Inner scope can shadow values when they are named the same. 

2. Implicit scope. Values and converters declared in companion objects and package object of the type, its parts, or super types.