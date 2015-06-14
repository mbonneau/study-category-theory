package com.github.dnvriend.typesystem

import java.io.{FileInputStream, InputStream}

import com.github.dnvriend.TestSpec

import scala.util.Try

class TypeTest extends TestSpec {
	/**
	 * From the book: Scala in Depth (please buy, it's great!)
	 *
	 * The Scala Type System
	 * =====================
	 * The type system is an important component of the Scala language. It enables lots of
	 * rich optimizations and constraints to be used during compilation, which helps runtime
	 * speed and prevents programming errors. The type system allows us to create
	 * all sorts of interesting walls around ourselves, known as "types". These walls help prevent
	 * us from accidentally writing improper code. This is done through the compiler
	 * tracking information about variables, methods, and classes. The more you
	 * know about Scala’s type system, the more information you can give the compiler,
	 * and the type walls become less restrictive while still providing the same protection.
	 *
	 * The type system will constantly warn you of problems or prevent you from doing things altogether.
	 * The better you communicate with the type system, the less restrictive it becomes. But if you attempt
	 * to do something deemed inappropriate, the compiler will warn you. The compiler can be a great means
	 * of detecting errors (early) if you give it enough information.
	 *
	 * What are types?
	 * ===============
	 * A type is a set of information the compiler knows. This could be anything from “what
	 * class was used to instantiate this variable” to “what collection-of-methods are known to
	 * exist on this variable.” The user can explicitly provide this information, or the compiler
	 * can infer it through inspection of other code. When passing or manipulating variables,
	 * this information can be expanded or reduced, depending on how you’ve written your methods.
	 *
	 * In Scala, types can be defined in two ways:
	 *  1. Defining a class, trait or object.
	 *  2. Directly defining a type using the "type" keyword.
	 *
	 * Defining types using Class, Trait or Object
	 * ===========================================
	 * Defining a class, trait, or object automatically creates an associated type for the class,
	 * trait, or object. This type can be referred to using the same name as the class or trait.
	 * For objects we refer to the type slightly differently due to the potential of classes or
	 * traits having the same name as an object.
	 */

	class ClassName
	trait TraitName
	object ObjectName

	def foo(x: ClassName): ClassName = x
	def bar(x: TraitName): TraitName = x
	def baz(x: ObjectName.type): ObjectName.type = x

	/**
	 * Directly defining a type using the "type" keyword
	 * =================================================
	 * Scala also allows types to be constructed using the "type" keyword. This can be used to
	 * create both concrete and abstract types. Concrete types are created by referring to
	 * existing types, or through structural types. Abstract types are created as place holders
	 * that you can later refine in a subclass. This allows a significant level of abstraction
	 * and type safety within programs.
	 *
	 * The type keyword can only define types within some sort of "context", specifically within a class,
	 * trait, or object, or within subcontext of one of these. The syntax of the type keyword is simple.
	 * It consists of the keyword itself, an identifier, and, optionally, a definition or constraint for
	 * the type. If a "definition" is provided, the type is "concrete". If no constraints or assignments are
	 * provided, the type is considered "abstract".
	 */

	type SomeFooType = String // referring to an existing type
	type SomeBarType = Iterable[Char] // referring to an existing type
	type AbstractType // no constraints nor assignments, it's abstract
	type ConcreteType = SomeFooType
	type ConcreteType2 = SomeFooType with SomeBarType

	/**
	 * Side note: types are also useful for naming your types, which could be useful in tuples
	 */

	type PersonUUID = String
	type FirstName = String
	type LastName = String

	val personTuple: (PersonUUID, FirstName, LastName) = (id, FirstName, LastName)
	// looks better than (String, String, String)

	/**
	 * Notice that concrete types can be defined through combining other types. This new type is referred to
	 * as a "compound type". The new type is satisfied only if an instance meets all the requirements of both
	 * original types. The compiler will ensure that these types are compatible before allowing the combination.
	 */

	/**
	 * Structural types
	 * ================
	 * In Scala, a structural type is created using the type keyword and defining what method "signatures" and
	 * variable "signatures" you expect on the desired type. This allows a developer to define an abstract interface
	 * without requiring users to extend some trait or class to meet this interface.
	 *
	 * Note: Structural types make use of reflection to assert whether or not a type meets all requirements defined
	 * by the structural types (the method and variable signatures). When performance is an issue (so when *not* doing
	 * I/O) don't use structural types, else it is no problem.
	 *
	 * A common use case for structural types is resource handling, where a resource must always be closed, eg.
	 * we could define a type Closable that is a structural type that states that a "Closable" is-a closable when
	 * it has a method 'close(): Unit'
	 */

	type Closable = { def close(): Unit } // define a structural type that only defines a method signature

	def using[A <: Closable, B](stream: A)(f: A => B): B =
		try f(stream) finally Try(stream.close())

	"Resource" should "always be closed after being used" in {
		val inputStream: InputStream = new FileInputStream(".gitignore")
		val lines: List[String] = using(inputStream) { (is: InputStream) =>
			io.Source.fromInputStream(is).getLines().toList
		}
		lines should not be 'empty
	}

	/**
	 * Type constraints
	 * ================
	 * Type constraints are rules associated with a type that must be met for a variable to match the given
	 * type. A type can be defined with multiple constraints at once. Each of these constraints must be satisfied
	 * when the compiler is type checking expressions. Type constraints take the following two forms:
	 *  - Lower bounds (subtype restrictions)
	 *  - Upper bounds (supertype restrictions, also known as Conformance relations)
	 *
	 * Lower bound restrictions can be thought of as super-restrictions. This is where the type selected must be equal
	 * to or a supertype of the lower bound restriction
	 */

}