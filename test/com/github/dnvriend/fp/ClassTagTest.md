see: http://stackoverflow.com/questions/19386964/i-want-to-get-the-type-of-a-variable-at-runtime

So, strictly speaking, the "type of a variable" is always present, and can be passed around as a type parameter. For example:

val x = 5
def f[T](v: T) = v
f(x) // T is Int, the type of x
But depending on what you want to do, that won't help you. For instance, may want not to know what is the type of the variable, but to know if the type of the value is some specific type, such as this:

val x: Any = 5
def f[T](v: T) = v match {
  case _: Int    => "Int"
  case _: String => "String"
  case _         => "Unknown"
}
f(x)
Here it doesn't matter what is the type of the variable, Any. What matters, what is checked is the type of 5, the value. In fact, T is useless -- you might as well have written it def f(v: Any) instead. Also, this uses either ClassTag or a value's Class, which are explained below, and cannot check the type parameters of a type: you can check whether something is a List[_] (List of something), but not whether it is, for example, a List[Int] or List[String].

Another possibility is that you want to reify the type of the variable. That is, you want to convert the type into a value, so you can store it, pass it around, etc. This involves reflection, and you'll be using either ClassTag or a TypeTag. For example:

val x: Any = 5
import scala.reflect.ClassTag
def f[T](v: T)(implicit ev: ClassTag[T]) = ev.toString
f(x) // returns the string "Any"
A ClassTag will also let you use type parameters you received on match. This won't work:

def f[A, B](a: A, b: B) = a match {
  case _: B => "A is a B"
  case _ => "A is not a B"
}
But this will:

val x = 'c'
val y = 5
val z: Any = 5
import scala.reflect.ClassTag
def f[A, B: ClassTag](a: A, b: B) = a match {
  case _: B => "A is a B"
  case _ => "A is not a B"
}
f(x, y) // A (Char) is not a B (Int)
f(x, z) // A (Char) is a B (Any)
Here I'm using the context bounds syntax, B : ClassTag, which works just like the implicit parameter in the previous ClassTag example, but uses an anonymous variable.

One can also get a ClassTag from a value's Class, like this:

val x: Any = 5
val y = 5
import scala.reflect.ClassTag
def f(a: Any, b: Any) = {
  val B = ClassTag(b.getClass)
  ClassTag(a.getClass) match {
    case B => "a is the same class as b"
    case _ => "a is not the same class as b"
  }
}
f(x, y) == f(y, x) // true, a is the same class as b
A ClassTag is limited in that it only covers the base class, but not its type parameters. That is, the ClassTag for List[Int] and List[String] is the same, List. If you need type parameters, then you must use a TypeTag instead. A TypeTag however, cannot be obtained from a value, nor can it be used on a pattern match, due to JVM's erasure.

Examples with TypeTag can get quite complex -- not even comparing two type tags is not exactly simple, as can be seen below:

import scala.reflect.runtime.universe.TypeTag
def f[A, B](a: A, b: B)(implicit evA: TypeTag[A], evB: TypeTag[B]) = evA == evB
type X = Int
val x: X = 5
val y = 5
f(x, y) // false, X is not the same type as Int
Of course, there are ways to make that comparison return true, but it would require a few book chapters to really cover TypeTag, so I'll stop here.

Finally, maybe you don't care about the type of the variable at all. Maybe you just want to know what is the class of a value, in which case the answer is rather simple:

val x = 5
x.getClass // int -- technically, an Int cannot be a class, but Scala fakes it
It would be better, however, to be more specific about what you want to accomplish, so that the answer can be more to the point.


see: http://stackoverflow.com/questions/12218641/scala-what-is-a-typetag-and-how-do-i-use-it
A TypeTag solves the problem that Scala's types are erased at runtime (type erasure). If we wanna do

class Foo
class Bar extends Foo

def meth[A](xs: List[A]) = xs match {
  case _: List[String] => "list of strings"
  case _: List[Foo] => "list of foos"
}
we will get warnings:

<console>:23: warning: non-variable type argument String in type pattern List[String]↩
is unchecked since it is eliminated by erasure
         case _: List[String] => "list of strings"
                 ^
<console>:24: warning: non-variable type argument Foo in type pattern List[Foo]↩
is unchecked since it is eliminated by erasure
         case _: List[Foo] => "list of foos"
                 ^
To solve this problem Manifests were introduced to Scala. But they have the problem not being able to represent a lot of useful types, like path-dependent-types:

scala> class Foo{class Bar}
defined class Foo

scala> def m(f: Foo)(b: f.Bar)(implicit ev: Manifest[f.Bar]) = ev
warning: there were 2 deprecation warnings; re-run with -deprecation for details
m: (f: Foo)(b: f.Bar)(implicit ev: Manifest[f.Bar])Manifest[f.Bar]

scala> val f1 = new Foo;val b1 = new f1.Bar
f1: Foo = Foo@681e731c
b1: f1.Bar = Foo$Bar@271768ab

scala> val f2 = new Foo;val b2 = new f2.Bar
f2: Foo = Foo@3e50039c
b2: f2.Bar = Foo$Bar@771d16b9

scala> val ev1 = m(f1)(b1)
warning: there were 2 deprecation warnings; re-run with -deprecation for details
ev1: Manifest[f1.Bar] = Foo@681e731c.type#Foo$Bar

scala> val ev2 = m(f2)(b2)
warning: there were 2 deprecation warnings; re-run with -deprecation for details
ev2: Manifest[f2.Bar] = Foo@3e50039c.type#Foo$Bar

scala> ev1 == ev2 // they should be different, thus the result is wrong
res28: Boolean = true
Thus, they are replaced by TypeTags, which are both much simpler to use and well integrated into the new Reflection API. With them we can solve the problem above about path-dependent-types elegantly:

scala> def m(f: Foo)(b: f.Bar)(implicit ev: TypeTag[f.Bar]) = ev
m: (f: Foo)(b: f.Bar)(implicit ev: reflect.runtime.universe.TypeTag[f.Bar])↩
reflect.runtime.universe.TypeTag[f.Bar]

scala> val ev1 = m(f1)(b1)
ev1: reflect.runtime.universe.TypeTag[f1.Bar] = TypeTag[f1.Bar]

scala> val ev2 = m(f2)(b2)
ev2: reflect.runtime.universe.TypeTag[f2.Bar] = TypeTag[f2.Bar]

scala> ev1 == ev2 // the result is correct, the type tags are different
res30: Boolean = false

scala> ev1.tpe =:= ev2.tpe // this result is correct, too
res31: Boolean = false
They are also easy to use to check type parameters:

import scala.reflect.runtime.universe._

def meth[A : TypeTag](xs: List[A]) = typeOf[A] match {
  case t if t =:= typeOf[String] => "list of strings"
  case t if t <:< typeOf[Foo] => "list of foos"
}

scala> meth(List("string"))
res67: String = list of strings

scala> meth(List(new Bar))
res68: String = list of foos
At this point, it is extremely important to understand to use =:= (type equality) and <:< (subtype relation) for equality checks. Do never use == or !=, unless you absolutely know what you do:

scala> typeOf[List[java.lang.String]] =:= typeOf[List[Predef.String]]
res71: Boolean = true

scala> typeOf[List[java.lang.String]] == typeOf[List[Predef.String]]
res72: Boolean = false
The latter checks for structural equality, which often is not what should be done because it doesn't care about things such as prefixes (like in the example).

A TypeTag is completely compiler-generated, that means that the compiler creates and fills in a TypeTag when one calls a method expecting such a TypeTag. There exist three different forms of tags:

scala.reflect.ClassTag
scala.reflect.api.TypeTags#TypeTag
scala.reflect.api.TypeTags#WeakTypeTag
ClassTag substitutes ClassManifest whereas TypeTag is more or less the replacement for Manifest.

The former allows to fully work with generic arrays:

scala> import scala.reflect._
import scala.reflect._

scala> def createArr[A](seq: A*) = Array[A](seq: _*)
<console>:22: error: No ClassTag available for A
       def createArr[A](seq: A*) = Array[A](seq: _*)
                                           ^

scala> def createArr[A : ClassTag](seq: A*) = Array[A](seq: _*)
createArr: [A](seq: A*)(implicit evidence$1: scala.reflect.ClassTag[A])Array[A]

scala> createArr(1,2,3)
res78: Array[Int] = Array(1, 2, 3)

scala> createArr("a","b","c")
res79: Array[String] = Array(a, b, c)
ClassTag provides only the information needed to create types at runtime (which are type erased):

scala> classTag[Int]
res99: scala.reflect.ClassTag[Int] = ClassTag[int]

scala> classTag[Int].runtimeClass
res100: Class[_] = int

scala> classTag[Int].newArray(3)
res101: Array[Int] = Array(0, 0, 0)

scala> classTag[List[Int]]
res104: scala.reflect.ClassTag[List[Int]] =↩
        ClassTag[class scala.collection.immutable.List]
As one can see above, they don't care about type erasure, therefore if one wants "full" types TypeTag should be used:

scala> typeTag[List[Int]]
res105: reflect.runtime.universe.TypeTag[List[Int]] = TypeTag[scala.List[Int]]

scala> typeTag[List[Int]].tpe
res107: reflect.runtime.universe.Type = scala.List[Int]

scala> typeOf[List[Int]]
res108: reflect.runtime.universe.Type = scala.List[Int]

scala> res107 =:= res108
res109: Boolean = true
As one can see, method tpe of TypeTag results in a full Type, which is the same we get when typeOf is called. Of course, it is possible to use both, ClassTag and TypeTag:

scala> def m[A : ClassTag : TypeTag] = (classTag[A], typeTag[A])
m: [A](implicit evidence$1: scala.reflect.ClassTag[A],↩
       implicit evidence$2: reflect.runtime.universe.TypeTag[A])↩
      (scala.reflect.ClassTag[A], reflect.runtime.universe.TypeTag[A])

scala> m[List[Int]]
res36: (scala.reflect.ClassTag[List[Int]],↩
        reflect.runtime.universe.TypeTag[List[Int]]) =↩
       (scala.collection.immutable.List,TypeTag[scala.List[Int]])
The remaining question now is what is the sense of WeakTypeTag ? In short, TypeTag represents a concrete type (this means it only allows fully instantiated types) whereas WeakTypeTag just allows any type. Most of the time one does not care which is what (which means TypeTag should be used), but for example, when macros are used which should work with generic types they are needed:

object Macro {
  import language.experimental.macros
  import scala.reflect.macros.Context

  def anymacro[A](expr: A): String = macro __anymacro[A]

  def __anymacro[A : c.WeakTypeTag](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    // to get a Type for A the c.WeakTypeTag context bound must be added
    val aType = implicitly[c.WeakTypeTag[A]].tpe
    ???
  }
}
If one replaces WeakTypeTag with TypeTag an error is thrown:

<console>:17: error: macro implementation has wrong shape:
 required: (c: scala.reflect.macros.Context)(expr: c.Expr[A]): c.Expr[String]
 found   : (c: scala.reflect.macros.Context)(expr: c.Expr[A])(implicit evidence$1: c.TypeTag[A]): c.Expr[A]
macro implementations cannot have implicit parameters other than WeakTypeTag evidences
             def anymacro[A](expr: A): String = macro __anymacro[A]
                                                      ^
For a more detailed explanation about the differences between TypeTag and WeakTypeTag see this question: Scala Macros: “cannot create TypeTag from a type T having unresolved type parameters”

The official documentation site of Scala also contains a guide for Reflection.