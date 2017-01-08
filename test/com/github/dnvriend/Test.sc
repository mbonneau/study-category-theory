
val str: String = "1"

case class MyInteger(x: Int)
object MyInteger {
  implicit def fromString(str: String): MyInteger =
    MyInteger(Integer.parseInt(str))
}

def foo(str: String): MyInteger = str

foo("1")