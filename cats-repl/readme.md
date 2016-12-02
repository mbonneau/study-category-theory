# repl

- __CTRL+L:__ clear screen

# scala repl coloring
prompt: magenta
yellow: what you type
green: the type
blue: name of the variable
yellow: The value

To enable coloring launch scala with:

```bash
scala -Dscala.color=true
```

or set in build.sbt:

```scala
initialize ~= { _ =>
  val ansi = System.getProperty("sbt.log.noformat", "false") != "true"
  if (ansi) System.setProperty("scala.color", "true")
}
```

## Typelevel
- [Typelevel Scala](https://github.com/typelevel/scala)
- [Typelvel Cats](https://github.com/typelevel/cats)
- [Typelevel - Scalaz outlaws](https://github.com/typelevel/scalaz-outlaws) 

## Notes
Scratch book:

```
//reify(Monoid[String])
```