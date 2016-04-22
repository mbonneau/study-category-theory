# F-sharp
[F-sharp](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)) can be run [cross platform](http://fsharp.org/guides/mac-linux-cross-platform/). F# comes preinstalled with the mono brew formula:

```fsharp
$ brew install mono
$ fsharpi
F# Interactive for F# 4.0 (Open Source Edition)
Freely distributed under the Apache 2.0 Open Source License

For help type #help;;

>
```

## Introduction
[F-sharp](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)) is a strongly typed, multi-paradigm programming language that encompasses functional, imperative, and object-oriented programming techniques. F# is most often used as a cross-platform CLI language, but can also be used to generate JavaScript and GPU code.

F# is developed by the F# Software Foundation, Microsoft and open contributors. An open source, cross-platform compiler for F# is available from the F# Software Foundation. F# is also a fully supported language in Visual Studio and [Xamarin Studio](https://www.xamarin.com/studio). Other tools supporting F# development include [Mono](http://fsharp.org/use/mac/), MonoDevelop, SharpDevelop, MBrace and WebSharper.

F# originated as .NET implementation of a core of the [OCaml](https://en.wikipedia.org/wiki/OCaml) programming language, and has also been influenced by C#, Python, Haskell, Scala and Erlang.

## Using the F-sharp REPL
The REPL is called 'F# Interactive' and can be run by executing `fsharpi`.  It accepts F# code, compiles and executes it, then prints the results. This allows you to quickly and easily experiment with F# without needing to create new projects or build a full application to test the results of a code snippet. 

The REPL accepts F# code until you terminate the input with `;;` and a newline (enter). If the REPL evaluates an expression that was not assigned to a value, it will instead assign it to the name `it`, just like Haskell does. 

For example:

```fsharp
$ fsharpi
F# Interactive for F# 4.0 (Open Source Edition)
Freely distributed under the Apache 2.0 Open Source License

For help type #help;;

> let x = 42;;

val x : int = 42

> x * 2;;
val it : int = 84
>
```
The REPL will process input and displays the name, type, and value of the identifiers. For example, in the example above the value x was introduced with type int and value 42.

You can also type multiple expressions, just terminate the input with `;;` and a newline (enter).

```fsharp
> let x = 1
- let y = 2.3
- float x + y
- let cube x = x * x * x;;

val x : int = 1
val y : float = 2.3
val cube : x:int -> int

> cube 4;;
val it : int = 64
```

# LINQ
[Language Integrated Query](https://en.wikipedia.org/wiki/Language_Integrated_Query) (LINQ, pronounced "link") is a Microsoft .NET Framework component that adds native data querying capabilities to .NET languages, although ports exist for Java, PHP, JavaScript and ActionScript.

[LINQ](https://msdn.microsoft.com/en-us/library/bb397926.aspx) extends the language by the addition of query expressions, which are akin to SQL statements, and can be used to conveniently extract and process data from arrays, enumerable classes, XML documents, relational databases, and third-party data sources. Other uses, which utilize query expressions as a general framework for readably composing arbitrary computations, include the construction of event handlers or monadic parsers.

- [Typesafe’s Slick is Not About SQL](https://blog.jooq.org/2013/12/30/typesafes-slick-is-not-about-sql/)
- [LINQ analogues in Scala?](http://stackoverflow.com/questions/3785413/linq-analogues-in-scala)
