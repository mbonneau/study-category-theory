# sbt-haskell
A placeholder project for an upcoming sbt-haskell plugin for sbt that will make it possible to use
the well known sbt build tool to build haskell src files and use the test framework to test haskell code.

# roadmap

1. Learn how to create sbt plugins. I have bought the book [sbt in action by Joshua Suereth](https://www.manning.com/books/sbt-in-action) way back when EAP, but never got 
   around reading it, now would be a good moment. Also there are multiple [blog posts](https://tersesystems.com/2014/06/24/writing-an-sbt-plugin/)
   on how to create an sbt plugin. I've used Maven with an Ant runner and called Groovy code from that script, so how difficult can it be...

1. Read up on dependency management frameworks for Haskell (Cabal?) and see what the possibilities are to replace or extend ivy to resolve / download dependencies
   using Cabal/Hackage

1. Create an initial sbt plugin that will read haskell src code from src/main/haskell, sets the compile environment and compiles it using 'ghc' and outputs to target/haskell

1. Read up on packaging formats that are used for Haskell applications and choose one that is appropriate.

1. Package the application with dependencies in a distribution like eg. tar, zip, dmg (sbt-native-packager??)

1. See whether it is possible to run the Haskell application from sbt

1. Initial study how to test Haskell applications and how to integrate the process in sbt using 'sbt test, testOnly* etc'

1. ...

# Why SBT?
- Flexibility in our build
- The ability to insert custom tasks easily.
- The ability to communicate between tasks. In sbt tasks have output and explicit dependencies. 

sbt consists of TASKS and SETTINGS. 

TASKS:
Sbt is built around tasks; which makes sbt easy I guess. If you want to *do* something,
just execute a task. If you want your task to run another task, just add an explicit dependency between the tasks. If you want
to use the result of a task in another task, just push the output from one task into another. 

All tasks we execute now like 'clean', 'compile', 'test', 'run', 'package' are all tasks. 

SETTINGS:
A setting is just a value, like the name of the project of the scala version to use. Well, that was short..

     




