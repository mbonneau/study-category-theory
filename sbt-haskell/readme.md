# sbt-haskell
A placeholder project for an upcoming sbt-haskell plugin for sbt that will make it possible to use
the well known sbt build tool to build haskell src files and use the test framework to test haskell code.

# roadmap

1. Learn how to create sbt plugins. Well I have bought the book "sbt in action" by Joshua Suereth way back when EAP, but never got 
   around reading it, now would be a good moment. Also there are multiple [blog posts](https://tersesystems.com/2014/06/24/writing-an-sbt-plugin/)
   on how to create an sbt plugin. I've used Maven with an Ant runner and called Groovy code from that script, so how difficult will sbt be.. :)

1. Read up on dependency management frameworks for Haskell (Cabal?) and see what the possibilities are to replace or extend ivy to resolve / download dependencies
   using Cabal/Hackage

1. Create an initial sbt plugin that will read haskell src code from src/main/haskell, sets the compile environment and compiles it using 'ghc' and outputs to target/haskell

1. Read up on packaging formats that are used for Haskell applications and choose one that is appropriate.

1. Package the application with dependencies in a distribution like eg. tar, zip, dmg (sbt-native-packager??)

1. See whether it is possible to run the Haskell application from sbt

1. Initial study how to test Haskell applications and how to integrate the process in sbt using 'sbt test, testOnly* etc'

1. ...

