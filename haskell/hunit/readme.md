# HUnit
[HUnit](https://github.com/hspec/HUnit) is a unit testing framework for Haskell, inspired by the [JUnit](https://github.com/junit-team) tool for Java. 
Of course HUnit is available on [Hackage](http://hackage.haskell.org/package/HUnit). Both HUnit and JUnit are programmer-oriented testing framework. 

## Installation
To install type:

```bash
cabal install hunit
```

# What is Tasty?
[Tasty](http://documentup.com/feuerbach/tasty) is a modern testing framework for Haskell. It lets you combine your unit tests,
golden tests, QuickCheck/SmallCheck properties, and any other types of tests into a single test suite. Tasty is the core package. 
It contains basic definitions and APIs and a console runner. In order to create a test suite, you also need to install one or more __providers__:

- [tasty-hunit](http://hackage.haskell.org/package/tasty-hunit) — for unit tests (based on HUnit),
- tasty-golden — for golden tests, which are unit tests whose results are kept in files
- tasty-smallcheck — exhaustive property-based testing (based on smallcheck)
- tasty-quickcheck — for randomized property-based testing (based on QuickCheck)
- tasty-hspec — for Hspec tests
- tasty-program — run external program and test whether it terminates successfully

 
 ## Installation
 To install type:
 
 ```bash
 cabal install tasty
 cabal install tasty-hunit
 cabal install tasty-rerun
 ```
 
 
 