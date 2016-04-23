# Cabal
- [Cabal User Guide](https://www.haskell.org/cabal/users-guide/)
- [How to write a Haskell program](https://wiki.haskell.org/How_to_write_a_Haskell_program)
- [Repeat after me: “Cabal is not a Package Manager”](https://ivanmiljenovic.wordpress.com/2010/03/15/repeat-after-me-cabal-is-not-a-package-manager/)

# Video
- [Writing a Haskell Project with Cabal](https://www.youtube.com/watch?v=c-WPDMhxouI)
- [Discussion: Haskell building and packaging by Malcolm Wallace](https://vimeo.com/6572504)

# What is Cabal?
[Cabal](https://www.haskell.org/cabal/users-guide/) is the __C__ommon __A__rchitecture for __B__uilding __A__pplications and __L__ibraries. Cabal is a package system for Haskell software. The point of a package system is to enable software developers and users to easily distribute, use and reuse software. A package system makes it easier for developers to get their software into the hands of users. Equally importantly, it makes it easier for software developers to be able to reuse software components written by other developers.

There is a command line tool called __cabal__ for working with Cabal packages. It helps with installing existing packages and also helps people developing their own packages. It can be used to work with local packages or to install packages from online package archives, including automatically installing dependencies. By default it is configured to use Hackage which is Haskell’s central package archive that contains thousands of libraries and applications in the Cabal package format.

```bash
$ brew install ghc cabal-install
$ cabal -V
cabal-install version 1.22.6.0
using version 1.22.5.0 of the Cabal library
```

Packaging systems deal with packages and with Cabal we call them Cabal packages. The Cabal package is the unit of distribution. Every Cabal package has a name and a version number which are used to identify the package, e.g. __filepath-1.0__.

Cabal packages can depend on other Cabal packages. There are tools to enable automated package management. This means it is possible for developers and users to install a package plus all of the other Cabal packages that it depends on. It also means that it is practical to make very modular systems using lots of packages that reuse code written by many developers.

Cabal packages are source based and are typically (but not necessarily) portable to many platforms and Haskell implementations. The Cabal package format is designed to make it possible to translate into other formats, including binary packages for various systems.

When distributed, Cabal packages use the standard compressed tarball format, with the file extension .tar.gz, e.g. __filepath-1.0.tar.gz__.

# What’s in a package
A Cabal package consists of:

- Haskell software, including libraries, executables and tests,
- metadata about the package in a standard human and machine readable format (the “.cabal” file),
- a standard interface to build the package (the __Setup.[l]hs__ file),

The __.cabal__ file contains information about the package, supplied by the package author. In particular it lists the other Cabal packages that the package depends on.

The __Setup.[l]hs__ file is a valid Haskell program/script using the Cabal and which performs the actual configuration, building and installation of the package; for most packages this is a mere two lines long (one to import the Cabal library, the second that states that it uses the default build setup).

For more information see [Building and installing packages](https://www.haskell.org/cabal/users-guide/installing-packages.html).

# What is Hackage?
[Hackage](https://hackage.haskell.org/) is the Haskell community's central package archive of open source software. Package authors use it to publish their libraries and programs while other Haskell programmers use tools like __cabal-install__ to download and install packages (or people get the packages via their distro).

# What is HackageDB?
HackageDB is the central repository of open-source Haskell software. However, it is limited solely to Haskell software that is installed using Cabal. 

# Cabal-install
The [cabal-install](http://hackage.haskell.org/package/cabal-install) package provides the command line tool __cabal__. The __cabal__ command-line tool simplifies the process of managing Haskell software by automating the fetching, configuration, compilation and installation of Haskell libraries and programs.

Whilst cabal-install may use Cabal and act as a wrapper around both it and HackageDB, it is indeed a completely separate package. So remember, whilst you may do __cabal install xmonad__, you’re not using Cabal to do that but rather cabal-install. 

The command __cabal-install__ brings a convenient command-line interface, dependency resolution and downloading to the Hackage ecosystem. Assuming that you have GHC installed, __cabal install xmonad__ will indeed determine, download and build all Haskell dependencies for XMonad.

As a wrapper around Cabal and HackageDB, cabal-install only manages Haskell libraries. 

# Installing packages from Hackage
The __cabal__ tool also can download, configure, build and install a Hackage package and all of its dependencies in a single step. To do this, run:

```bash
cabal install [PACKAGE...]
```

To browse the list of available packages, visit the [Hackage](http://hackage.haskell.org/) web site.
