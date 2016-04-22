# Dart
[Dart](https://www.dartlang.org/) is a general-purpose programming language originally developed by Google and later approved as a standard by Ecma (ECMA-408). 
It is used to build web, server and mobile applications, and for Internet of Things (IoT) devices. It is open-source software under a BSD license.
Dart is a class-based, single inheritance, object-oriented language with C-style syntax which compiles into JavaScript or native code. 
It supports interfaces, mixins, abstract classes, reified generics, and optional typing.

The Dart Language specification can be found [here](https://www.dartlang.org/docs/spec/).

## Installing Dart
Dart can be installed using brew by first adding a [Dart tap for homebrew](https://github.com/dart-lang/homebrew-dart):

```bash
brew tap dart-lang/dart
brew install dart
# brew install dart --with-dartium --with-content-shell
```

This will install the dart sdk to `/usr/local/opt/dart/libexec`. 

## Dart and IntelliJ
Install the __dart__ IntelliJ plugin from the plugins setup page in IntelliJ.

## Dartium
[Dartium](https://www.dartlang.org/tools/dartium/) is a special build of Chromium that includes the Dart VM. Using Dartium means you don’t have to compile your code to JavaScript until you’re ready to test on other browsers.
