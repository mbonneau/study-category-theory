# Scala Symbols
see: http://stackoverflow.com/questions/3554362/purpose-of-scalas-symbol
see: http://stackoverflow.com/questions/1324466/practical-examples-of-using-symbols-in-scala

Symbols are identifiers and identify an item in a program. In this matter they are like class names, method names or attribute names.
But while a class name identifies the class -i.e. the set of properties declaring the class' structure and behavior-
and a method name identifies the method -i.e. the parameters and statements- ,
a symbol name identifies the symbol -i.e. itsself, nothing more-.

According to the Scala book, Symbols are interned: "If you write the same symbol twice, 
both expressions will refer to the exact same Symbol object."

In contrast, Strings are only interned if they appear in literal form 
(at least in Java they are). So I guess if you do a lot of serialization 
of Strings that are then put into collections, you might use symbols instead 
and save yourself some memory.

Symbols are used where you have a closed set of identifiers that you want to be able to compare quickly.
When you have two String instances they are not guaranteed to be interned, so to compare them you must often
check their contents by comparing lengths and even checking character-by-character whether they are the same.
With Symbol instances, comparisons are a simple eq check (i.e. == in Java), so they are constant time (i.e. O(1)) to look up.

This sort of structure tends to be used more in dynamic languages (notably Ruby and Lisp code tends to make a lot of use of symbols)
since in statically-typed languages one usually wants to restrict the set of items by type.

Having said that, if you have a key/value store where there are a restricted set of keys,
where it is going to be unwieldy to use a static typed object, a Map[Symbol, Data]-style structure might well be good for you.

A note about String interning on Java (and hence Scala): Java Strings are interned in some cases anyway;
in particular string literals are automatically interned, and you can call the intern() method on a String
instance to return an interned copy. Not all Strings are interned, though, which means that the runtime still
has to do the full check unless they are the same instance; interning makes comparing two equal interned strings faster,
but does not improve the runtime of comparing different strings.

Symbols benefit from being guaranteed to be interned, so in this case a single reference equality check is both sufficient
to prove equality or inequality.

Interning is a process whereby when you create an object,
you check whether an equal one already exists, and use that one if it does.
It means that if you have two objects which are equal, they are precisely the same object (i.e. they are reference equal).
The downsides to this are that it can be costly to look up which object you need to be using,
and allowing objects to be garbage collected can require complex implementation.