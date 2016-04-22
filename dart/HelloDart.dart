main() {
  print("hello");

  var f = (x) => x + 1; // lambda expression

  print(f(3));

  // define a local function, inside main() we define a local function g and
  // g can refer to the local variable f.
  g(x) {
    return f(x) * 2;
  }

  print(g(4));

  // higher order function twice, it takes a function f and a value x
  // it looks like a lambda expression in haskel
  twice(f,x) => f(f(x));

  // defined as a lambda, looks more like Haskell
  //var twice = (f,x) => f(f(x));

  print(twice(f,3));
}