# Rustlang
[Rust](https://www.rust-lang.org/) is a systems programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety. 

# Hello World in Scala
Because Scala is my reference, first a Scala example:

```scala
import scala.io.StdIn
object Hello {
  def main(args: Array[String]) {
    println("What's your name?")
    StdIn.readLine() match {
      case name: String => println(s"Hello, $name")
      case _ => println("Sorry, I didn't hear your null pointer")
    }
  }
}
```

# Hello World in Rust
Now the Rust version:

```rust
use std::io;
fn main() {
    println!("What's your name?");
    let mut name = String::new();
    match io::stdin().read_line(&mut name) {
        Ok(_) => println!("Hello, {}", name),
        Err(e) => println!("Sorry, {}", e)
    }
}
```

# Info
- [A Scala view of Rust](http://koeninger.github.io/scala-view-of-rust/#1)