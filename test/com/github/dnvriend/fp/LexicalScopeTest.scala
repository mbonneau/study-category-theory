/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

/**
 * This is a Scala adaptation from the example I took from
 * http://howtonode.org/what-is-this
 */
class LexicalScopeTest extends TestSpec { // this is the global scope
  // It's all about where you are..

  // this is the global scope
  val globalName = "global"
  val globalAge = 15

  def localScope(): Unit = { // a code block '{ .. }' creates a new scope
    val localName = "local"
    val localAge = 16
  }

  "global scope" should "show name and age" in {
    // this is a test function, and still we can reference globalName and globalAge,
    globalName shouldBe "global"
    globalAge shouldBe 15
  }

  "localScope" should "not be referenced" in {
    // commented the following two statements, the localName and localAge
    // variable cannot be referenced because they are local
    //    localName shouldBe "local"
    //    localAge shouldBe 16
  }

  /**
   * Lexical Scope
   *
   * Lexical scope is the key to making closures work. Here's a quote
   * from wikipedia about closures and lexical scope:
   *
   * In computer science, a closure is a first-class function
   * with `free variables`, that are bound in the lexical environment.
   * Such a function is said to be "closed over" its free variables.
   * A closure is defined within the scope of its free variables, and
   * the extent of those variables is at least as long as the
   * lifetime of the closure itself.
   *
   * So what does all that mean? Here's an example:
   */

  /**
   * The following method returns a function that returns a function that returns a String.
   * The function myModule contains the local variables name and age, and the
   * function () => String closes over those two variables and can be referenced from within
   * that function.
   */
  def myModule: () => () => String = () => { // create a new scope using code block '{ .. }'
    // lexical scope of the function 'greet'
    val name = "tim"
    val age = 28
    val greet: () => String = () => s"Hello $name. Wow, you are $age years old."
    greet
  }

  "closure 'greet'" should "reference the variables in its lexical scope" in {
    val greeter = myModule()
    greeter() shouldBe "Hello tim. Wow, you are 28 years old."
  }

  def loops(max: Int): () => String = {
    var i = 0
    () => { // note; this is a Function0[String]
      while (i < max)
        i += 1
      i.toString
    }
  }

  "loops" should "close over the free variable 'i' in its lexical scope" in {
    loops(10)() shouldBe "10"
    loops(10) shouldBe a[Function0[_]]
  }
}
