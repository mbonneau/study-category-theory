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

class SimpleClassesTest extends TestSpec {

  // create the simplest class ever...
  class SimpleClass

  "SimpleClass" should "be instantiated" in {
    new SimpleClass shouldBe a[SimpleClass]
    new SimpleClass shouldBe a[AnyRef] // note, AnyRef is used when no superclass is extended explicitly
  }

  class SimpleClass2 {
    // class gets a primary constructor
    // class body (except fields and methods) is
    // the implementation of the constructor
    println("Hello World!")
  }

  "SimpleClass" should "have a primary constructor" in {
    new SimpleClass2 shouldBe a[SimpleClass2]
    new SimpleClass2 shouldBe a[AnyRef]
  }

  class SimpleClass3(message: String) {
    // the `message` parameter is the parameter of the primary constructor
    println(message)
  }

  "SimpleClass3" should "have one or more parameters" in {
    new SimpleClass3("Hi!") shouldBe a[SimpleClass3]
    new SimpleClass3("Hi!") shouldBe a[AnyRef]
  }

  class SimpleClass4(message: String) {
    // `this()` is an additional constructor
    // you can have multiple additional constructors
    // in the end the primary constructor must be called
    def this() = this("default!")
    println(message)
  }

  "SimpleClass4" should "have additional constructors" in {
    new SimpleClass4("Hi!") shouldBe a[SimpleClass4]
    new SimpleClass4("Hi!") shouldBe a[AnyRef]
    new SimpleClass4() shouldBe a[SimpleClass4]
    new SimpleClass4() shouldBe a[AnyRef]
  }

  // class parameters are not fields, they are only
  // accessible in the body of the class, and not from outside
  class SimpleClass5(message: String)

  "SimpleClass5" should "have class parameters" in {
    // new SimpleClass5("foo").message // does not have access to the private field
  }

  // adding val/var promotes the class parameter to a field
  class SimpleClass6(val message: String)

  "SimpleClass6" should "have immutable field (class parameter promoted to a field)" in {
    val sc = new SimpleClass6("message")
    sc.message shouldBe "message"
    //    sc.message = ""  // does not work, it does not have a setter
  }

  class SimpleClass7(var message: String)

  "SimpleClass7" should "have mutable field (class parameter promoted to a field)" in {
    val sc = new SimpleClass7("message")
    sc.message shouldBe "message"
    sc.message = ""
    sc.message shouldBe ""
  }

  class SimpleClass8 {
    val message = "foobar"
  }

  "SimpleClass8" should "have immutable field" in {
    val sc = new SimpleClass8()
    sc.message shouldBe "foobar"
    //    sc.message = ""  // does not work, it does not have a setter
  }

  class SimpleClass9 {
    var message = "foobar"
  }

  "SimpleClass9" should "have mutable field" in {
    val sc = new SimpleClass9()
    sc.message shouldBe "foobar"
    sc.message = ""
    sc.message shouldBe ""
  }

  class SimpleClass10 {
    def echo(message: String): String = message
  }

  "SimpleClass10" should "have operations/behavior/methods" in {
    new SimpleClass10().echo("foo") shouldBe "foo"
  }
}
