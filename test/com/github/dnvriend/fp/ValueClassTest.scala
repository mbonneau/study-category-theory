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

import java.util.UUID

import com.github.dnvriend.TestSpec

case class UserId(id: String = UUID.randomUUID.toString) extends AnyVal
case class UserName(name: String) extends AnyVal
case class TypedPerson(id: UserId, name: UserName)

class ValueClassTest extends TestSpec {
  /**
   * When you come from Java (or any other value-based programming language), as a programmer
   * we are used thinking in values. For example, we are used to program imperatively, defining
   * values and mutating them in an execution flow using the underlying thread to do the work.
   *
   * But, as we all have experienced, using this style of programming has some drawbacks. In runtime
   * we must prove that the program does what we think it does. Using the fail-fast paradigm we introduce
   * exceptions that can be thrown when something goes wrong and handing these exceptions.
   *
   * Another way to prove that the code is correct is creating another program that tests the code, the
   * infamous unit tests.
   *
   * The thing is, Exceptions and handing them, and unit tests/integration tests are basically overhead.
   * The programmer already coded the flow, designed the data structures and thought about modularity and reuse
   * and the compiler already created the byte code. So what's the problem?
   *
   * Can we cut away the overhead mensioned above? Well, maybe not fully, but we can cut down a bit and thus saving
   * time (and money) on the applications we create and still prove that it works!
   *
   * How them? Well, we can have the compiler do much more work for us than just creating imperative style byte code.
   *
   * One way is:
   *  1. use Scala and the features it gives us, so use immutability, higher order functions, functors, monads,
   *     declarative style of programming and please, stop using nulls (use Option).
   *  2. start getting used to type everything; this is called contract-first approach in which we are explicit in
   *     which types we are using. So, the pre-conditions of a method must line up with the post-conditions of
   *     the callee, type-wise. The compiler can help us there!
   *
   * But how them? Well, stop stringify/intify/primitify your types. What do I mean by that? Well, for example look
   * at the following data structure:
   *
   * case class User(id: String, name: String)
   *
   * The compiler sees that the structure above has an id field of type String and a field name of type String, so
   * basically it sees a named tuple of type (String, String). The id is a UUID, and the name is the name of a person
   * but the compiler does not know that so it cannot help us when we initialize the structure incorrectly. And of course,
   * we want to fail fast so we must write test code for this structure.
   *
   * Let's make the compiler work for us here using a concept called 'tiny types'.
   *
   * case class UserId(id: String = UUID.randomUUID.toString)
   * case class UserName(name: String)
   * case TypedPerson(id: UserId, name: UserName)
   *
   * val person = TypedPerson(UserId(), UserName("John Doe"))
   *
   * The compiler now sees a Person(UserId, UserName) and does all the work for us making sure that no username will
   * be placed into the id field and vice versa. Basically tiny types are wrappers around the basic types.
   *
   * There is only one problem; these wrapper types are instances of those two classes, a UserId class and a UserName
   * class, and for any Person that live in memory, you have two extra instances, this is not good for performance
   * and memory usage.
   *
   * Value classes to the rescue:
   *
   * Value classes are for performance and memory optimization. A value class is a wrapper around a simple value but
   * when they are compiled to byte code, the type used is the simple value type (the wrapped type, like the Int, Long,
   * String etc) and not an instance of the wrapper type will be created.
   *
   * This is great! The compiler can check the type, but after compilation the underlying simple type will be used
   * in byte code saving performance and memory.
   *
   * Value classes are the first baby steps to thinking in type systems and would advice beginning to let the compiler
   * do more work for us, start thinking more with types and as a result have more spare time!
   */

  "Person" should "be defined using tiny types" in {
    val person = TypedPerson(UserId("abc"), UserName("John Doe"))

    person.name shouldBe UserName("John Doe") // note the typed result, not the String value, start thinking in types
  }
}
