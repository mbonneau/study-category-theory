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

import com.github.dnvriend.{ Person, TestSpec }

class ValDefInTraitTest extends TestSpec {

  /**
   * In Scala, a class or trait can have members. Members can be declared with
   * def, val, lazy val or var. Also, a def is the most abstract form of defining
   * a member that fits nicely with the nature of a trait; being abstract.
   *
   * A def can be implemented by either of a def, a val, a lazy val or an object.
   *
   * Using a val in a trait is perfectly valid, and declaring a val as abstract ie. not
   * giving an implementation is also perfectly valid. The thing is, when you do this,
   * you must keep in mind the class linearization technique that Scala uses. How a class will be
   * initialized and whether or not a field will be initialized with null or not, is also a problem with an
   * abstract val. This can be mitigated by defining a member as a def or a lazy val. Always go for def here.
   */

  class PersonRepository {
    def people: Seq[Person] = (20 to 22).map(age => Person(s"Person$age", age))
  }

  trait NotInitializedPersonRoute {
    val personRepository: PersonRepository // abstract personRepository
    val route: String = personRepository.people.toJson.toString // will be made 'stable' thus evaluated when initialized
  }

  object NotInializedRestService extends NotInitializedPersonRoute {
    override val personRepository: PersonRepository = new PersonRepository
  }

  "NotInializedRestService" should "initialize with null" in {
    a[NullPointerException] should be thrownBy NotInializedRestService.route
  }

  trait PersonRoute {
    def personRepository: PersonRepository
    def route: String = personRepository.people.toJson.toString
  }

  object RestService extends PersonRoute {
    override val personRepository: PersonRepository = new PersonRepository // def can be replaced with val
  }

  "RestService" should "initialize correctly" in {
    RestService.route shouldBe """[{"firstName":"Person20","age":20},{"firstName":"Person21","age":21},{"firstName":"Person22","age":22}]"""
  }
}
