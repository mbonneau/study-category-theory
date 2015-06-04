package com.github.dnvriend.fp

import com.github.dnvriend.{Person, TestSpec}
import spray.json._

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
    def persons: Seq[Person] = (20 to 22).map(age => Person(s"Person$age", age))
  }

  trait NotInitializedPersonRoute {
    val personRepository: PersonRepository // abstract personRepository
    val route: String = personRepository.persons.toJson.compactPrint // will be made 'stable' thus evaluated when initialized
  }

  object NotInializedRestService extends NotInitializedPersonRoute {
    override val personRepository: PersonRepository = new PersonRepository
  }

  "NotInializedRestService" should "initialize with null" in {
    a [NullPointerException] should be thrownBy NotInializedRestService.route
  }

  trait PersonRoute {
    def personRepository: PersonRepository
    def route: String = personRepository.persons.toJson.compactPrint
  }

  object RestService extends PersonRoute {
    override val personRepository: PersonRepository = new PersonRepository // def can be replaced with val
  }

  "RestService" should "initialize correctly" in {
    RestService.route shouldBe """[{"name":"Person20","age":20},{"name":"Person21","age":21},{"name":"Person22","age":22}]"""
  }
}
