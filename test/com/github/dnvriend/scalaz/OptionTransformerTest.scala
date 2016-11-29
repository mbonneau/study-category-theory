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

package com.github.dnvriend.scalaz

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._
import scalaz.OptionT._
import scala.concurrent.Future

class OptionTransformerTest extends TestSpec {

  def fut[A](a: Option[A]): Future[Option[A]] = Future.successful(a)

  it should "shortcircuit on a" in {
    val f: Future[Option[Int]] = (for {
      a <- optionT(Future.successful(Option.empty[Int]))
      b <- optionT(Future.successful(2.some))
      c <- optionT(Future.successful(3.some))
    } yield a + b + c).run
    f.futureValue should not be 'defined
  }

  it should "shortcircuit on b" in {
    val f: Future[Option[Int]] = (for {
      a <- optionT(Future.successful(1.some))
      b <- optionT(Future.successful(Option.empty[Int]))
      c <- optionT(Future.successful(3.some))
    } yield a + b + c).run
    f.futureValue should not be 'defined
  }

  it should "shortcircuit on c" in {
    val f: Future[Option[Int]] = (for {
      a <- optionT(Future.successful(1.some))
      b <- optionT(Future.successful(2.some))
      c <- optionT(Future.successful(Option.empty[Int]))
    } yield a + b + c).run
    f.futureValue should not be 'defined
  }

  it should "compose a + b + c" in {
    val f: Future[Option[Int]] = (for {
      a <- optionT(Future.successful(1.some))
      b <- optionT(Future.successful(2.some))
      c <- optionT(Future.successful(3.some))
    } yield a + b + c).run
    f.futureValue.value shouldBe 6
  }

  case class Address(addressId: Long = 1)
  case class Person(address: Option[Address] = Address().some)
  class PersonRepository(find: Boolean) {
    def findPerson(personId: Long): Future[Option[Person]] = Future.successful {
      if (find) Person().some else None
    }
  }

  case class Country(code: Option[String] = "NL".some)
  class CountryRepository(find: Boolean) {
    def findCountry(countryId: Long): Future[Option[Country]] = Future.successful {
      if (find) Country().some else None
    }
  }

  /**
   * Note, only Future[Option] can be composed
   */
  class DetermineCountryCodeService(personRepository: PersonRepository, countryRepository: CountryRepository) {
    def determineCountryCode(personId: Long): Future[Option[String]] = {
      val result: OptionT[Future, String] = for {
        person <- optionT(personRepository.findPerson(1))
        address <- optionT(Future.successful(person.address))
        country <- optionT(countryRepository.findCountry(address.addressId))
        code <- optionT(Future.successful(country.code))
      } yield code
      result.run
    }
  }

  def withService(findPerson: Boolean = true, findCountry: Boolean = true)(f: DetermineCountryCodeService => Unit): Unit = {
    f(new DetermineCountryCodeService(new PersonRepository(findPerson), new CountryRepository(findCountry)))
  }

  "Determine Country Code Service" should "compose when person and country are found" in withService() { service =>
    service.determineCountryCode(1).futureValue.value shouldBe "NL"
  }

  it should "not compose when no person could be found" in withService(findPerson = false) { service =>
    service.determineCountryCode(1).futureValue should not be 'defined
  }

  it should "not compose when no country could be found" in withService(findCountry = false) { service =>
    service.determineCountryCode(1).futureValue should not be 'defined
  }
}