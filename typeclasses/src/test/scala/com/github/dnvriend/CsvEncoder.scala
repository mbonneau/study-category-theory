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

package com.github.dnvriend

import scalaz._
import Scalaz._
import simulacrum._

object CsvEncoder {
  implicit val csvEncoderPerson = new CsvEncoder[Person] {
    def encode(person: Person): String =
      s"${person.name},${person.age}"
  }
}

@typeclass trait CsvEncoder[A] {
  def encode(value: A): String
}
