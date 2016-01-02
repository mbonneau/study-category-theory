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
import org.scalamock.scalatest.MockFactory

/**
 * See: http://scalamock.org
 * See: http://scalamock.org/quick-start/
 * See: https://github.com/paulbutcher/ScalaMock
 * See: https://github.com/pawel-wiejacha/scalamock-examples
 */
class ScalaMockTest extends TestSpec with MockFactory {

  trait Country
  object Countries {
    object Germany extends Country
    object Russia extends Country
  }

  type PlayerId = Int
  case class Player(id: PlayerId, nickname: String, country: Country)
  case class MatchResult(winner: PlayerId, loser: PlayerId)

  case class CountryLeaderboardEntry(country: Country, points: Int)

  trait CountryLeaderboard {
    def addVictoryForCountry(country: Country): Unit
    def getTopCountries(): List[CountryLeaderboardEntry]
  }

  class MatchResultObserver(playerDatabase: PlayerDatabase, countryLeaderBoard: CountryLeaderboard) {
    def recordMatchResult(result: MatchResult): Unit = {
      val player = playerDatabase.getPlayerById(result.winner)
      countryLeaderBoard.addVictoryForCountry(player.country)
    }
  }

  trait PlayerDatabase {
    def getPlayerById(playerId: PlayerId): Player
  }

  /**
   * What is ScalaMock?
   * ==================
   * With ScalaMock you can create objects that pretend to implement some trait or interface.
   * Then you can instruct that “faked” object how it should respond to all interactions
   * with it. For example:
   */

  //  val winner = Player(id = 222, nickname = "boris", country = Countries.Russia)
  //  val loser = Player(id = 333, nickname = "hans", country = Countries.Germany)

  "ScalaMock" should "support record-then-verify style" in {
    // In the record-then-verify style, expectations are verified
    // after the system under test has executed. It uses the `stub` method

    // create fakeDb stub that implements PlayerDatabase trait
    val fakeDb = stub[PlayerDatabase]

    // configure fakeDb behavior
    fakeDb.getPlayerById _ when 222 returns Player(222, "boris", Countries.Russia)
    fakeDb.getPlayerById _ when 333 returns Player(333, "hans", Countries.Germany)

    // use fakeDb
    fakeDb.getPlayerById(222).nickname shouldBe "boris"
    fakeDb.getPlayerById(333).nickname shouldBe "hans"
  }

  trait PowerState
  object PowerStates {
    object On extends PowerState
    object Off extends PowerState
  }

  trait Heater {
    def isReady: Boolean
    def setPowerState(powerState: PowerState): Unit
  }

  case class Coffee(temp: Double)

  case class CoffeeMachine(heater: Heater) {
    def makeCoffee: Coffee = {
      heater.isReady
      heater.setPowerState(PowerStates.On)
      heater.setPowerState(PowerStates.Off)
      Coffee(65.0)
    }
  }

  "CoffeeMachine" should "turn off the heater after coffee making is finished" in {
    // A mock object that supports expectations-first style is created with the `mock` method.
    // The difference between the two is, that an expectations-first mock object expects to be used
    // a certain way. If that is not the case, the test will fail.

    val heaterMock = mock[Heater]
    val coffeeMachine = new CoffeeMachine(heaterMock)

    (heaterMock.isReady _).expects().returning(true) // it expects to be called with no arguments
    (heaterMock.setPowerState _).expects(PowerStates.On)
    (heaterMock.setPowerState _).expects(PowerStates.Off)

    coffeeMachine.makeCoffee
  }
}
