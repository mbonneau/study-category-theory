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

class ExpressionOrientedProgrammingTest extends TestSpec {

  /**
   * In Java, we are tought/used to writing statements (Statement Oriented Programming).
   * This is a style of programming in which the statements do not return results and are
   * executed solely for their side effects. For example order.calculateTaxes(), order.updatePrices().
   * Because it's a style of programming, every programming language supports it, also Scala does.
   * Most of the time Statement Oriented Programming is applied using JEE, in which some kind of Enterprise
   * Bean, of course completely designed and modeled using OO concepts and drawn with pretty pictures using UML
   * in combination with Domain Driven Design in which the objects just 'do stuff'; it is side-effect heaven
   * basically.
   */

  case class Order(priceWithoutTax: Double, taxPercentage: Double) {
    def calculateTaxes(): Unit = {
      (priceWithoutTax / 100) * taxPercentage
      println(s"The tax is: $taxPercentage")
    }

    def updatePrices(): Unit = println("Updating prices in the database, please wait...")
  }

  "Scala" should "support statement oriented programming" in {
    val order = Order(100.0, 21.0)
    order.calculateTaxes()
    order.updatePrices()
  }

  /**
   * Expression Oriented Programming is a style of programming in which we write expressions and not statements.
   * Expressions always return a value and (often) do not have side effects at all. Pure functions for example have
   * no side effect whatsoever. Examples of expressions are:
   *
   * val tax = calculateTax(order)
   * val price = calculatePrice(order)
   *
   * To support Expression Oriented Programming, a programming language must support returning a value for
   * (nearly every) programming construct. Every functional programming language is expression oriented so a language
   * like Clojure is expression oriented. Because Scala supports the functional style of programming (but is a
   * pure object oriented programming language at heart) it also supports Expression Oriented Programming because
   * almost every language construct returns a value; which is great!
   *
   * For example a block of code is an expression, if statements, try-catch statements, for expressions, pattern match
   * blocks, etc.
   *
   * So to recap; an `instruction` gets executed (for its side effect), an `expression` gets evaluated to a value.
   */

  // another benefit of expression oriented programming; composing functions, which is a
  // great way of controlling complexity (and reuse) of logic.
  def calculatePrice(order: Order): Double = order.priceWithoutTax + calculateTax(order)

  def calculateTax(order: Order): Double = (order.priceWithoutTax / 100) * order.taxPercentage

  "Scala" should "support expression oriented programming" in {
    val order = Order(100.0, 21.0)
    calculatePrice(order) shouldBe 121.0
  }
}
