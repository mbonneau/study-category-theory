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

class FunctionCompositionTest extends TestSpec {

  "compose" should "compose two functions, evaluating g first with the value 'yay', then f as last evaluation" in {
    Given("A function named 'f'")
    //    def f(s: String): String = "f(" + s + ")"
    val f = (s: String) => "f(" + s + ")"
    And("A function named 'g'")
    //    def g(s: String): String = "g(" + s + ")"
    val g = (s: String) => "g(" + s + ")"

    When("The function 'f' and 'g' are composed using the combinator 'compose'")
    val fComposeGFunction = f.compose(g)

    And("The new function 'fComposeGFunction' will be applied with 'yay'")
    Then("function g will be evaluated first with the value 'yay', then the function f with the result of g('yay')")
    fComposeGFunction("yay") shouldBe "f(g(yay))"
  }

  it should "compose two methods by first expaning the method to functions (expanding the method to functions) and then composing them" in {
    Given("A method named 'f'")
    def f(s: String): String = "f(" + s + ")"
    And("Expanding that method to a function")
    val fExpanded: String => String = f _

    And("A method named 'g'")
    def g(s: String): String = "g(" + s + ")"
    val gExpanded: String => String = g _

    When("The function 'f' and 'g' are composed using the combinator 'compose'")
    val fComposeGFunction = fExpanded.compose(gExpanded)

    And("The new function 'fComposeGFunction' will be applied with 'yay'")
    Then("function g will be evaluated first with the value 'yay', then the function f with the result of g('yay')")
    fComposeGFunction("yay") shouldBe "f(g(yay))"
  }

  "andThen" should "compose two functions, evaluating f first with the value 'yay' and then g last" in {
    Given("A function named 'f'")
    val f = (s: String) => "f(" + s + ")"
    And("A function named 'g'")
    val g = (s: String) => "g(" + s + ")"

    When("The function 'f' and 'g' are composed using the combinator 'andThen'")
    val fAndThenGFunction = f.andThen(g)

    And("The new function 'fAndThenGFunction' will be applied with 'yay'")
    Then("function f will be evaluated first with the value 'yay', then the function g with the result of f('yay')")
    fAndThenGFunction("yay") shouldBe "g(f(yay))"
  }
}
