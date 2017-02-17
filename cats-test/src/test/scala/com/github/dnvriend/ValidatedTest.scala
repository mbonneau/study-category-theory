package com.github.dnvriend

import cats.data.Validated.{Invalid, Valid}
import mouse.string._

class ValidatedTest extends TestSpec {
  it should "validate int" in {
    "1".parseIntValidated shouldBe Valid(1)
    "foo".parseIntValidated shouldBe Invalid("")
  }
}
