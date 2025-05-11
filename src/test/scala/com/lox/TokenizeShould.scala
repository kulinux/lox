package com.lox

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Tokens._

class TokenizeShould extends AnyFreeSpec with Matchers {

  val lox = Lox()

  "variable assignment" in {
    val actual = lox.tokenize("var language = \"lox\";")

    val expected =
      Seq(
        Var,
        Identifier,
        Equal,
        Str("lox"),
        Semicolon,
        Eof
      )

    actual.shouldBe(expected)
  }
}
