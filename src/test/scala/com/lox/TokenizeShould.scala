package com.lox

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Tokens._

class TokenizeShould extends AnyFreeSpec with Matchers {

  val lox = Lox()

  "several tokens" - {
    "var" in {
      val actual = lox.tokenize("var")

      actual shouldBe Seq(Var, Eof)
    }

    "var language" in {
      val actual = lox.tokenize("var language")

      actual shouldBe Seq(Var, Identifier("language"), Eof)
    }
  }

  "variable assignment" in {
    val actual = lox.tokenize("var language = \"lox\" ;")

    val expected =
      Seq(
        Var,
        Identifier("language"),
        Equal,
        Str("lox"),
        Semicolon,
        Eof
      )

    actual.shouldBe(expected)
  }

  "Parenthesis" in {
    val actual = lox.tokenize("( ( )")

    val expected =
      Seq(
        LeftParen,
        LeftParen,
        RightParen,
        Eof
      )

    actual shouldBe expected
  }
}
