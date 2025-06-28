package com.lox

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TokenizerShould extends AnyFreeSpec with Matchers {

  "tokenize" - {
    "empty input" in {
      val tokenizer = Tokenizer()
      val actual    = tokenizer.tokenize("")
      actual shouldBe List()
    }

    "single token" in {
      val tokenizer = Tokenizer()
      val actual    = tokenizer.tokenize("var")
      actual shouldBe List("var")
    }

    "multiple tokens" in {
      val tokenizer = Tokenizer()
      val actual    = tokenizer.tokenize("var language = \"lox\" ;")
      actual shouldBe List("var", "language", "=", "\"lox\"", ";")
    }

    "parenthesis" in {
      val tokenizer = Tokenizer()
      val actual    = tokenizer.tokenize("(()")
      actual shouldBe List("(", "(", ")")
    }

    "braces" in {
      val tokenizer = Tokenizer()
      val actual    = tokenizer.tokenize("{}")
      actual shouldBe List("{", "}")
    }
  }
}
