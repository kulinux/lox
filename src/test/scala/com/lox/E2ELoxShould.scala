package com.lox

import cats.Show
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show._

import Tokens._

given showIdentifier: Show[Identifier] with
  def show(token: Identifier): String = "IDENTIFIER language null"

given showStrToken: Show[Str] with
  def show(token: Str): String = "STRING \"lox\" lox"

given showAllTokens: Show[Tokens] with
  def show(tokens: Tokens): String = tokens match
    case v: Var.type        => "VAR var null"
    case id: Identifier     => id.show
    case e: Equal.type      => "EQUAL = null"
    case str: Str           => str.show
    case sc: Semicolon.type => "SEMICOLON ; null"
    case v: LeftParen.type  => "LEFT_PAREN ( null"
    case v: RightParen.type => "RIGHT_PAREN ) null"
    case v: LeftBrace.type  => "LEFT_BRACE ( null"
    case v: RightBrace.type => "RIGHT_BRACE ) null"
    case eof: Eof.type      => "EOF null"

given showTokenized: Show[Tokenized] with
  def show(tokenized: Tokenized): String = tokenized match
    case token: Token            => "Token"
    case seqOfTokens: Seq[Token] => seqOfTokens.map(_.show).mkString("\n")

class E2ELoxShould extends AnyFeatureSpec with GivenWhenThen with Matchers {
  Feature("tokenize") {
    Scenario("assigmnment") {
      Given("A line and a lox")
      val line = "var language = \"lox\" ;"
      val lox  = Lox()

      When("tokenize")
      val actual = lox.tokenize(line)

      Then("I have an string representation")
      val expected =
        """VAR var null
          |IDENTIFIER language null
          |EQUAL = null
          |STRING "lox" lox
          |SEMICOLON ; null
          |EOF null""".stripMargin

      actual.show shouldBe expected
    }

    Scenario("Parenthesis") {
      Given("A line and a lox")
      val line = "(()"
      val lox  = Lox()

      When("tokenize")
      val actual = lox.tokenize(line)

      Then("I have an string representation")
      val expected =
        """LEFT_PAREN ( null
          |LEFT_PAREN ( null
          |RIGHT_PAREN ) null
          |EOF null""".stripMargin

      actual.show shouldBe expected

    }

    Scenario("Braces") {
      Given("A line and a lox")
      val line = "{{}}"
      val lox  = Lox()

      When("tokenize")
      val actual = lox.tokenize(line)

      Then("I have an string representation")
      val expected =
        """LEFT_BRACE ( null
          |LEFT_BRACE ( null
          |RIGHT_BRACE ) null
          |RIGHT_BRACE ) null
          |EOF null""".stripMargin

      actual.show shouldBe expected

    }
  }
}
