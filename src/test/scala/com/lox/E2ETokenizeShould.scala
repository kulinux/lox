package com.lox

import cats.Show
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show._

import Tokens._

given showIdentifier: Show[Identifier] with
  def show(token: Identifier): String = "VAR var null"

given showStrToken: Show[Str] with
  def show(token: Str): String = "VAR var null"

given showAllTokens: Show[Tokens] with
    def show(tokens: Tokens): String = tokens match
      case v: Var.type => "var"
      case id: Identifier => id.show
      case e: Equal.type => "equal"
      case str: Str => str.show
      case sc: Semicolon.type => "semicolon"
      case eof: Eof.type => "eof"

given showTokenized: Show[Tokenized] with
  def show(tokenized: Tokenized): String = tokenized match
    case token: Token => "Token"
    case seqOfTokens: Seq[Token] => seqOfTokens.show


class E2ETokenizeShould extends AnyFeatureSpec with GivenWhenThen with Matchers {
  Feature("tokenize") {
    Scenario("assigmnment") {
      Given("A line and a lox")
      val line = "var language = \"lox\";"
      val lox = Lox()

      When("tokenize")
      val actual = lox.tokenize(line)
      Then("I have an string representation")

      val expected =
        """VAR var null
          |IDENTIFIER language null
          |EQUAL = null
          |STRING "lox" lox
          |SEMICOLON ; null
          |EOF  null""".stripMargin

      actual.show shouldBe expected
    }
  }
}