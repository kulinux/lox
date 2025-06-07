package com.lox

import cats.data.State

enum Tokens:
  case Var
  case Identifier(name: String)
  case Equal
  case Str(name: String)
  case Semicolon
  case Eof

type Token = Tokens

type Tokenized = Tokens | Seq[Tokens]

enum States:
  case Initial
  case Var
  case Identifier
  case Equal
  case Str
  case Semicolon
  case Error

class Lox:

  val change: String => State[States, Tokens] = (value: String) =>
    State { state =>
      (state, value) match
        case (States.Initial, "var")  => (States.Var, Tokens.Var)
        case (States.Var, _)          => (States.Identifier, Tokens.Identifier(value))
        case (States.Identifier, "=") => (States.Equal, Tokens.Equal)
        case (States.Equal, _)        => (States.Str, Tokens.Str(value.replace("\"", "")))
        case (States.Str, ";")        => (States.Semicolon, Tokens.Semicolon)
        case _                        => (States.Error, Tokens.Eof)
    }

  val changelist: List[String] => State[States, Seq[Tokens]] =
    (values: List[String]) =>
      values.foldLeft(State.pure[States, Seq[Tokens]](Seq.empty)) { (acc, value) =>
        acc.flatMap(tokens => change(value).map(tokens :+ _))
      }

  import Tokens._

  def tokenize(input: String): Tokenized =
    val stringTokens = input.split(" ").toList
    val res          = changelist.apply(stringTokens).runA(States.Initial).value

    res :+ Eof
