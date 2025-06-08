package com.lox

import cats.data.State
import cats.syntax.traverse._ // para traverse
import cats.instances.list._

enum Tokens:
  case Var
  case Identifier(name: String)
  case Equal
  case Str(name: String)
  case Semicolon
  case LeftParen
  case RightParen
  case Eof

type Token = Tokens

type Tokenized = Tokens | Seq[Tokens]

sealed trait AllStates

enum StatesCommon extends AllStates:
  case Initial
  case Eof
  case Error

enum StatesVar extends AllStates:
  case Var
  case Identifier
  case Equal
  case Str
  case Semicolon

enum StatesParen extends AllStates:
  case None

class Lox:

  def changeVar(value: String): State[AllStates, Tokens] =
    State { state =>
      (state, value) match
        case (StatesCommon.Initial, "var") => (StatesVar.Var, Tokens.Var)
        case (StatesVar.Var, _)            => (StatesVar.Identifier, Tokens.Identifier(value))
        case (StatesVar.Identifier, "=")   => (StatesVar.Equal, Tokens.Equal)
        case (StatesVar.Equal, _)          => (StatesVar.Str, Tokens.Str(value.replace("\"", "")))
        case (StatesVar.Str, ";")          => (StatesVar.Semicolon, Tokens.Semicolon)
        case _                             => (StatesCommon.Error, Tokens.Eof)
    }

  def changeParen(value: String): State[AllStates, Tokens] =
    State { state =>
      (state, value) match
        case (_, "(") => (StatesCommon.Initial, Tokens.LeftParen)
        case (_, ")") => (StatesCommon.Initial, Tokens.RightParen)
        case _        => (state, Tokens.Eof)
    }

  def changeList(changeItem: String => State[AllStates, Tokens])(values: List[String]): State[AllStates, List[Tokens]] =
    values.traverse(changeItem)

  val change: List[String] => State[AllStates, List[Tokens]] =
    (values: List[String]) =>
      values.head match
        case "(" => changeList(changeParen)(values)
        case _   => changeList(changeVar)(values)

  import Tokens._

  def tokenize(input: String): Tokenized =
    val stringTokens = input.split(" ").toList
    val res          = change.apply(stringTokens).runA(StatesCommon.Initial).value

    res :+ Eof
