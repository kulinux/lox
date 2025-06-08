package com.lox

import cats.data.State
import cats.syntax.functor._

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

sealed trait StatesFoo

enum StatesVar extends StatesFoo:
  case Initial
  case Var
  case Identifier
  case Equal
  case Str
  case Semicolon
  case Error

enum StatesParen extends StatesFoo:
  case Initial

class Lox:

  val changeVar: String => State[StatesFoo, Tokens] = (value: String) =>
    State { state =>
      (state, value) match
        case (StatesVar.Initial, "var")  => (StatesVar.Var, Tokens.Var)
        case (StatesVar.Var, _)          => (StatesVar.Identifier, Tokens.Identifier(value))
        case (StatesVar.Identifier, "=") => (StatesVar.Equal, Tokens.Equal)
        case (StatesVar.Equal, _)        => (StatesVar.Str, Tokens.Str(value.replace("\"", "")))
        case (StatesVar.Str, ";")        => (StatesVar.Semicolon, Tokens.Semicolon)
        case _                           => (StatesVar.Error, Tokens.Eof)
    }

  val changeParen: String => State[StatesFoo, Tokens] = (value: String) =>
    State { state =>
      (state, value) match
        case (_, "(") => (StatesParen.Initial, Tokens.LeftParen)
        case (_, ")") => (StatesParen.Initial, Tokens.RightParen)
        case _        => (state, Tokens.Eof)
    }

  val changeVarList: List[String] => State[StatesFoo, Seq[Tokens]] =
    (values: List[String]) =>
      values.foldLeft(State.pure[StatesFoo, Seq[Tokens]](Seq.empty)) { (acc, value) =>
        acc.flatMap(tokens => changeVar(value).map(tokens :+ _))
      }

  val changeParenList: List[String] => State[StatesFoo, Seq[Tokens]] =
    (values: List[String]) =>
      values.foldLeft(State.pure[StatesFoo, Seq[Tokens]](Seq.empty)) { (acc, value) =>
        acc.flatMap(tokens => changeParen(value).map(tokens :+ _))
      }

  val change: List[String] => State[StatesFoo, Seq[Tokens]] =
    (values: List[String]) =>
      values.head match
        case "(" => changeParenList(values)
        case _   => changeVarList(values)

  import Tokens._

  def tokenize(input: String): Tokenized =
    val stringTokens = input.split(" ").toList
    val res          = change.apply(stringTokens).runA(StatesVar.Initial).value

    res :+ Eof
