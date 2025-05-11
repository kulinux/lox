package com.lox


enum Tokens:
  case Var
  case Identifier(name: String)
  case Equal
  case Str(name: String)
  case Semicolon
  case Eof


type Token = Tokens

type Tokenized = Tokens | Seq[Tokens]

class Lox:

  import Tokens._


  def tokenize(input: String): Tokenized = Seq(
    Var,
    Equal,
    Str("lox"),
    Semicolon,
    Eof
  )

