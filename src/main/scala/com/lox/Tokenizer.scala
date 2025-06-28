package com.lox

class Tokenizer:
  def tokenize(input: String): List[String] =
    if input.isEmpty then return List()

    val anyWord  = """\w+"""
    val inQuotes = """"[^"]*""""
    val specials = """[\(\)=;{}]"""

    val pattern = s"""${anyWord}|${inQuotes}|${specials}""".r

    pattern
      .findAllIn(input)
      .toList
