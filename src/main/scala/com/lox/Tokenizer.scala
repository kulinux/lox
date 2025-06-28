package com.lox

class Tokenizer:
  def tokenize(input: String): List[String] =
    input.split(" ").toList
