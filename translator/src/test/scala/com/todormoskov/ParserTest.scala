package com.todormoskov

import com.todormoskov.Lexer.*

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  // TRUE
  test("Parses the literal TRUE") {
    val inputSpec =
      """TRUE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseLiteral().toOption.get
    assert(ast == Literal(true))
  }

  // FALSE
  test("Parses the literal FALSE") {
    val inputSpec =
      """FALSE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseLiteral().toOption.get
    assert(ast == Literal(false))
  }

  // BOOLEAN
  test("Parses the literal BOOLEAN") {
    val inputSpec =
      """BOOLEAN""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseLiteral().toOption.get
    assert(ast == Literal("BOOLEAN"))
  }

  // Integers
  test("Parses integer literals") {
    val inputSpec =
      """1""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseLiteral().toOption.get
    assert(ast == Literal(1))
  }

  // String
  test("Parses string literals") {
    val inputSpec =
      """ "someString" """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseLiteral().toOption.get
    assert(ast == Literal("someString"))
  }

  // Identifiers
  test("Parses identifiers") {
    val inputSpec =
      """foo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseIdentifier().toOption.get
    assert(ast == Identifier("foo"))
  }

  // EXTENDS statements
  test("Parses EXTENDS statements") {
    val inputSpec =
      """EXTENDS foo, bar""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExtendsStatement().toOption.get
    assert(ast == ExtendsStatement(List("foo", "bar")))
  }

  test("Parses CONSTANT declarations") {
    val inputSpec =
      """CONSTANTS X, Y, Z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseConstantDeclaration().toOption.get
    assert(ast == ConstantDeclaration(List("X", "Y", "Z")))
  }

  test("Parses VARIABLE declarations") {
    val inputSpec =
      """VARIABLES x, y, z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseVariableDeclaration().toOption.get
    assert(ast == VariableDeclaration(List("x", "y", "z")))
  }

  test("Parses operator applications") {
    val inputSpec =
      """foo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Identifier("foo"))
  }

  test("Parses operator applications with arguments") {
    val inputSpec =
      """foo(bar(1,2,3))""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      OperatorApplication("foo",
        List(OperatorApplication("bar",
          List(
            Literal(1),
            Literal(2),
            Literal(3))))))
  }

  test("Parses function applications with arguments") {
    val inputSpec =
      """foo[bar[1, 2, 3], "ABC"]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      FunctionApplication(
        Identifier("foo"),
        List(
          FunctionApplication(
            Identifier("bar"),
            List(
              Literal(1),
              Literal(2),
              Literal(3))),
          Literal("ABC"))
      ))
  }

  test("Parses multiple function applications") {
    val inputSpec =
      """foo[1][2]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      FunctionApplication(
        FunctionApplication(
          Identifier("foo"),
          List(Literal(1))),
        List(Literal(2))
      ))
  }

  test("Parses an empty set") {
    val inputSpec =
      """{ }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExplicitSet(
        List()))
  }

  test("Parses one-element set") {
    val inputSpec =
      """{ 1 }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExplicitSet(
        List(
          Literal(1)
        )))
  }

  test("Parses an explicitly given set") {
    val inputSpec =
      """{ 1, "ABC", foo[bar] }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExplicitSet(
        List(
          Literal(1),
          Literal("ABC"),
          FunctionApplication(
            Identifier("foo"),
            List(Identifier("bar"))))))
  }

  test("Parses a set given by a predicate") {
    val inputSpec =
      """{ n \in Nat : TRUE }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      FilteredSet(
        Identifier("n"),
        Identifier("Nat"),
        Literal(true)))
  }

  test("Parses a set given by a mapping expression") {
    val inputSpec =
      """{ n : n \in Nat }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      MappedSet(
        Identifier("n"),
        Identifier("n"),
        Identifier("Nat")))
  }

  /*
  private def parse(input: String): Either[String, Module] = {
    val tokenizer = Tokenizer(Scanner(input))
    val parser = new Parser(tokenizer)
    parser.parseModule()
  }

  // Module-level constructs
  test("Parses the beginning and the end of a module") {
    val inputSpec =
      """---- MODULE M ----
        |----
        |====
        |""".stripMargin

    val module = parse(inputSpec).toOption.get
    assert(module == Module("M", ExtendsStatement(Nil), Nil))
  }
  */
}
