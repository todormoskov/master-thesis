package com.todormoskov

import com.todormoskov.Lexer.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class LexerTest extends AnyFunSuite {

  private def recognizeTokens(input: String): List[Token] = {
    val tokenizer = Tokenizer(Scanner(input))
    val tokens = ListBuffer[Token]()
    while (tokenizer.peek().asInstanceOf[Token].kind != TokenKind.EOF) {
      tokens.addOne(tokenizer.consume())
    }
    tokens.addOne(tokenizer.consume())
    tokens.toList
  }

  // Module-level constructs
  test("Recognizes the beginning and the end of a module") {
    val inputSpec =
      """---- MODULE M ----
        |----
        |====
        |""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.MINUS_SEPARATOR, "----", 1, 1),
        Token(TokenKind.MODULE, "MODULE", 1 , 6),
        Token(TokenKind.IDENTIFIER, "M", 1, 13),
        Token(TokenKind.MINUS_SEPARATOR, "----", 1, 15),
        Token(TokenKind.MINUS_SEPARATOR, "----", 2, 1),
        Token(TokenKind.EQUAL_SEPARATOR, "====", 3, 1),
        Token(TokenKind.EOF, null, 4, 1)
      )
    )
  }

  test("Recognizes constant declarations") {
    val inputSpec =
      """CONSTANTS X, Y, Z""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CONSTANTS, "CONSTANTS", 1, 1),
        Token(TokenKind.IDENTIFIER, "X", 1, 11),
        Token(TokenKind.COMMA, ",", 1, 12),
        Token(TokenKind.IDENTIFIER, "Y", 1, 14),
        Token(TokenKind.COMMA, ",", 1, 15),
        Token(TokenKind.IDENTIFIER, "Z", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes variable declarations") {
    val inputSpec =
      """VARIABLES x, y, z""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.VARIABLES, "VARIABLES", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 11),
        Token(TokenKind.COMMA, ",", 1, 12),
        Token(TokenKind.IDENTIFIER, "y", 1, 14),
        Token(TokenKind.COMMA, ",", 1, 15),
        Token(TokenKind.IDENTIFIER, "z", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes assumptions") {
    val inputSpec =
      """ASSUME X > 0""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.ASSUME, "ASSUME", 1, 1),
        Token(TokenKind.IDENTIFIER, "X", 1, 8),
        Token(TokenKind.GT, ">", 1, 10),
        Token(TokenKind.INTEGER, "0", 1, 12),
        Token(TokenKind.EOF, null, 1, 13)
      )
    )
  }

  test("Recognizes operator definitions") {
    val inputSpec =
      """F(x, y) == x + y""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "F", 1, 1),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 2),
        Token(TokenKind.IDENTIFIER, "x", 1, 3),
        Token(TokenKind.COMMA, ",", 1, 4),
        Token(TokenKind.IDENTIFIER, "y", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.DEFINE, "==", 1, 9),
        Token(TokenKind.IDENTIFIER, "x", 1, 12),
        Token(TokenKind.PLUS, "+", 1 ,14),
        Token(TokenKind.IDENTIFIER, "y", 1, 16),
        Token(TokenKind.EOF, null, 1, 17)
      )
    )
  }

  test("Recognizes function definitions") {
    val inputSpec =
      """successor[n \in Nat] == n + 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "successor", 1, 1),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 10),
        Token(TokenKind.IDENTIFIER, "n", 1, 11),
        Token(TokenKind.SET_IN, "\\in", 1, 13),
        Token(TokenKind.IDENTIFIER, "Nat", 1, 17),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 20),
        Token(TokenKind.DEFINE, "==", 1, 22),
        Token(TokenKind.IDENTIFIER, "n", 1, 25),
        Token(TokenKind.PLUS, "+", 1, 27),
        Token(TokenKind.INTEGER, "1", 1, 29),
        Token(TokenKind.EOF, null, 1, 30)
      )
    )
  }

  test("Recognizes instance statements") {
    val inputSpec =
      """INSTANCE M WITH X <- 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INSTANCE, "INSTANCE", 1, 1),
        Token(TokenKind.IDENTIFIER, "M", 1, 10),
        Token(TokenKind.WITH, "WITH", 1, 12),
        Token(TokenKind.IDENTIFIER, "X", 1, 17),
        Token(TokenKind.LEFT_ARROW, "<-", 1, 19),
        Token(TokenKind.INTEGER, "1", 1, 22),
        Token(TokenKind.EOF, null, 1, 23)
      )
    )
  }

  test("Recognizes theorems") {
    val inputSpec =
      """THEOREM SuccessorReturnsTheNextNaturalNumber == \A n \in Nat : successor[n] = n + 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.THEOREM, "THEOREM", 1, 1),
        Token(TokenKind.IDENTIFIER, "SuccessorReturnsTheNextNaturalNumber", 1, 9),
        Token(TokenKind.DEFINE, "==", 1, 46),
        Token(TokenKind.FORALL, "\\A", 1, 49),
        Token(TokenKind.IDENTIFIER, "n", 1, 52),
        Token(TokenKind.SET_IN, "\\in", 1, 54),
        Token(TokenKind.IDENTIFIER, "Nat", 1, 58),
        Token(TokenKind.COLON, ":", 1, 62),
        Token(TokenKind.IDENTIFIER, "successor", 1, 64),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 73),
        Token(TokenKind.IDENTIFIER, "n", 1, 74),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 75),
        Token(TokenKind.EQ, "=", 1, 77),
        Token(TokenKind.IDENTIFIER, "n", 1 ,79),
        Token(TokenKind.PLUS, "+", 1, 81),
        Token(TokenKind.INTEGER, "1", 1, 83),
        Token(TokenKind.EOF, null, 1, 84)
      )
    )
  }

  test("Recognizes the logical operators") {
    val inputSpec =
      """/\ \/ ~ => <=>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.AND, "/\\", 1, 1),
        Token(TokenKind.OR, "\\/", 1, 4),
        Token(TokenKind.NOT, "~", 1, 7),
        Token(TokenKind.IMPLICATION, "=>", 1, 9),
        Token(TokenKind.EQUIVALENCE, "<=>", 1, 12),
        Token(TokenKind.EOF, null, 1, 15)
      )
    )
  }

  test("Recognizes the symbols TRUE, FALSE, BOOLEAN") {
    val inputSpec =
      """TRUE FALSE BOOLEAN""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.TRUE, "TRUE", 1, 1),
        Token(TokenKind.FALSE, "FALSE", 1, 6),
        Token(TokenKind.BOOLEAN, "BOOLEAN", 1, 12),
        Token(TokenKind.EOF, null, 1, 19)
      )
    )
  }

  test("Recognizes forall expressions") {
    val inputSpec =
      """\A x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.FORALL, "\\A", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 4),
        Token(TokenKind.SET_IN, "\\in", 1, 6),
        Token(TokenKind.IDENTIFIER, "S", 1, 10),
        Token(TokenKind.COLON, ":", 1, 12),
        Token(TokenKind.IDENTIFIER, "P", 1, 14),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 15),
        Token(TokenKind.IDENTIFIER, "x", 1, 16),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes exists expressions") {
    val inputSpec =
      """\E x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EXISTS, "\\E", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 4),
        Token(TokenKind.SET_IN, "\\in", 1, 6),
        Token(TokenKind.IDENTIFIER, "S", 1, 10),
        Token(TokenKind.COLON, ":", 1, 12),
        Token(TokenKind.IDENTIFIER, "P", 1, 14),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 15),
        Token(TokenKind.IDENTIFIER, "x", 1, 16),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes CHOOSE expressions") {
    val inputSpec =
      """CHOOSE x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CHOOSE, "CHOOSE", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 8),
        Token(TokenKind.SET_IN, "\\in", 1, 10),
        Token(TokenKind.IDENTIFIER, "S", 1, 14),
        Token(TokenKind.COLON, ":", 1, 16),
        Token(TokenKind.IDENTIFIER, "P", 1, 18),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 19),
        Token(TokenKind.IDENTIFIER, "x", 1, 20),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 21),
        Token(TokenKind.EOF, null, 1, 22)
      )
    )
  }

  test("Recognizes the set operators") {
    val inputSpec =
      """= # /= \in \notin \cup \union \cap \intersect \subset \""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EQ, "=", 1, 1),
        Token(TokenKind.NEQ, "#", 1, 3),
        Token(TokenKind.NEQ, "/=", 1, 5),
        Token(TokenKind.SET_IN, "\\in", 1, 8),
        Token(TokenKind.NOTIN, "\\notin", 1, 12),
        Token(TokenKind.UNION, "\\cup", 1, 19),
        Token(TokenKind.UNION, "\\union", 1, 24),
        Token(TokenKind.INTERSECT, "\\cap", 1, 31),
        Token(TokenKind.INTERSECT, "\\intersect", 1, 36),
        Token(TokenKind.SUBSET, "\\subset", 1, 47),
        Token(TokenKind.SET_DIFFERENCE, "\\", 1, 55),
        Token(TokenKind.EOF, null, 1, 56)
      )
    )
  }

  test("Recognizes set constructor with all elements") {
    val inputSpec =
      """{1, 2, 3}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 1),
        Token(TokenKind.INTEGER, "1", 1, 2),
        Token(TokenKind.COMMA, ",", 1, 3),
        Token(TokenKind.INTEGER, "2", 1, 5),
        Token(TokenKind.COMMA, ",", 1, 6),
        Token(TokenKind.INTEGER, "3", 1, 8),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 9),
        Token(TokenKind.EOF, null, 1, 10)
      )
    )
  }

  test("Recognizes set constructor with 'such that'") {
    val inputSpec =
      """{ x \in S : p}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 3),
        Token(TokenKind.SET_IN, "\\in", 1, 5),
        Token(TokenKind.IDENTIFIER, "S", 1, 9),
        Token(TokenKind.COLON, ":", 1, 11),
        Token(TokenKind.IDENTIFIER, "p", 1, 13),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 14),
        Token(TokenKind.EOF, null, 1, 15)
      )
    )
  }

  test("Recognizes set constructor with expressions for the different elements") {
    val inputSpec =
      """{ e : x \in S}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 1),
        Token(TokenKind.IDENTIFIER, "e", 1, 3),
        Token(TokenKind.COLON, ":", 1, 5),
        Token(TokenKind.IDENTIFIER, "x", 1, 7),
        Token(TokenKind.SET_IN, "\\in", 1, 9),
        Token(TokenKind.IDENTIFIER, "S", 1, 13),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 14),
        Token(TokenKind.EOF, null, 1, 15)
      )
    )
  }

  test("Recognizes the set of all subsets") {
    val inputSpec =
      """SUBSET S""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.BIG_SUBSET, "SUBSET", 1, 1),
        Token(TokenKind.IDENTIFIER, "S", 1, 8),
        Token(TokenKind.EOF, null, 1, 9)
      )
    )
  }

  test("Recognizes the union of all elements of a set") {
    val inputSpec =
      """UNION S""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.BIG_UNION, "UNION", 1, 1),
        Token(TokenKind.IDENTIFIER, "S", 1, 7),
        Token(TokenKind.EOF, null, 1, 8)
      )
    )
  }

  test("Recognizes function application") {
    val inputSpec =
      """f[e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "f", 1, 1),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 2),
        Token(TokenKind.IDENTIFIER, "e", 1, 3),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 4),
        Token(TokenKind.EOF, null, 1, 5)
      )
    )
  }

  test("Recognizes the DOMAIN keyword") {
    val inputSpec =
      """DOMAIN f""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.DOMAIN, "DOMAIN", 1, 1),
        Token(TokenKind.IDENTIFIER, "f", 1, 8),
        Token(TokenKind.EOF, null, 1, 9)
      )
    )
  }

  test("Recognizes function constructor with 'such that'") {
    val inputSpec =
      """[ x \in S |-> e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 3),
        Token(TokenKind.SET_IN, "\\in", 1, 5),
        Token(TokenKind.IDENTIFIER, "S", 1, 9),
        Token(TokenKind.MAPS_TO, "|->", 1, 11),
        Token(TokenKind.IDENTIFIER, "e", 1, 15),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 16),
        Token(TokenKind.EOF, null, 1, 17)
      )
    )
  }

  test("Recognizes the set of all functions from S to T") {
    val inputSpec =
      """[S -> T]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "S", 1, 2),
        Token(TokenKind.RIGHT_ARROW, "->", 1, 4),
        Token(TokenKind.IDENTIFIER, "T", 1, 7),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 8),
        Token(TokenKind.EOF, null, 1, 9)
      )
    )
  }

  test("Recognizes the function updates with EXCEPT") {
    val inputSpec =
      """[f EXCEPT ![e1]=e2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "f", 1, 2),
        Token(TokenKind.EXCEPT, "EXCEPT", 1, 4),
        Token(TokenKind.BANG, "!", 1, 11),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 12),
        Token(TokenKind.IDENTIFIER, "e1", 1, 13),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 15),
        Token(TokenKind.EQ, "=", 1, 16),
        Token(TokenKind.IDENTIFIER, "e2", 1, 17),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 19),
        Token(TokenKind.EOF, null, 1, 20)
      )
    )
  }

  test("Recognizes the dot notation for records") {
    val inputSpec =
      """e.h""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "e", 1, 1),
        Token(TokenKind.DOT, ".", 1, 2),
        Token(TokenKind.IDENTIFIER, "h", 1, 3),
        Token(TokenKind.EOF, null, 1, 4)
      )
    )
  }

  test("Recognizes the 'maps to' notation for records") {
    val inputSpec =
      """[h1 |-> e1, h2 |-> e2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "h1", 1, 2),
        Token(TokenKind.MAPS_TO, "|->", 1, 5),
        Token(TokenKind.IDENTIFIER, "e1", 1, 9),
        Token(TokenKind.COMMA, ",", 1, 11),
        Token(TokenKind.IDENTIFIER, "h2", 1, 13),
        Token(TokenKind.MAPS_TO, "|->", 1, 16),
        Token(TokenKind.IDENTIFIER, "e2", 1, 20),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 22),
        Token(TokenKind.EOF, null, 1, 23)
      )
    )
  }

  test("Recognizes the set of all records with h_i's in S_i's") {
    val inputSpec =
      """[h1 : S1, h2 : S2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "h1", 1, 2),
        Token(TokenKind.COLON, ":", 1, 5),
        Token(TokenKind.IDENTIFIER, "S1", 1, 7),
        Token(TokenKind.COMMA, ",", 1, 9),
        Token(TokenKind.IDENTIFIER, "h2", 1, 11),
        Token(TokenKind.COLON, ":", 1, 14),
        Token(TokenKind.IDENTIFIER, "S2", 1, 16),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 18),
        Token(TokenKind.EOF, null, 1, 19)
      )
    )
  }

  test("Recognizes the record updates with EXCEPT") {
    val inputSpec =
      """[r EXCEPT !.h = e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "r", 1, 2),
        Token(TokenKind.EXCEPT, "EXCEPT", 1, 4),
        Token(TokenKind.BANG, "!", 1, 11),
        Token(TokenKind.DOT, ".", 1, 12),
        Token(TokenKind.IDENTIFIER, "h", 1, 13),
        Token(TokenKind.EQ, "=", 1, 15),
        Token(TokenKind.IDENTIFIER, "e", 1, 17),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 18),
        Token(TokenKind.EOF, null, 1, 19)
      )
    )
  }

  test("Recognizes tuples notation") {
    val inputSpec =
      """<<e1, e2>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 1),
        Token(TokenKind.IDENTIFIER, "e1", 1, 3),
        Token(TokenKind.COMMA, ",", 1, 5),
        Token(TokenKind.IDENTIFIER, "e2", 1, 7),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 9),
        Token(TokenKind.EOF, null, 1, 11)
      )
    )
  }

  test("Recognizes the 'cross product' notation") {
    val inputSpec =
      """S1 \X S2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "S1", 1, 1),
        Token(TokenKind.TIMES, "\\X", 1, 4),
        Token(TokenKind.IDENTIFIER, "S2", 1, 7),
        Token(TokenKind.EOF, null, 1, 9)
      )
    )
  }

  test("Recognizes the 'IF ... THEN ... ELSE' notation") {
    val inputSpec =
      """IF p THEN e1 ELSE e2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IF, "IF", 1, 1),
        Token(TokenKind.IDENTIFIER, "p", 1, 4),
        Token(TokenKind.THEN, "THEN", 1, 6),
        Token(TokenKind.IDENTIFIER, "e1", 1, 11),
        Token(TokenKind.ELSE, "ELSE", 1, 14),
        Token(TokenKind.IDENTIFIER, "e2", 1, 19),
        Token(TokenKind.EOF, null, 1, 21)
      )
    )
  }

  test("Recognizes the case notation") {
    val inputSpec =
      """CASE p1 -> e1 [] p2 -> e2 [] OTHER -> e""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CASE, "CASE", 1, 1),
        Token(TokenKind.IDENTIFIER, "p1", 1, 6),
        Token(TokenKind.RIGHT_ARROW, "->", 1, 9),
        Token(TokenKind.IDENTIFIER, "e1", 1, 12),
        Token(TokenKind.SQUARE, "[]", 1, 15),
        Token(TokenKind.IDENTIFIER, "p2", 1, 18),
        Token(TokenKind.RIGHT_ARROW, "->", 1, 21),
        Token(TokenKind.IDENTIFIER, "e2", 1, 24),
        Token(TokenKind.SQUARE, "[]", 1, 27),
        Token(TokenKind.OTHER, "OTHER", 1, 30),
        Token(TokenKind.RIGHT_ARROW, "->", 1, 36),
        Token(TokenKind.IDENTIFIER, "e", 1, 39),
        Token(TokenKind.EOF, null, 1, 40)
      )
    )
  }

  test("Recognizes the 'LET ... IN ...' notation") {
    val inputSpec =
      """LET d1 == e1 IN e""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LET, "LET", 1, 1),
        Token(TokenKind.IDENTIFIER, "d1", 1, 5),
        Token(TokenKind.DEFINE, "==", 1, 8),
        Token(TokenKind.IDENTIFIER, "e1", 1, 11),
        Token(TokenKind.IN, "IN", 1, 14),
        Token(TokenKind.IDENTIFIER, "e", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes the conjunction lists notation") {
    val inputSpec =
      """/\ p1
        |/\ p2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.AND, "/\\", 1, 1),
        Token(TokenKind.IDENTIFIER, "p1", 1, 4),
        Token(TokenKind.AND, "/\\", 2, 1),
        Token(TokenKind.IDENTIFIER, "p2", 2, 4),
        Token(TokenKind.EOF, null, 2, 6)
      )
    )
  }

  test("Recognizes the prime notation") {
    val inputSpec =
      """e'""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "e", 1, 1),
        Token(TokenKind.PRIME, "'", 1, 2),
        Token(TokenKind.EOF, null, 1, 3)
      )
    )
  }

  test("Recognizes the [A]_e notation") {
    val inputSpec =
      """[A]_<<e1>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 1),
        Token(TokenKind.IDENTIFIER, "A", 1, 2),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 3),
        Token(TokenKind.UNDERSCORE, "_", 1, 4),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 5),
        Token(TokenKind.IDENTIFIER, "e1", 1, 7),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 9),
        Token(TokenKind.EOF, null, 1, 11)
      )
    )
  }

  test("Recognizes the <<A>>_e notation") {
    val inputSpec =
      """<<A>>_<<e1>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 1),
        Token(TokenKind.IDENTIFIER, "A", 1, 3),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 4),
        Token(TokenKind.UNDERSCORE, "_", 1, 6),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 7),
        Token(TokenKind.IDENTIFIER, "e1", 1, 9),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 11),
        Token(TokenKind.EOF, null, 1, 13)
      )
    )
  }

  test("Recognizes the ENABLED notation") {
    val inputSpec =
      """ENABLED A""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.ENABLED, "ENABLED", 1, 1),
        Token(TokenKind.IDENTIFIER, "A", 1, 9),
        Token(TokenKind.EOF, null, 1, 10)
      )
    )
  }

  test("Recognizes the UNCHANGED notation") {
    val inputSpec =
      """UNCHANGED A""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.UNCHANGED, "UNCHANGED", 1, 1),
        Token(TokenKind.IDENTIFIER, "A", 1, 11),
        Token(TokenKind.EOF, null, 1, 12)
      )
    )
  }

  test("Recognizes the 'always' notation") {
    val inputSpec =
      """[] F""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.SQUARE, "[]", 1, 1),
        Token(TokenKind.IDENTIFIER, "F", 1, 4),
        Token(TokenKind.EOF, null, 1, 5)
      )
    )
  }

  test("Recognizes the 'eventually' notation") {
    val inputSpec =
      """<> F""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.DIAMOND, "<>", 1, 1),
        Token(TokenKind.IDENTIFIER, "F", 1, 4),
        Token(TokenKind.EOF, null, 1, 5)
      )
    )
  }

  test("Recognizes the 'WF_e(A)' notation") {
    val inputSpec =
      """WF_e(A)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.WF, "WF", 1, 1),
        Token(TokenKind.UNDERSCORE, "_", 1, 3),
        Token(TokenKind.IDENTIFIER, "e", 1, 4),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 5),
        Token(TokenKind.IDENTIFIER, "A", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.EOF, null, 1, 8)
      )
    )
  }

  test("Recognizes the 'SF_e(A)' notation") {
    val inputSpec =
      """SF_e(A)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.SF, "SF", 1, 1),
        Token(TokenKind.UNDERSCORE, "_", 1, 3),
        Token(TokenKind.IDENTIFIER, "e", 1, 4),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 5),
        Token(TokenKind.IDENTIFIER, "A", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.EOF, null, 1, 8)
      )
    )
  }

  test("Recognizes the 'leads to' notation") {
    val inputSpec =
      """F ~> G""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "F", 1, 1),
        Token(TokenKind.LEADS_TO, "~>", 1, 3),
        Token(TokenKind.IDENTIFIER, "G", 1, 6),
        Token(TokenKind.EOF, null, 1, 7)
      )
    )
  }

  test("Recognizes the 'concatenation' notation (1)") {
    val inputSpec =
      """seq1 \o seq2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "seq1", 1, 1),
        Token(TokenKind.CIRCLE, "\\o", 1, 6),
        Token(TokenKind.IDENTIFIER, "seq2", 1, 9),
        Token(TokenKind.EOF, null, 1, 13)
      )
    )
  }

  test("Recognizes the 'concatenation' notation (2)") {
    val inputSpec =
      """seq1 \circ seq2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "seq1", 1, 1),
        Token(TokenKind.CIRCLE, "\\circ", 1, 6),
        Token(TokenKind.IDENTIFIER, "seq2", 1, 12),
        Token(TokenKind.EOF, null, 1, 16)
      )
    )
  }

  test("Recognizes the 'oplus' notation (1)") {
    val inputSpec =
      """bag1 (+) bag2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "bag1", 1, 1),
        Token(TokenKind.OPLUS, "(+)", 1, 6),
        Token(TokenKind.IDENTIFIER, "bag2", 1, 10),
        Token(TokenKind.EOF, null, 1, 14)
      )
    )
  }

  test("Recognizes the 'oplus' notation (2)") {
    val inputSpec =
      """bag1 \oplus bag2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "bag1", 1, 1),
        Token(TokenKind.OPLUS, "\\oplus", 1, 6),
        Token(TokenKind.IDENTIFIER, "bag2", 1, 13),
        Token(TokenKind.EOF, null, 1, 17)
      )
    )
  }

  test("Recognizes the 'ominus' notation (1)") {
    val inputSpec =
      """bag1 (-) bag2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "bag1", 1, 1),
        Token(TokenKind.OMINUS, "(-)", 1, 6),
        Token(TokenKind.IDENTIFIER, "bag2", 1, 10),
        Token(TokenKind.EOF, null, 1, 14)
      )
    )
  }

  test("Recognizes the 'ominus' notation (2)") {
    val inputSpec =
      """bag1 \ominus bag2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "bag1", 1, 1),
        Token(TokenKind.OMINUS, "\\ominus", 1, 6),
        Token(TokenKind.IDENTIFIER, "bag2", 1, 14),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes the 'sqsubseteq' notation") {
    val inputSpec =
      """bag1 \sqsubseteq bag2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "bag1", 1, 1),
        Token(TokenKind.SQSUBSETEQ, "\\sqsubseteq", 1, 6),
        Token(TokenKind.IDENTIFIER, "bag2", 1, 18),
        Token(TokenKind.EOF, null, 1, 22)
      )
    )
  }

  test("Recognizes the ':>' notation from the TLC module") {
    val inputSpec =
      """:>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.COLON_GT, ":>", 1, 1),
        Token(TokenKind.EOF, null, 1, 3)
      )
    )
  }

  test("Recognizes strings") {
    val inputSpec =
      """ {"string1", "string2"} """.stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 2),
        Token(TokenKind.STRING, "string1", 1, 3),
        Token(TokenKind.COMMA, ",", 1, 12),
        Token(TokenKind.STRING, "string2", 1, 14),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 23),
        Token(TokenKind.EOF, null, 1, 25)
      )
    )
  }

  test("Recognizes integers") {
    val inputSpec =
      """ {1, -1} """.stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 2),
        Token(TokenKind.INTEGER, "1", 1, 3),
        Token(TokenKind.COMMA, ",", 1, 4),
        Token(TokenKind.MINUS, "-", 1, 6),
        Token(TokenKind.INTEGER, "1", 1, 7),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 8),
        Token(TokenKind.EOF, null, 1, 10)
      )
    )
  }

  test("Recognizes single-line comments") {
    val inputSpec =
      """ \* comment ...
        |1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INTEGER, "1", 2, 1),
        Token(TokenKind.EOF, null, 2, 2)
      )
    )
  }

  test("Recognizes single-line comments after some expressions") {
    val inputSpec =
      """1 \* comment ...""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INTEGER, "1", 1, 1),
        Token(TokenKind.EOF, null, 1, 17)
      )
    )
  }

  test("Recognizes multi-line comments") {
    val inputSpec =
      """ (* comment ... *)
        |1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INTEGER, "1", 2, 1),
        Token(TokenKind.EOF, null, 2, 2)
      )
    )
  }

  test("Recognizes multi-line comments on multiple lines") {
    val inputSpec =
      """ 1 (* comment ...
        | ... *) + 2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INTEGER, "1", 1, 2),
        Token(TokenKind.PLUS, "+", 2, 9),
        Token(TokenKind.INTEGER, "2", 2, 11),
        Token(TokenKind.EOF, null, 2, 12)
      )
    )
  }

  test("Recognizes the opening of an EPC spec within multi-line comments") {
    val inputSpec =
      """(* --EPC foo
        | (p = 1) [[ [] <| var x. 0 ]] *)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_CONTEXT_OPEN, "--EPC", 1, 1),
        Token(TokenKind.IDENTIFIER, "foo", 1, 10),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 2, 2),
        Token(TokenKind.IDENTIFIER, "p", 2, 3),
        Token(TokenKind.EQ, "=", 2, 5),
        Token(TokenKind.INTEGER, "1", 2, 7),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 2, 8),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 2, 10),
        Token(TokenKind.SQUARE, "[]", 2, 13),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 2, 16),
        Token(TokenKind.EPC_VAR, "var", 2, 19),
        Token(TokenKind.IDENTIFIER, "x", 2, 23),
        Token(TokenKind.DOT, ".", 2, 24),
        Token(TokenKind.INTEGER, "0", 2, 26),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 2, 28),
        Token(TokenKind.EPC_CONTEXT_CLOSE, "", 2, 31),
        Token(TokenKind.EOF, null, 2, 33)
      )
    )
  }

  test("Recognizes EPC 'var' declarations") {
    val inputSpec =
      """var x""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_VAR, "var", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 5),
        Token(TokenKind.EOF, null, 1, 6)
      )
    )
  }

  test("Recognizes EPC 'assign' actions") {
    val inputSpec =
      """<< x := 10 >>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 1),
        Token(TokenKind.IDENTIFIER, "x", 1, 4),
        Token(TokenKind.EPC_ASSIGN, ":=", 1, 6),
        Token(TokenKind.INTEGER, "10", 1, 9),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 12),
        Token(TokenKind.EOF, null, 1, 14)
      )
    )
  }

  test("Recognizes single EPC location with an integer id") {
    val inputSpec =
      """(p = 1) [[ [x |-> Undefined] <| (j = 1) (var x; <<x := 10>>) ]]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 1),
        Token(TokenKind.IDENTIFIER, "p", 1, 2),
        Token(TokenKind.EQ, "=", 1, 4),
        Token(TokenKind.INTEGER, "1", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 1, 9),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 12),
        Token(TokenKind.IDENTIFIER, "x", 1, 13),
        Token(TokenKind.MAPS_TO, "|->", 1, 15),
        Token(TokenKind.IDENTIFIER, "Undefined", 1, 19),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 28),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 1, 30),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 33),
        Token(TokenKind.IDENTIFIER, "j", 1, 34),
        Token(TokenKind.EQ, "=", 1, 36),
        Token(TokenKind.INTEGER, "1", 1, 38),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 39),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 41),
        Token(TokenKind.EPC_VAR, "var", 1, 42),
        Token(TokenKind.IDENTIFIER, "x", 1, 46),
        Token(TokenKind.SEMICOLON, ";", 1, 47),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 49),
        Token(TokenKind.IDENTIFIER, "x", 1, 51),
        Token(TokenKind.EPC_ASSIGN, ":=", 1, 53),
        Token(TokenKind.INTEGER, "10", 1, 56),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 58),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 60),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 1, 62),
        Token(TokenKind.EOF, null, 1, 64)
      )
    )
  }

  test("Recognizes single EPC location with a string id") {
    val inputSpec =
      """(q = "a") [[ [x |-> Undefined] <| (j = "a") (var x; <<x := 10>>) ]]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 1),
        Token(TokenKind.IDENTIFIER, "q", 1, 2),
        Token(TokenKind.EQ, "=", 1, 4),
        Token(TokenKind.STRING, "a", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 9),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 1, 11),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "[", 1, 14),
        Token(TokenKind.IDENTIFIER, "x", 1, 15),
        Token(TokenKind.MAPS_TO, "|->", 1, 17),
        Token(TokenKind.IDENTIFIER, "Undefined", 1, 21),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", 1, 30),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 1, 32),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 35),
        Token(TokenKind.IDENTIFIER, "j", 1, 36),
        Token(TokenKind.EQ, "=", 1, 38),
        Token(TokenKind.STRING, "a", 1, 40),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 43),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 45),
        Token(TokenKind.EPC_VAR, "var", 1, 46),
        Token(TokenKind.IDENTIFIER, "x", 1, 50),
        Token(TokenKind.SEMICOLON, ";", 1, 51),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 53),
        Token(TokenKind.IDENTIFIER, "x", 1, 55),
        Token(TokenKind.EPC_ASSIGN, ":=", 1, 57),
        Token(TokenKind.INTEGER, "10", 1, 60),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 62),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 64),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 1, 66),
        Token(TokenKind.EOF, null, 1, 68)
      )
    )
  }

  test("Recognizes multiple EPC locations with integer ids") {
    val inputSpec =
      """(p \in 1..10) [[ [] <| (j = 1)( {{ }} ) ]]"""

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 1),
        Token(TokenKind.IDENTIFIER, "p", 1, 2),
        Token(TokenKind.SET_IN, "\\in", 1, 4),
        Token(TokenKind.INTEGER, "1", 1, 8),
        Token(TokenKind.DOT_DOT, "..", 1, 9),
        Token(TokenKind.INTEGER, "10", 1, 11),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 13),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 1, 15),
        Token(TokenKind.SQUARE, "[]", 1, 18),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 1, 21),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 24),
        Token(TokenKind.IDENTIFIER, "j", 1, 25),
        Token(TokenKind.EQ, "=", 1, 27),
        Token(TokenKind.INTEGER, "1", 1, 29),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 30),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 31),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 33),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 36),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 39),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 1, 41),
        Token(TokenKind.EOF, null, 1, 43)
      )
    )
  }

  test("Recognizes multiple EPC locations with string ids") {
    val inputSpec =
      """(q \in {"a", "b"}) [[ [] <| (j = 1)( {{ }} ) ]]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 1),
        Token(TokenKind.IDENTIFIER, "q", 1, 2),
        Token(TokenKind.SET_IN, "\\in", 1, 4),
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 8),
        Token(TokenKind.STRING, "a", 1, 9),
        Token(TokenKind.COMMA, ",", 1, 12),
        Token(TokenKind.STRING, "b", 1, 14),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 17),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 18),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 1, 20),
        Token(TokenKind.SQUARE, "[]", 1, 23),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 1, 26),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 29),
        Token(TokenKind.IDENTIFIER, "j", 1, 30),
        Token(TokenKind.EQ, "=", 1, 32),
        Token(TokenKind.INTEGER, "1", 1, 34),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 35),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 36),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 38),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 41),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 44),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 1, 46),
        Token(TokenKind.EOF, null, 1, 48)
      )
    )
  }

  test("Recognizes thread compositions") {
    val inputSpec =
      """(p = 1) [[ [] <| (j = 1) ({{ }}) | (k = 2) ({{ }}) ]]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 1),
        Token(TokenKind.IDENTIFIER, "p", 1, 2),
        Token(TokenKind.EQ, "=", 1, 4),
        Token(TokenKind.INTEGER, "1", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", 1, 9),
        Token(TokenKind.SQUARE, "[]", 1, 12),
        Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", 1, 15),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 18),
        Token(TokenKind.IDENTIFIER, "j", 1, 19),
        Token(TokenKind.EQ, "=", 1, 21),
        Token(TokenKind.INTEGER, "1", 1, 23),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 24),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 26),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 27),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 30),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 32),
        Token(TokenKind.EPC_THREAD_COMPOSITION, "|", 1, 34),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 36),
        Token(TokenKind.IDENTIFIER, "k", 1, 37),
        Token(TokenKind.EQ, "=", 1, 39),
        Token(TokenKind.INTEGER, "2", 1, 41),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 42),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 44),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 45),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 48),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 50),
        Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", 1, 52),
        Token(TokenKind.EOF, null, 1, 54)
      )
    )
  }

  test("Recognizes EPC channel definitions") {
    val inputSpec =
      """new c""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_NEW, "new", 1, 1),
        Token(TokenKind.IDENTIFIER, "c", 1, 5),
        Token(TokenKind.EOF, null, 1, 6)
      )
    )
  }

  test("Recognizes EPC suspicious channel definitions") {
    val inputSpec =
      """suspchan c""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_SUSPCHAN, "suspchan", 1, 1),
        Token(TokenKind.IDENTIFIER, "c", 1, 10),
        Token(TokenKind.EOF, null, 1, 11)
      )
    )
  }

  test("Recognizes EPC receiving a message on a channel") {
    val inputSpec =
      """c@2?(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "c", 1, 1),
        Token(TokenKind.AT, "@", 1, 2),
        Token(TokenKind.INTEGER, "2", 1, 3),
        Token(TokenKind.QUESTION_MARK, "?", 1, 4),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 5),
        Token(TokenKind.IDENTIFIER, "x", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.EOF, null, 1, 8)
      )
    )
  }

  test("Recognizes EPC choices of multiple receive actions") {
    val inputSpec =
      """c@1?(x) + c@2?(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "c", 1, 1),
        Token(TokenKind.AT, "@", 1, 2),
        Token(TokenKind.INTEGER, "1", 1, 3),
        Token(TokenKind.QUESTION_MARK, "?", 1, 4),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 5),
        Token(TokenKind.IDENTIFIER, "x", 1, 6),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 7),
        Token(TokenKind.PLUS, "+", 1, 9),
        Token(TokenKind.IDENTIFIER, "c", 1, 11),
        Token(TokenKind.AT, "@", 1, 12),
        Token(TokenKind.INTEGER, "2", 1, 13),
        Token(TokenKind.QUESTION_MARK, "?", 1, 14),
        Token(TokenKind.LEFT_ROUND_BRACKET, "(", 1, 15),
        Token(TokenKind.IDENTIFIER, "x", 1, 16),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")", 1, 17),
        Token(TokenKind.EOF, null, 1, 18)
      )
    )
  }

  test("Recognizes EPC sending a message on a channel") {
    val inputSpec =
      """c@2<<x>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "c", 1, 1),
        Token(TokenKind.AT, "@", 1, 2),
        Token(TokenKind.INTEGER, "2", 1, 3),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 4),
        Token(TokenKind.IDENTIFIER, "x", 1, 6),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 7),
        Token(TokenKind.EOF, null, 1, 9)
      )
    )
  }

  test("Recognizes EPC traveling messages") {
    val inputSpec =
      """c{1 -> 2}<<1>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "c", 1, 1),
        Token(TokenKind.LEFT_CURLY_BRACKET, "{", 1, 2),
        Token(TokenKind.INTEGER, "1", 1, 3),
        Token(TokenKind.RIGHT_ARROW, "->", 1, 5),
        Token(TokenKind.INTEGER, "2", 1, 8),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}", 1, 9),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", 1, 10),
        Token(TokenKind.INTEGER, "1", 1, 12),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", 1, 13),
        Token(TokenKind.EOF, null, 1, 15)
      )
    )
  }

  test("Recognizes EPC multi-sets") {
    val inputSpec =
      """{{ 1, 1 }}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 1),
        Token(TokenKind.INTEGER, "1", 1, 4),
        Token(TokenKind.COMMA, ",", 1, 5),
        Token(TokenKind.INTEGER, "1", 1, 7),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 9),
        Token(TokenKind.EOF, null, 1, 11)
      )
    )
  }

  test("Recognizes EPC conditional threads") {
    val inputSpec =
      """if 1 = 1 then {{ }} else {{ }} fi""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EPC_IF, "if", 1, 1),
        Token(TokenKind.INTEGER, "1", 1, 4),
        Token(TokenKind.EQ, "=", 1, 6),
        Token(TokenKind.INTEGER, "1", 1, 8),
        Token(TokenKind.EPC_THEN, "then", 1, 10),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 15),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 18),
        Token(TokenKind.EPC_ELSE, "else", 1, 21),
        Token(TokenKind.EPC_MULTISET_OPEN, "{{", 1, 26),
        Token(TokenKind.EPC_MULTISET_CLOSE, "}}", 1, 29),
        Token(TokenKind.EPC_FI, "fi", 1, 32),
        Token(TokenKind.EOF, null, 1, 34)
      )
    )
  }

  test("Recognizes EPC skip elements") {
    val inputSpec =
      """skip""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens == List(Token(TokenKind.EPC_SKIP, "skip", 1, 1), Token(TokenKind.EOF, null, 1, 5)))
  }

}
