package com.todormoskov

import com.todormoskov.Lexer.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class LexerTest extends AnyFunSuite {

  private def recognizeTokens(input: String): List[Token] = {
    val tokenizer = Tokenizer(Scanner(input))
    val tokens = ListBuffer[Token]()
    while (tokenizer.peek() != Token(TokenKind.EOF, null)) {
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
        Token(TokenKind.MINUS_SEPARATOR, "----"),
        Token(TokenKind.MODULE, "MODULE"),
        Token(TokenKind.IDENTIFIER, "M"),
        Token(TokenKind.MINUS_SEPARATOR, "----"),
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.MINUS_SEPARATOR, "----"),
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.EQUAL_SEPARATOR, "===="),
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes constant declarations") {
    val inputSpec =
      """CONSTANTS X, Y, Z""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CONSTANTS, "CONSTANTS"),
        Token(TokenKind.IDENTIFIER, "X"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "Y"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "Z"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes variable declarations") {
    val inputSpec =
      """VARIABLES x, y, z""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.VARIABLES, "VARIABLES"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "y"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "z"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes assumptions") {
    val inputSpec =
      """ASSUME X > 0""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.ASSUME, "ASSUME"),
        Token(TokenKind.IDENTIFIER, "X"),
        Token(TokenKind.GT, ">"),
        Token(TokenKind.INTEGER, "0"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes operator definitions") {
    val inputSpec =
      """F(x, y) == x + y""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "F"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "y"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.DEFINE, "=="),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.PLUS, "+"),
        Token(TokenKind.IDENTIFIER, "y"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes function definitions") {
    val inputSpec =
      """successor[n \in Nat] == n + 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "successor"),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "n"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "Nat"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.DEFINE, "=="),
        Token(TokenKind.IDENTIFIER, "n"),
        Token(TokenKind.PLUS, "+"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes instance statements") {
    val inputSpec =
      """INSTANCE M WITH X <- 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INSTANCE, "INSTANCE"),
        Token(TokenKind.IDENTIFIER, "M"),
        Token(TokenKind.WITH, "WITH"),
        Token(TokenKind.IDENTIFIER, "X"),
        Token(TokenKind.LEFT_ARROW, "<-"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes theorems") {
    val inputSpec =
      """THEOREM SuccessorReturnsTheNextNaturalNumber == \A n \in Nat : successor[n] = n + 1""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.THEOREM, "THEOREM"),
        Token(TokenKind.IDENTIFIER, "SuccessorReturnsTheNextNaturalNumber"),
        Token(TokenKind.DEFINE, "=="),
        Token(TokenKind.FORALL, "\\A"),
        Token(TokenKind.IDENTIFIER, "n"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "Nat"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "successor"),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "n"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EQ, "="),
        Token(TokenKind.IDENTIFIER, "n"),
        Token(TokenKind.PLUS, "+"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the logical operators") {
    val inputSpec =
      """/\ \/ ~ => <=>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.AND, "/\\"),
        Token(TokenKind.OR, "\\/"),
        Token(TokenKind.NOT, "~"),
        Token(TokenKind.IMPLICATION, "=>"),
        Token(TokenKind.EQUIVALENCE, "<=>"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the symbols TRUE, FALSE, BOOLEAN") {
    val inputSpec =
      """TRUE FALSE BOOLEAN""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.TRUE, "TRUE"),
        Token(TokenKind.FALSE, "FALSE"),
        Token(TokenKind.BOOLEAN, "BOOLEAN"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes forall expressions") {
    val inputSpec =
      """\A x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.FORALL, "\\A"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "P"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes exists expressions") {
    val inputSpec =
      """\E x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EXISTS, "\\E"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "P"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes CHOOSE expressions") {
    val inputSpec =
      """CHOOSE x \in S : P(x)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CHOOSE, "CHOOSE"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "P"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the set operators") {
    val inputSpec =
      """= # /= \in \notin \cup \union \cap \intersect \subset \""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.EQ, "="),
        Token(TokenKind.NEQ, "#"),
        Token(TokenKind.NEQ, "/="),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.NOTIN, "\\notin"),
        Token(TokenKind.UNION, "\\cup"),
        Token(TokenKind.UNION, "\\union"),
        Token(TokenKind.INTERSECT, "\\cap"),
        Token(TokenKind.INTERSECT, "\\intersect"),
        Token(TokenKind.SUBSET, "\\subset"),
        Token(TokenKind.SET_DIFFERENCE, "\\"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes set constructor with all elements") {
    val inputSpec =
      """{1, 2, 3}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.INTEGER, "2"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.INTEGER, "3"),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes set constructor with 'such that'") {
    val inputSpec =
      """{ x \in S : p}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "p"),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes set constructor with expressions for the different elements") {
    val inputSpec =
      """{ e : x \in S}""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the set of all subsets") {
    val inputSpec =
      """SUBSET S""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.BIG_SUBSET, "SUBSET"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the union of all elements of a set") {
    val inputSpec =
      """UNION S""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.BIG_UNION, "UNION"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes function application") {
    val inputSpec =
      """f[e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "f"),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the DOMAIN keyword") {
    val inputSpec =
      """DOMAIN f""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.DOMAIN, "DOMAIN"),
        Token(TokenKind.IDENTIFIER, "f"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes function constructor with 'such that'") {
    val inputSpec =
      """[ x \in S |-> e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.SET_IN, "\\in"),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.MAPS_TO, "|->"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the set of all functions from S to T") {
    val inputSpec =
      """[S -> T]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "S"),
        Token(TokenKind.RIGHT_ARROW, "->"),
        Token(TokenKind.IDENTIFIER, "T"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the function updates with EXCEPT") {
    val inputSpec =
      """[f EXCEPT ![e1]=e2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "f"),
        Token(TokenKind.EXCEPT, "EXCEPT"),
        Token(TokenKind.BANG, "!"),
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EQ, "="),
        Token(TokenKind.IDENTIFIER, "e2"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the dot notation for records") {
    val inputSpec =
      """e.h""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.DOT, "."),
        Token(TokenKind.IDENTIFIER, "h"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'maps to' notation for records") {
    val inputSpec =
      """[h1 |-> e1, h2 |-> e2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "h1"),
        Token(TokenKind.MAPS_TO, "|->"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "h2"),
        Token(TokenKind.MAPS_TO, "|->"),
        Token(TokenKind.IDENTIFIER, "e2"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the set of all records with h_i's in S_i's") {
    val inputSpec =
      """[h1 : S1, h2 : S2]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "h1"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "S1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "h2"),
        Token(TokenKind.COLON, ":"),
        Token(TokenKind.IDENTIFIER, "S2"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the record updates with EXCEPT") {
    val inputSpec =
      """[r EXCEPT !.h = e]""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "r"),
        Token(TokenKind.EXCEPT, "EXCEPT"),
        Token(TokenKind.BANG, "!"),
        Token(TokenKind.DOT, "."),
        Token(TokenKind.IDENTIFIER, "h"),
        Token(TokenKind.EQ, "="),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes tuples notation") {
    val inputSpec =
      """<<e1, e2>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.IDENTIFIER, "e2"),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'cross product' notation") {
    val inputSpec =
      """S1 \X S2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "S1"),
        Token(TokenKind.TIMES, "\\X"),
        Token(TokenKind.IDENTIFIER, "S2"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'IF ... THEN ... ELSE' notation") {
    val inputSpec =
      """IF p THEN e1 ELSE e2""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IF, "IF"),
        Token(TokenKind.IDENTIFIER, "p"),
        Token(TokenKind.THEN, "THEN"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.ELSE, "ELSE"),
        Token(TokenKind.IDENTIFIER, "e2"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the case notation") {
    val inputSpec =
      """CASE p1 -> e1 [] p2 -> e2 [] OTHER -> e""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.CASE, "CASE"),
        Token(TokenKind.IDENTIFIER, "p1"),
        Token(TokenKind.RIGHT_ARROW, "->"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.SQUARE, "[]"),
        Token(TokenKind.IDENTIFIER, "p2"),
        Token(TokenKind.RIGHT_ARROW, "->"),
        Token(TokenKind.IDENTIFIER, "e2"),
        Token(TokenKind.SQUARE, "[]"),
        Token(TokenKind.OTHER, "OTHER"),
        Token(TokenKind.RIGHT_ARROW, "->"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'LET ... IN ...' notation") {
    val inputSpec =
      """LET d1 == e1 IN e""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LET, "LET"),
        Token(TokenKind.IDENTIFIER, "d1"),
        Token(TokenKind.DEFINE, "=="),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.IN, "IN"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.EOF, null)
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
        Token(TokenKind.AND, "/\\"),
        Token(TokenKind.IDENTIFIER, "p1"),
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.AND, "/\\"),
        Token(TokenKind.IDENTIFIER, "p2"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the prime notation") {
    val inputSpec =
      """e'""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.PRIME, "'"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the [A]_e notation") {
    val inputSpec =
      """[A]_<<e1>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_SQUARE_BRACKET, "["),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.RIGHT_SQUARE_BRACKET, "]"),
        Token(TokenKind.UNDERSCORE, "_"),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the <<A>>_e notation") {
    val inputSpec =
      """<<A>>_<<e1>>""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<"),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>"),
        Token(TokenKind.UNDERSCORE, "_"),
        Token(TokenKind.LEFT_ANGLE_BRACKET, "<<"),
        Token(TokenKind.IDENTIFIER, "e1"),
        Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the ENABLED notation") {
    val inputSpec =
      """ENABLED A""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.ENABLED, "ENABLED"),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the UNCHANGED notation") {
    val inputSpec =
      """UNCHANGED A""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.UNCHANGED, "UNCHANGED"),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'always' notation") {
    val inputSpec =
      """[] F""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.SQUARE, "[]"),
        Token(TokenKind.IDENTIFIER, "F"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'eventually' notation") {
    val inputSpec =
      """<> F""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.DIAMOND, "<>"),
        Token(TokenKind.IDENTIFIER, "F"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'WF_e(A)' notation") {
    val inputSpec =
      """WF_e(A)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.WF, "WF"),
        Token(TokenKind.UNDERSCORE, "_"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'SF_e(A)' notation") {
    val inputSpec =
      """SF_e(A)""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.SF, "SF"),
        Token(TokenKind.UNDERSCORE, "_"),
        Token(TokenKind.IDENTIFIER, "e"),
        Token(TokenKind.LEFT_ROUND_BRACKET, "("),
        Token(TokenKind.IDENTIFIER, "A"),
        Token(TokenKind.RIGHT_ROUND_BRACKET, ")"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes the 'leads to' notation") {
    val inputSpec =
      """F ~> G""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.IDENTIFIER, "F"),
        Token(TokenKind.LEADS_TO, "~>"),
        Token(TokenKind.IDENTIFIER, "G"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes strings") {
    val inputSpec =
      """ {"string1", "string2"} """.stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{"),
        Token(TokenKind.STRING, "string1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.STRING, "string2"),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes integers") {
    val inputSpec =
      """ {1, -1} """.stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.LEFT_CURLY_BRACKET, "{"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.COMMA, ","),
        Token(TokenKind.MINUS, "-"),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.RIGHT_CURLY_BRACKET, "}"),
        Token(TokenKind.EOF, null)
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
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
      )
    )
  }

  test("Recognizes single-line comments after some expressions") {
    val inputSpec =
      """1 \* comment ...""".stripMargin

    val tokens = recognizeTokens(inputSpec)
    assert(tokens ==
      List(
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
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
        Token(TokenKind.NEWLINE, null),
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.EOF, null)
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
        Token(TokenKind.INTEGER, "1"),
        Token(TokenKind.PLUS, "+"),
        Token(TokenKind.INTEGER, "2"),
        Token(TokenKind.EOF, null)
      )
    )
  }


}
