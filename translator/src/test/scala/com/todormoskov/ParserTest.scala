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
    assert(ast == ExtendsStatement(List(Identifier("foo"), Identifier("bar"))))
  }

  test("Parses CONSTANT declarations") {
    val inputSpec =
      """CONSTANTS X, Y, Z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseConstantDeclaration().toOption.get
    assert(ast == ConstantDeclaration(List(Identifier("X"), Identifier("Y"), Identifier("Z"))))
  }

  test("Parses VARIABLE declarations") {
    val inputSpec =
      """VARIABLES x, y, z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseVariableDeclaration().toOption.get
    assert(ast == VariableDeclaration(List(Identifier("x"), Identifier("y"), Identifier("z"))))
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

  test("Parses bounded set comprehensions") {
    val inputSpec =
      """{ n \in Nat : TRUE }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedSetComprehension(
        Identifier("n"),
        Identifier("Nat"),
        Literal(true)))
  }

  test("Parses quantified set comprehensions") {
    val inputSpec =
      """{ n : n \in Nat }""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      QuantifiedSetComprehension(
        Identifier("n"),
        List(
          QuantifierBound(
            Identifier("n"),
            Identifier("Nat")
          )
        )
      )
    )
  }

  test("Parses a CHOOSE expression") {
    val inputSpec =
      """CHOOSE n \in Nat : TRUE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ChooseExpression(
        Identifier("n"),
        Identifier("Nat"),
        Literal(true)))
  }

  test("Parses a function with a single quantifier bound") {
    val inputSpec =
      """[ n \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(QuantifierBound(Identifier("n"), Identifier("Nat"))),
        Identifier("n")))
  }

  test("Parses a function with a multiple quantifier bounds") {
    val inputSpec =
      """[ n \in Nat, m \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          QuantifierBound(Identifier("n"), Identifier("Nat")),
          QuantifierBound(Identifier("m"), Identifier("Nat"))
        ),
        Identifier("n")))
  }

  test("Parses a function with a single quantifier bound with multiple variables") {
    val inputSpec =
      """[ n, m \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          QuantifierBound(List(Identifier("n"), Identifier("m")), Identifier("Nat")),
        ),
        Identifier("n")))
  }

  test("Parses a function with a single quantifier bound, which is a pair") {
    val inputSpec =
      """[ << n, m >> \in NatPairs |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          QuantifierBound(IdentifierTuple(List(Identifier("n"), Identifier("m"))), Identifier("NatPairs")),
        ),
        Identifier("n")))
  }

  test("Parses a function with a multiple quantifier bounds, which are pairs") {
    val inputSpec =
      """[ << n, m >> \in NatPairs, << k, l >> \in NatPairs |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          QuantifierBound(IdentifierTuple(List(Identifier("n"), Identifier("m"))), Identifier("NatPairs")),
          QuantifierBound(IdentifierTuple(List(Identifier("k"), Identifier("l"))), Identifier("NatPairs")),
        ),
        Identifier("n")))
  }

  test("Parses the set of all functions of type A -> B") {
    val inputSpec =
      """[ A -> B ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      SetOfAllFunctions(Identifier("A"), Identifier("B")))
  }

  test("Parses the set of all records with specific keys and their corresponding domains") {
    val inputSpec =
      """[ foo : {"FOO"}, bar: {"BAR"} ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      SetOfAllRecords(List(
        Mapping(Identifier("foo"), ExplicitSet(List(Literal("FOO")))),
        Mapping(Identifier("bar"), ExplicitSet(List(Literal("BAR")))),
      )))
  }

  test("Parses the explicit mappings of identifiers to expressions") {
    val inputSpec =
      """[ foo |-> "FOO", bar |-> "BAR" ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExplicitFunction(List(
        Mapping(Identifier("foo"), Literal("FOO")),
        Mapping(Identifier("bar"), Literal("BAR"))
      )))
  }

  test("Parses EXCEPT expressions with dot selectors") {
    val inputSpec =
      """[ foo EXCEPT !.foo1.foo2 = "FOO", !.bar1.bar2 = "BAR" ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExceptExpression(
        Identifier("foo"),
        List(
          ExceptClause(
            SelectorPath(
              List(
                DotSelector(Identifier("foo1")),
                DotSelector(Identifier("foo2")))),
            Literal("FOO")
          ),
          ExceptClause(
            SelectorPath(
              List(
                DotSelector(Identifier("bar1")),
                DotSelector(Identifier("bar2")))),
            Literal("BAR")
          ),
        )
      ))
  }

  test("Parses EXCEPT expressions with index selectors") {
    val inputSpec =
      """[ foo EXCEPT ![foo1][foo2] = "FOO", ![bar1, bar2] = "BAR" ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ExceptExpression(
        Identifier("foo"),
        List(
          ExceptClause(
            SelectorPath(
              List(
                IndexSelector(List(Identifier("foo1"))),
                IndexSelector(List(Identifier("foo2")))
              )
            ),
            Literal("FOO")
          ),
          ExceptClause(
            SelectorPath(
              List(
                IndexSelector(
                  List(
                    Identifier("bar1"),
                    Identifier("bar2")
                  )
                )
              )
            ),
            Literal("BAR")
          ),
        )
      ))
  }

  test("Parses tuples of expressions") {
    val inputSpec =
      """<< n, m >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      Tuple(List(Identifier("n"), Identifier("m")))
    )
  }

  test("Parses parenthesized expressions") {
    val inputSpec =
      """(n)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      ParenthesizedExpression(Identifier("n"))
    )
  }

  test("Parses an IF-THEN-ELSE expression.") {
    val inputSpec =
      """IF TRUE THEN TRUE ELSE FALSE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      IfThenElse(Literal(true), Literal(true), Literal(false))
    )
  }

  test("Parses primed expressions") {
    val inputSpec =
      """n'""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      PrimedExpression(Identifier("n"))
    )
  }

  test("Parses primed expressions applied to arguments") {
    val inputSpec =
      """n'[bar]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      FunctionApplication(
        PrimedExpression(Identifier("n")),
        List(Identifier("bar"))
      )
    )
  }

  test("Parses field accesses") {
    val inputSpec =
      """foo1.foo2.foo3""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        ".",
        BinaryExpression(
          ".",
          Identifier("foo1"),
          Identifier("foo2")
        ),
        Identifier("foo3")
      )
    )
  }

  test("Parses exponentiations") {
    val inputSpec =
      """10^20^30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "^",
        Literal(10),
        BinaryExpression(
          "^",
          Literal(20),
          Literal(30)
        )
      )
    )
  }

  test("Parses multiplicative expressions") {
    val inputSpec =
      """10 * 20 * 30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "*",
        BinaryExpression(
          "*",
          Literal(10),
          Literal(20)
        ),
        Literal(30),
      )
    )
  }

  test("Parses multiplicative expressions and exponentiations (1)") {
    val inputSpec =
      """10 * 20^30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "*",
        Literal(10),
        BinaryExpression(
          "^",
          Literal(20),
          Literal(30)
        )
      )
    )
  }

  test("Parses multiplicative expressions and exponentiations (2)") {
    val inputSpec =
      """10^20 * 30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "*",
        BinaryExpression(
          "^",
          Literal(10),
          Literal(20)
        ),
        Literal(30)
      )
    )
  }

  test("Parses multiplicative expressions and exponentiations (3)") {
    val inputSpec =
      """10^(20 * 30) * 40""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "*",
        BinaryExpression(
          "^",
          Literal(10),
          ParenthesizedExpression(
            BinaryExpression(
              "*",
              Literal(20),
              Literal(30)
            )
          )
        ),
        Literal(40)
      )
    )
  }

  test("Parses additive expressions") {
    val inputSpec =
      """10 + 20 + 30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "+",
        BinaryExpression(
          "+",
          Literal(10),
          Literal(20)
        ),
        Literal(30),
      )
    )
  }

  test("Parses multiplicative and additive expressions (1)") {
    val inputSpec =
      """10 * 20 + 30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "+",
        BinaryExpression(
          "*",
          Literal(10),
          Literal(20)
        ),
        Literal(30),
      )
    )
  }

  test("Parses multiplicative and additive expressions (2)") {
    val inputSpec =
      """10 + 20 * 30""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "+",
        Literal(10),
        BinaryExpression(
          "*",
          Literal(20),
          Literal(30)
        )
      )
    )
  }

  test("Parses range expressions (1)") {
    val inputSpec =
      """10..20""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "..",
        Literal(10),
        Literal(20)
      )
    )
  }

  test("Parses range expressions (2)") {
    val inputSpec =
      """10..10+10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "..",
        Literal(10),
        BinaryExpression(
          "+",
          Literal(10),
          Literal(10),
        )
      )
    )
  }

  test("Parses range expressions (3)") {
    val inputSpec =
      """10+10..10*10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "..",
        BinaryExpression(
          "+",
          Literal(10),
          Literal(10),
        ),
        BinaryExpression(
          "*",
          Literal(10),
          Literal(10),
        )
      )
    )
  }

  test("Parses range expressions (4)") {
    val inputSpec =
      """foo[10]..bar[20]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "..",
        FunctionApplication(
          Identifier("foo"),
          List(
            Literal(10)
          ),
        ),
        FunctionApplication(
          Identifier("bar"),
          List(
            Literal(20)
          ),
        ),
      )
    )
  }

  test("Parses set operations (1)") {
    val inputSpec =
      """10..20 \ 10..15""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\",
        BinaryExpression(
          "..",
          Literal(10),
          Literal(20)
        ),
        BinaryExpression(
          "..",
          Literal(10),
          Literal(15)
        )
      )
    )
  }

  test("Parses set operations (2)") {
    val inputSpec =
      """{1,2,3} \union {4,5,6}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\union",
        ExplicitSet(
          List(
            Literal(1),
            Literal(2),
            Literal(3)
          )
        ),
        ExplicitSet(
          List(
            Literal(4),
            Literal(5),
            Literal(6)
          )
        ),
      )
    )
  }

  test("Parses set operations (3)") {
    val inputSpec =
      """{1,2,3} \union {4,5,6} \intersect 1..6""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\intersect",
        BinaryExpression(
          "\\union",
          ExplicitSet(
            List(
              Literal(1),
              Literal(2),
              Literal(3)
            )
          ),
          ExplicitSet(
            List(
              Literal(4),
              Literal(5),
              Literal(6)
            )
          ),
        ),
        BinaryExpression(
          "..",
          Literal(1),
          Literal(6)
        )
      )
    )
  }

  test("Parses relational operations (1)") {
    val inputSpec =
      """10 = 10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "=",
        Literal(10),
        Literal(10)
      )
    )
  }

  test("Parses relational operations (2)") {
    val inputSpec =
      """10 # 20""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "#",
        Literal(10),
        Literal(20)
      )
    )
  }

  test("Parses relational operations (3)") {
    val inputSpec =
      """10 /= 20""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "/=",
        Literal(10),
        Literal(20)
      )
    )
  }

  test("Parses relational operations (4)") {
    val inputSpec =
      """10 < 20""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "<",
        Literal(10),
        Literal(20)
      )
    )
  }

  test("Parses relational operations (5)") {
    val inputSpec =
      """10 =< 20""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "=<",
        Literal(10),
        Literal(20)
      )
    )
  }

  test("Parses relational operations (6)") {
    val inputSpec =
      """20 > 10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        ">",
        Literal(20),
        Literal(10)
      )
    )
  }

  test("Parses relational operations (7)") {
    val inputSpec =
      """20 >= 10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        ">=",
        Literal(20),
        Literal(10)
      )
    )
  }


  test("Parses relational operations (8)") {
    val inputSpec =
      """1 \in 1..10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\in",
        Literal(1),
        BinaryExpression(
          "..",
          Literal(1),
          Literal(10)
        )
      )
    )
  }

  test("Parses relational operations (9)") {
    val inputSpec =
      """0 \notin 1..10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\notin",
        Literal(0),
        BinaryExpression(
          "..",
          Literal(1),
          Literal(10)
        )
      )
    )
  }

  test("Parses relational operations (10)") {
    val inputSpec =
      """1..5 \subset 1..10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\subset",
        BinaryExpression(
          "..",
          Literal(1),
          Literal(5)
        ),
        BinaryExpression(
          "..",
          Literal(1),
          Literal(10)
        )
      )
    )
  }

  test("Parses binary disjunctions") {
    val inputSpec =
      """TRUE /\ FALSE /\ (TRUE /\ FALSE)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "/\\",
         BinaryExpression(
          "/\\",
          Literal(true),
          Literal(false)
        ),
        ParenthesizedExpression(
          BinaryExpression(
            "/\\",
            Literal(true),
            Literal(false)
          )
        )
      )
    )
  }

  test("Parses binary conjunctions") {
    val inputSpec =
      """TRUE \/ FALSE \/ (TRUE \/ FALSE)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\/",
        BinaryExpression(
          "\\/",
          Literal(true),
          Literal(false)
        ),
        ParenthesizedExpression(
          BinaryExpression(
            "\\/",
            Literal(true),
            Literal(false)
          )
        )
      )
    )
  }

  test("Parses binary equivalence") {
    val inputSpec =
      """A /\ B \equiv B /\ A""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\equiv",
        BinaryExpression(
          "/\\",
          Identifier("A"),
          Identifier("B")
        ),
        BinaryExpression(
          "/\\",
          Identifier("B"),
          Identifier("A")
        )
      )
    )
  }

  test("Parses binary implications") {
    val inputSpec =
      """A => B""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "=>",
        Identifier("A"),
        Identifier("B")
      )
    )
  }

  test("Parses complex formulas (1)") {
    val inputSpec =
      """(A => B) /\ (B => A) \equiv (A \equiv B)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\equiv",
        BinaryExpression(
          "/\\",
          ParenthesizedExpression(
            BinaryExpression(
              "=>",
              Identifier("A"),
              Identifier("B")
            )
          ),
          ParenthesizedExpression(
            BinaryExpression(
              "=>",
              Identifier("B"),
              Identifier("A")
            )
          )
        ),
        ParenthesizedExpression(
          BinaryExpression(
            "\\equiv",
            Identifier("A"),
            Identifier("B")
          )
        )
      )
    )
  }

  test("Parses unary minus") {
    val inputSpec =
      """-1""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "-",
        Literal(1)
      )
    )
  }

  test("Parses unary minus between addition and multiplication") {
    val inputSpec =
      """-1 * -2 + -3""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "+",
        BinaryExpression(
          "*",
          UnaryExpression(
            "-",
            Literal(1)
          ),
          UnaryExpression(
            "-",
            Literal(2)
          )
        ),
        UnaryExpression(
          "-",
          Literal(3)
        )
      )
    )
  }

  test("Parses unary minus in front of exponentiations") {
    val inputSpec =
      """-10^10""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "-",
        BinaryExpression(
          "^",
          Literal(10),
          Literal(10)
        )
      )
    )
  }

  test("Parses unary DOMAIN expression") {
    val inputSpec =
      """DOMAIN f""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "DOMAIN",
        Identifier("f")
      )
    )
  }

  test("Parses unary SUBSET expression") {
    val inputSpec =
      """SUBSET {1, 2, 3}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "SUBSET",
        ExplicitSet(List(Literal(1), Literal(2), Literal(3)))
      )
    )
  }

  test("Parses unary SUBSET expression in front of set operations") {
    val inputSpec =
      """SUBSET {1, 2, 3} \union {4,5,6}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "SUBSET",
        BinaryExpression(
          "\\union",
          ExplicitSet(List(Literal(1), Literal(2), Literal(3))),
          ExplicitSet(List(Literal(4), Literal(5), Literal(6)))
        )
      )
    )
  }

  test("Parses unary negations") {
    val inputSpec =
      """~TRUE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "~",
        Literal(true)
      )
    )
  }

  test("Negations bind stronger than conjunctions") {
    val inputSpec =
      """~TRUE /\ ~FALSE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "/\\",
        UnaryExpression(
          "~",
          Literal(true)
        ),
        UnaryExpression(
          "~",
          Literal(false)
        )
      )
    )
  }

  test("Negation binds weaker than equality / relational / membership operators") {
    val inputSpec =
      """~ x = y""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      UnaryExpression(
        "~",
        BinaryExpression(
          "=",
          Identifier("x"),
          Identifier("y")
        )
      ),
    )
  }

  test("Parses quantified expressions") {
    val inputSpec =
      """\E x, y : P(x,y)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      QuantifiedExpression(
        "\\E",
        List(Identifier("x"), Identifier("y")),
        OperatorApplication(
          "P",
          List(
            Identifier("x"),
            Identifier("y")
          )
        )
      ),
    )
  }

  test("Parses bounded quantified expressions") {
    val inputSpec =
      """\E x \in S, y \in T: P(x,y)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedQuantifiedExpression(
        "\\E",
        List(
          QuantifierBound(Identifier("x"), Identifier("S")),
          QuantifierBound(Identifier("y"), Identifier("T"))),
        OperatorApplication(
          "P",
          List(
            Identifier("x"),
            Identifier("y")
          )
        )
      ),
    )
  }

  test("Quantifiers bind weaker than conjunctions / disjunctions") {
    val inputSpec =
      """\E x : P /\ Q""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      QuantifiedExpression(
        "\\E",
        List(Identifier("x")),
        BinaryExpression(
          "/\\",
          Identifier("P"),
          Identifier("Q")
        )
      ),
    )
  }

  test("Quantifiers bind weaker than implications / equivalences") {
    val inputSpec =
      """\E x : x > 0 => P""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      QuantifiedExpression(
        "\\E",
        List(Identifier("x")),
        BinaryExpression(
          "=>",
          BinaryExpression(
            ">",
            Identifier("x"),
            Literal(0)
          ),
          Identifier("P")
        )
      ),
    )
  }

  test("Parses cross-products") {
    val inputSpec =
      """R \X S \times T""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BinaryExpression(
        "\\times",
        BinaryExpression(
          "\\X",
          Identifier("R"),
          Identifier("S")
        ),
        Identifier("T"),
      ),
    )
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
