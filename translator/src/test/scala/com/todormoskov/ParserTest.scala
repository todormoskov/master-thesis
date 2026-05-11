package com.todormoskov

import com.todormoskov.Lexer.*
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  // TRUE
  test("Parses the literal TRUE") {
    val inputSpec =
      """TRUE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Literal(true))
  }

  // FALSE
  test("Parses the literal FALSE") {
    val inputSpec =
      """FALSE""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Literal(false))
  }

  // BOOLEAN
  test("Parses the literal BOOLEAN") {
    val inputSpec =
      """BOOLEAN""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Literal("BOOLEAN"))
  }

  // Integers
  test("Parses integer literals") {
    val inputSpec =
      """1""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Literal(1))
  }

  // String
  test("Parses string literals") {
    val inputSpec =
      """ "someString" """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Literal("someString"))
  }

  // Identifiers
  test("Parses identifiers") {
    val inputSpec =
      """foo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == Identifier("foo"))
  }

  // EXTENDS statements
  test("Parses EXTENDS statements") {
    val inputSpec =
      """EXTENDS foo, bar""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExtendsStatement().toOption.get
    assert(ast == ExtendsStatement(List(Identifier("foo"), Identifier("bar"))))
  }

  test("Parses VARIABLE declarations") {
    val inputSpec =
      """VARIABLES x, y, z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      VariableDeclaration(
        Token(TokenKind.VARIABLES, "VARIABLES", 1, 1),
        List(Identifier("x"), Identifier("y"), Identifier("z"))
      )
    )
  }

  test("Parses CONSTANT declarations") {
    val inputSpec =
      """CONSTANTS X, Y, Z""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      ConstantDeclaration(
        Token(TokenKind.CONSTANTS, "CONSTANTS", 1, 1),
        List(
          OperatorDeclaration(Identifier("X"), Nil),
          OperatorDeclaration(Identifier("Y"), Nil),
          OperatorDeclaration(Identifier("Z"), Nil)
        )
      )
    )
  }

  test("Parses CONSTANT declarations of operators with parameters") {
    val inputSpec =
      """CONSTANTS F(_, _, _)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      ConstantDeclaration(
        Token(TokenKind.CONSTANTS, "CONSTANTS", 1, 1),
        List(
          OperatorDeclaration(
            Identifier("F"),
            List(
              Token(TokenKind.UNDERSCORE, "_", 1, 13),
              Token(TokenKind.UNDERSCORE, "_", 1, 16),
              Token(TokenKind.UNDERSCORE, "_", 1, 19)
            )
          ),
        )
      )
    )
  }

  test("Parses operator definitions with no parameters") {
    val inputSpec =
      """F == 42""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      OperatorDefinition(
        NonFixLHS(
          Identifier("F"),
          Nil
        ),
        Literal(42)
      )
    )
  }

  test("Parses operator definitions with parameters") {
    val inputSpec =
      """Add(x, y) == x + y""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      OperatorDefinition(
        NonFixLHS(
          Identifier("Add"),
          List(
            Identifier("x"),
            Identifier("y")
          )
        ),
        BinaryExpression(
          "+",
          Identifier("x"),
          Identifier("y")
        )
      )
    )
  }

  test("Parses higher-order operator definitions") {
    val inputSpec =
      """Compose(f(_, _), g(_, _), x) == f(g(x, x), g(x, x))""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      OperatorDefinition(
        NonFixLHS(
          Identifier("Compose"),
          List(
            OperatorDeclaration(
              Identifier("f"),
              List(
                Token(TokenKind.UNDERSCORE, "_", 1, 11),
                Token(TokenKind.UNDERSCORE, "_", 1, 14)
              )
            ),
            OperatorDeclaration(
              Identifier("g"),
              List(
                Token(TokenKind.UNDERSCORE, "_", 1, 20),
                Token(TokenKind.UNDERSCORE, "_", 1, 23)
              )
            ),
            Identifier("x")
          )
        ),
        IdentifierApplication(
          Identifier("f"),
          List(
            IdentifierApplication(
              Identifier("g"),
              List(
                Identifier("x"),
                Identifier("x"),
              )
            ),
            IdentifierApplication(
              Identifier("g"),
              List(
                Identifier("x"),
                Identifier("x"),
              )
            )
          )
        )
      )
    )
  }

  test("Parses function definitions with one parameter and one domain") {
    val inputSpec =
      """f[x \in S] == x + 1""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      FunctionDefinition(
        Identifier("f"),
        List(
          BoundedVariableDeclaration(
            Identifier("x"),
            Identifier("S")
          )
        ),
        BinaryExpression(
          "+",
          Identifier("x"),
          Literal(1)
        )
      )
    )
  }

  test("Parses function definitions with two parameters and separate bounds") {
    val inputSpec =
      """f[x \in S, y \in T] == << x, y >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      FunctionDefinition(
        Identifier("f"),
        List(
          BoundedVariableDeclaration(
            Identifier("x"),
            Identifier("S")
          ),
          BoundedVariableDeclaration(
            Identifier("y"),
            Identifier("T")
          )
        ),
        Tuple(
          List(
            Identifier("x"),
            Identifier("y")
          )
        )
      )
    )
  }

  test("Parses function definitions with parameters using a single comma-list bound") {
    val inputSpec =
      """h[i, j \in 1..n] == i * j""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      FunctionDefinition(
        Identifier("h"),
        List(
          BoundedVariableDeclaration(
            List(
              Identifier("i"),
              Identifier("j")
            ),
            BinaryExpression(
              "..",
              Literal(1),
              Identifier("n")
            )
          )
        ),
        BinaryExpression(
          "*",
          Identifier("i"),
          Identifier("j")
        )
      )
    )
  }

  test("Parses function definitions with tuple parameter bound") {
    val inputSpec =
      """addPair[<<i, j>> \in Dom] == i + j""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      FunctionDefinition(
        Identifier("addPair"),
        List(
          BoundedVariableDeclaration(
            IdentifierTuple(
              List(
                Identifier("i"),
                Identifier("j")
              )
            ),
            Identifier("Dom")
          )
        ),
        BinaryExpression(
          "+",
          Identifier("i"),
          Identifier("j")
        )
      )
    )
  }

  test("Parses INSTANCE statements") {
    val inputSpec =
      """INSTANCE M WITH foo <- myFoo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      Instance(
        Identifier("M"),
        List(
          Substitution(
            Identifier("foo"),
            Identifier("myFoo")
          )
        )
      )
    )
  }

  test("Parses module definition with INSTANCE") {
    val inputSpec =
      """InstantiatedModule == INSTANCE M WITH foo <- myFoo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      ModuleDefinition(
        NonFixLHS(
          Identifier("InstantiatedModule"),
          Nil
        ),
        Instance(
          Identifier("M"),
          List(
            Substitution(
              Identifier("foo"),
              Identifier("myFoo")
            )
          )
        )
      )
    )
  }

  test("Parses assumptions") {
    val inputSpec =
      """ASSUME foo > 0 """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseAssumption().toOption.get
    assert(ast ==
      Assumption(
        Token(TokenKind.ASSUME, "ASSUME", 1, 1),
        BinaryExpression(
          ">",
          Identifier("foo"),
          Literal(0)
        )
      )
    )
  }

  test("Parses theorems") {
    val inputSpec =
      """THEOREM foo > 0""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModuleUnit().toOption.get
    assert(ast ==
      Theorem(
        BinaryExpression(
          ">",
          Identifier("foo"),
          Literal(0)
        )
      )
    )
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
      IdentifierApplication(Identifier("foo"),
        List(
          IdentifierApplication(
            Identifier("bar"),
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
          BoundedVariableDeclaration(
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

  test("Parses a function with a single bounded variable declaration") {
    val inputSpec =
      """[ n \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(BoundedVariableDeclaration(Identifier("n"), Identifier("Nat"))),
        Identifier("n")))
  }

  test("Parses a function with a multiple bounded variable declarations") {
    val inputSpec =
      """[ n \in Nat, m \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          BoundedVariableDeclaration(Identifier("n"), Identifier("Nat")),
          BoundedVariableDeclaration(Identifier("m"), Identifier("Nat"))
        ),
        Identifier("n")))
  }

  test("Parses a function with a single bounded variable declaration of multiple variables") {
    val inputSpec =
      """[ n, m \in Nat |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          BoundedVariableDeclaration(List(Identifier("n"), Identifier("m")), Identifier("Nat")),
        ),
        Identifier("n")))
  }

  test("Parses a function with a single bounded variable declaration, which is a pair") {
    val inputSpec =
      """[ << n, m >> \in NatPairs |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          BoundedVariableDeclaration(IdentifierTuple(List(Identifier("n"), Identifier("m"))), Identifier("NatPairs")),
        ),
        Identifier("n")))
  }

  test("Parses a function with a multiple bounded variable declarations, which are pairs") {
    val inputSpec =
      """[ << n, m >> \in NatPairs, << k, l >> \in NatPairs |-> n ]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      BoundedMapping(
        List(
          BoundedVariableDeclaration(IdentifierTuple(List(Identifier("n"), Identifier("m"))), Identifier("NatPairs")),
          BoundedVariableDeclaration(IdentifierTuple(List(Identifier("k"), Identifier("l"))), Identifier("NatPairs")),
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
        IdentifierApplication(
          Identifier("P"),
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
          BoundedVariableDeclaration(Identifier("x"), Identifier("S")),
          BoundedVariableDeclaration(Identifier("y"), Identifier("T"))),
        IdentifierApplication(
          Identifier("P"),
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

  test("Parses LET-IN expressions (1)") {
    val inputSpec =
      """LET IsEven(x) == x % 2 = 0
        |IN IsEven(5) """.stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == LetInExpression(List(
          OperatorDefinition(
            NonFixLHS(Identifier("IsEven"), List(Identifier("x"))),
            BinaryExpression(
              "=",
              BinaryExpression("%", Identifier("x"), Literal(2)),
              Literal(0)
            )
          )),
          IdentifierApplication(Identifier("IsEven"), List(Literal(5)))
        ))
      case Left(value) => fail(value)
    }
  }

  test("Parses LET-IN expressions (2)") {
    val inputSpec =
      """LET IsEven(x) == x % 2 = 0
        |    Five == 5
        |IN  IsEven(Five) """.stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == LetInExpression(List(
          OperatorDefinition(
            NonFixLHS(Identifier("IsEven"), List(Identifier("x"))),
            BinaryExpression(
              "=",
              BinaryExpression("%", Identifier("x"), Literal(2)),
              Literal(0)
            )
          ),
          OperatorDefinition(NonFixLHS(Identifier("Five"), Nil), Literal(5))
        ),
          IdentifierApplication(Identifier("IsEven"), List(Identifier("Five")))
        ))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (1)") {
    val inputSpec =
      """/\ x = 11
        |/\ y = 12""".stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          BinaryExpression("=", Identifier("x"), Literal(11)),
          BinaryExpression("=", Identifier("y"), Literal(12))
        )))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (2)") {
    val inputSpec =
      """/\ x = 11
        | /\ y = 12""".stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          BinaryExpression("/\\",
            BinaryExpression("=", Identifier("x"), Literal(11)),
            BinaryExpression("=", Identifier("y"), Literal(12))
          )
        )))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (3)") {
    val inputSpec =
      """ /\ x = 11
        |/\ y = 12""".stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          BinaryExpression("/\\",
            BinaryExpression("=", Identifier("x"), Literal(11)),
            BinaryExpression("=", Identifier("y"), Literal(12))
          )
        )))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (4)") {
    val inputSpec =
      """/\ /\ A
        |   /\ B
        |/\ C""".stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          ConjunctionList(List(
            Identifier("A"),
            Identifier("B")
          )),
          Identifier("C")
        )))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (5)") {
    val inputSpec =
      """/\ /\ A
        |   /\ B
        |/\ /\ C
        |   /\ D""".stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          ConjunctionList(List(
            Identifier("A"),
            Identifier("B")
          )),
          ConjunctionList(List(
            Identifier("C"),
            Identifier("D")
          )),
        )))
      case Left(value) => fail(value)
    }
  }

  test("Parses conjunction lists (6)") {
    val inputSpec =
      """/\ \/ /\ A
        |      /\ B
        |   \/ /\ C
        |      /\ D
        |/\ \/ E
        |   \/ F
        |/\ G   """.stripMargin
    new Parser(Tokenizer(Scanner(inputSpec))).parseExpression() match {
      case Right(ast) =>
        assert(ast == ConjunctionList(List(
          DisjunctionList(List(
            ConjunctionList(List(
              Identifier("A"),
              Identifier("B")
            )),
            ConjunctionList(List(
              Identifier("C"),
              Identifier("D")
            )),
          )),
          DisjunctionList(List(
            Identifier("E"),
            Identifier("F")
          )),
          Identifier("G")
        )))
      case Left(value) => fail(value)
    }
  }


  test("Parses weak-stuttering nodes") {
    val inputSpec =
      """[x' = x + 1]_x""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      WeakStuttering(
        BinaryExpression(
          "=",
          PrimedExpression(Identifier("x")),
          BinaryExpression(
            "+",
            Identifier("x"),
            Literal(1)
          )
        ),
        Identifier("x"),
      ),
    )
  }

  test("Parses strong-stuttering nodes") {
    val inputSpec =
      """<< x' = f(x) >>_x""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      StrongStuttering(
        BinaryExpression(
          "=",
          PrimedExpression(Identifier("x")),
          IdentifierApplication(
            Identifier("f"),
            List(
              Identifier("x")
            )
          )
        ),
        Identifier("x"),
      ),
    )
  }

  test("Parses weak fairness expressions") {
    val inputSpec =
      """WF_v(A)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      WeakFairness(
        Identifier("v"),
        Identifier("A")
      ),
    )
  }

  test("Parses strong fairness expressions") {
    val inputSpec =
      """SF_v(A)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      StrongFairness(
        Identifier("v"),
        Identifier("A")
      ),
    )
  }

  test("Parses @ sign") {
    val inputSpec =
      """@""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast ==
      AtSign()
    )
  }

  test("Parses general identifiers") {
    val inputSpec =
      """M!foo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == GeneralIdentifier(
      List(InstancePrefixPart(Identifier("M"), Nil)),
      Identifier("foo")
    ))
  }

  test("Parses general identifiers with arguments") {
    val inputSpec =
      """M(a, b)!foo""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExpression().toOption.get
    assert(ast == GeneralIdentifier(
      List(
        InstancePrefixPart(Identifier("M"),
        List(
          Identifier("a"),
          Identifier("b")
        )
      )),
      Identifier("foo")
    ))
  }

  test("Parses the beginning and the end of a module") {
    val inputSpec =
      """---- MODULE M ----
        | EXTENDS M1, M2
        | foo == 1
        | bar(arg1) == arg1 + arg1
        | ====""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModule().toOption.get
    assert(ast == Module(
      Identifier("M"),
      ExtendsStatement(List(Identifier("M1"), Identifier("M2"))),
      List(
        OperatorDefinition(
          NonFixLHS(
            Identifier("foo"),
            Nil
          ),
          Literal(1)
        ),
        OperatorDefinition(
          NonFixLHS(
            Identifier("bar"),
            List(
              Identifier("arg1")
            )
          ),
          BinaryExpression(
            "+",
            Identifier("arg1"),
            Identifier("arg1")
          )
        ),
      )))
  }

  test("Parses nested modules") {
    val inputSpec =
      """---- MODULE Outer ----
        |VARIABLE x
        |---- MODULE Inner ----
        |VARIABLE y
        |
        |S == {0, 1}
        |Init == x \in S /\ y \in S
        |Next == x' \in S /\ y' \in S /\ x' # y'
        |Spec == Init /\ [][Next]_<<x, y>> /\ WF_<<x, y>>(Next)
        |
        |THEOREM Spec => <>(x # y) \* TODO: THEOREM P == Spec => <>(x # y) should also work!
        |====
        |I == INSTANCE Inner WITH y <- x
        | ====""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseModule().toOption.get
    assert(ast == Module(
      Identifier("Outer"),
      ExtendsStatement(Nil),
      List(
        VariableDeclaration(
          Token(TokenKind.VARIABLE, "VARIABLE", 2, 1),
          List(Identifier("x"))
        ),
        Module(
          Identifier("Inner"),
          ExtendsStatement(Nil),
          List(
            VariableDeclaration(
              Token(TokenKind.VARIABLE, "VARIABLE", 4, 1),
              List(Identifier("y"))
            ),
            OperatorDefinition(
              NonFixLHS(
                Identifier("S"),
                Nil
              ),
              ExplicitSet(
                List(
                  Literal(0),
                  Literal(1)
                )
              )
            ),
            OperatorDefinition(
              NonFixLHS(
                Identifier("Init"),
                Nil
              ),
              BinaryExpression(
                "/\\",
                BinaryExpression(
                  "\\in",
                  Identifier("x"),
                  Identifier("S")
                ),
                BinaryExpression(
                  "\\in",
                  Identifier("y"),
                  Identifier("S")
                ),
              )
            ),
            OperatorDefinition(
              NonFixLHS(
                Identifier("Next"),
                Nil
              ),
              BinaryExpression(
                "/\\",
                BinaryExpression(
                  "/\\",
                  BinaryExpression(
                    "\\in",
                    PrimedExpression(
                      Identifier("x")
                    ),
                    Identifier("S")
                  ),
                  BinaryExpression(
                    "\\in",
                    PrimedExpression(
                      Identifier("y")
                    ),
                    Identifier("S")
                  ),
                ),
                BinaryExpression(
                  "#",
                  PrimedExpression(
                    Identifier("x")
                  ),
                  PrimedExpression(
                    Identifier("y")
                  ),
                ),
              )
            ),
            OperatorDefinition(
              NonFixLHS(
                Identifier("Spec"),
                Nil
              ),
              BinaryExpression(
                "/\\",
                BinaryExpression(
                  "/\\",
                  Identifier("Init"),
                  UnaryExpression(
                    "[]",
                    WeakStuttering(
                      Identifier("Next"),
                      Tuple(
                        List(
                          Identifier("x"),
                          Identifier("y")
                        )
                      )
                    )
                  ),
                ),
                WeakFairness(
                  IdentifierTuple(
                    List(
                      Identifier("x"),
                      Identifier("y")
                    )
                  ),
                  Identifier("Next")
                )
              )
            ),
            Theorem(
              BinaryExpression(
                "=>",
                Identifier("Spec"),
                UnaryExpression(
                  "<>",
                  ParenthesizedExpression(
                    BinaryExpression(
                      "#",
                      Identifier("x"),
                      Identifier("y")
                    )
                  )
                )
              )
            )
          )
        ),
        ModuleDefinition(
          NonFixLHS(
            Identifier("I"),
            Nil
          ),
          Instance(
            Identifier("Inner"),
            List(
              Substitution(
                Identifier("y"),
                Identifier("x")
              )
            )
          )
        )
      )))
  }

  test("Parses enumerators with equal operator") {
    val inputSpec =
      """(A = 1)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseEnumerator().toOption.get
    assert(ast ==
      BoundedEnumerator(
        Identifier("A"),
        "=",
        Literal(1)
      )
    )
  }

  test("Parses enumerators with \\in operator") {
    val inputSpec =
      """(p \in 1..10)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseEnumerator().toOption.get
    assert(ast ==
      BoundedEnumerator(
        Identifier("p"),
        "\\in",
        BinaryExpression(
          "..",
          Literal(1),
          Literal(10)
        )
      )
    )
  }

  test("Parses enumerators with \\in operator and filter condition") {
    val inputSpec =
      """(j \in 1..10 : j # self)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseEnumerator().toOption.get
    assert(ast ==
      BoundedEnumeratorWithFilter(
        Identifier("j"),
        "\\in",
        BinaryExpression(
          "..",
          Literal(1),
          Literal(10)
        ),
        BinaryExpression(
          "#",
          Identifier("j"),
          Identifier("self")
        )
      )
    )
  }

  test("Parses channel definitions") {
    val inputSpec =
      """chan c1, c2""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseChannelDefinition().toOption.get
    assert(ast ==
      ChannelDefinition(List(Identifier("c1"), Identifier("c2")))
    )
  }

  test("Parses suspicious channel definitions") {
    val inputSpec =
      """suspchan c1, c2""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseSuspiciousChannelDefinition().toOption.get
    assert(ast ==
      SuspiciousChannelDefinition(List(Identifier("c1"), Identifier("c2")))
    )
  }

  test("Parses travelling messages") {
    val inputSpec =
      """c_{1 -> 2}<< "Hello" >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseTravellingMessage().toOption.get
    assert(ast ==
      TravellingMessage(
        Identifier("c_"),
        Literal(1),
        Literal(2),
        Literal("Hello")
      )
    )
  }

  test("Parses outgoing messages") {
    val inputSpec =
      """c@2<< "Hello" >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseOutgoingMessage().toOption.get
    assert(ast ==
      OutgoingMessage(
        Identifier("c"),
        Literal(2),
        Literal("Hello")
      )
    )
  }

  test("Parses outgoing messages (2)") {
    val inputSpec =
      """getChannel[i, j]@j<< [type |-> "request", clock |-> 0] >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseOutgoingMessage().toOption.get
    assert(ast ==
      OutgoingMessage(
        FunctionApplication(
          Identifier("getChannel"),
          List(
            Identifier("i"),
            Identifier("j")
          )
        ),
        Identifier("j"),
        ExplicitFunction(
          List(
            Mapping(Identifier("type"), Literal("request")),
            Mapping(Identifier("clock"), Literal(0))
          )
        )
      )
    )
  }

  test("Parses explicit multiset of travelling messages") {
    val inputSpec =
      """{{ c_{1 -> 2}<< "Hello" >>, c_{"a" -> "b"}<< "Hello again" >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExplicitMultiSetOfTravellingMessages().toOption.get
    assert(ast ==
      ExplicitMultiSet(
        List(
          TravellingMessage(
            Identifier("c_"),
            Literal(1),
            Literal(2),
            Literal("Hello")
          ),
          TravellingMessage(
            Identifier("c_"),
            Literal("a"),
            Literal("b"),
            Literal("Hello again")
          ),
        )
      )
    )
  }

  test("Parses explicit multiset of outgoing messages") {
    val inputSpec =
      """{{ c@2<<"Hello">>, c@"a"<< <<1,2,3>> >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseExplicitMultiSetOfOutgoingMessages().toOption.get
    assert(ast ==
      ExplicitMultiSet(
        List(
          OutgoingMessage(
            Identifier("c"),
            Literal(2),
            Literal("Hello")
          ),
          OutgoingMessage(
            Identifier("c"),
            Literal("a"),
            Tuple(
              List(
                Literal(1),
                Literal(2),
                Literal(3)
              )
            )
          ),
        )
      )
    )
  }

  test("Parses quantified multiset of travelling messages") {
    val inputSpec =
      """{{ (j \in 1..10) c_{1 -> j}<< "Hello" >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseQuantifiedMultiSetOfTravellingMessages().toOption.get
    assert(ast ==
      QuantifiedMultiSet(
        BoundedEnumerator(
          Identifier("j"),
          "\\in",
          BinaryExpression(
            "..",
            Literal(1),
            Literal(10)
          )
        ),
        TravellingMessage(
          Identifier("c_"),
          Literal(1),
          Identifier("j"),
          Literal("Hello")
        )
      )
    )
  }

  test("Parses quantified multiset of outgoing messages") {
    val inputSpec =
      """{{ (j \in 1..10 : j # self) c@j<< "Hello" >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseQuantifiedMultiSetOfOutgoingMessages().toOption.get
    assert(ast ==
      QuantifiedMultiSet(
        BoundedEnumeratorWithFilter(
          Identifier("j"),
          "\\in",
          BinaryExpression(
            "..",
            Literal(1),
            Literal(10)
          ),
          BinaryExpression(
            "#",
            Identifier("j"),
            Identifier("self")
          )
        ),
        OutgoingMessage(
          Identifier("c"),
          Identifier("j"),
          Literal("Hello")
        )
      )
    )
  }

  test("Parses variable declarations") {
    val inputSpec =
      """var x""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseDeclaration().toOption.get
    assert(ast ==
      Declaration(
        Identifier("x")
      )
    )
  }

  test("Parses variable assignments") {
    val inputSpec =
      """<< x := 10 >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseAssignment().toOption.get
    assert(ast ==
      Assignment(
        Identifier("x"),
        Literal(10)
      )
    )
  }

  test("Parses receive actions") {
    val inputSpec =
      """c@2?(x)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseReceive().toOption.get
    assert(ast ==
      Receive(
        Identifier("c"),
        Literal(2),
        Identifier("x")
      )
    )
  }

  test("Parses choices of multiple actions") {
    val inputSpec = """c1@1?(x) + c2@2?(x)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseChoice().toOption.get
    assert(ast == Choice(List(
          Receive(
            Identifier("c1"),
            Literal(1),
            Identifier("x")
          ),
          Receive(
            Identifier("c2"),
            Literal(2),
            Identifier("x")
          )
        )
      )
    )
  }

  test("Parses choices of multiple actions, given as an indexed sum") {
    val inputSpec = """(j \in 1..2) c1@j?(x)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseIndexedSumChoice().toOption.get
    assert(ast ==
      IndexedSumChoice(
        BoundedEnumerator(Identifier("j"), "\\in", BinaryExpression("..", Literal(1), Literal(2))),
        Receive(Identifier("c1"), Identifier("j"), Identifier("x"))
      )
    )
  }

  test("Parses send actions with explicit multiset") {
    val inputSpec =
      """{{ c@1<< 1 >>,  c@2<< 2 >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseSend().toOption.get
    assert(ast ==
      Send(
        ExplicitMultiSet(
          List(
            OutgoingMessage(
              Identifier("c"),
              Literal(1),
              Literal(1)
            ),
            OutgoingMessage(
              Identifier("c"),
              Literal(2),
              Literal(2)
            ),
          )
        )
      )
    )
  }

  test("Parses send actions with quantified multiset") {
    val inputSpec =
      """{{ (i \in 1..2 : i # self) c@i<< self >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseSend().toOption.get
    assert(ast ==
      Send(
        QuantifiedMultiSet(
          BoundedEnumeratorWithFilter(
            Identifier("i"),
            "\\in",
            BinaryExpression(
              "..",
              Literal(1),
              Literal(2)
            ),
            BinaryExpression(
              "#",
              Identifier("i"),
              Identifier("self")
            )
          ),
          OutgoingMessage(
            Identifier("c"),
            Identifier("i"),
            Identifier("self")
          )
        )
      )
    )
  }

  test("Parses empty processes") {
    val inputSpec =
      """ {{ }} """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          Send(ExplicitMultiSet(Nil))
        )
      )
    )
  }

  test("Parses skip threads") {
    val inputSpec =
      """ skip """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          Send(ExplicitMultiSet(Nil))
        )
      )
    )
  }

  test("Parses threads with declarations and assignments") {
    val inputSpec =
      """var x; << x := 10 >>""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          Declaration(
            Identifier("x")
          )
        ),
        ThreadUnit(
          None,
          Assignment(
            Identifier("x"),
            Literal(10)
          )
        )
      )
    )
  }

  test("Parses threads with declarations and receive actions") {
    val inputSpec =
      """var x; c@2?(x)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          Declaration(
            Identifier("x")
          )
        ),
        ThreadUnit(
          None,
          Receive(
            Identifier("c"),
            Literal(2),
            Identifier("x")
          )
        )
      )
    )
  }

  test("Parses threads with send actions") {
    val inputSpec =
      """ {{ c@2<<"Hello">> }} """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          Send(
            ExplicitMultiSet(
              List(
                OutgoingMessage(
                  Identifier("c"),
                  Literal(2),
                  Literal("Hello")
                )
              )
            )
          )
        )
      )
    )
  }

  test("Parses conditional threads") {
    val inputSpec =
      """if 1 = 1 then var x else var y fi""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          ConditionalThread(
            BinaryExpression(
              "=",
              Literal(1),
              Literal(1)
            ),
            List(
              ThreadUnit(
                None,
                Declaration(
                  Identifier("x")
                )
              )
            ),
            List(
              ThreadUnit(
                None,
                Declaration(
                  Identifier("y")
                )
              )
            )
          )
        )
      )
    )
  }

  test("Parses thread instantiations") {
    val inputSpec =
      """foo()""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThread().toOption.get
    assert(ast ==
      List(
        ThreadUnit(
          None,
          ThreadInstantiation(
            Identifier("foo")
          )
        )
      )
    )
  }

  test("Parses thread definitions with no free variables") {
    val inputSpec =
      """def declareXAndStop = var x""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThreadDefinition().toOption.get
    assert(ast ==
      ThreadDefinition(
        Identifier("declareXAndStop"),
        Nil,
        List(
          ThreadUnit(
            None,
            Declaration(
              Identifier("x")
            )
          )
        )
      )
    )
  }

  test("Parses thread definitions with free variables") {
    val inputSpec =
      """def assignXAndStop(x) = << x := 10 >> """.stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThreadDefinition().toOption.get
    assert(ast ==
      ThreadDefinition(
        Identifier("assignXAndStop"),
        List(
          Identifier("x")
        ),
        List(
          ThreadUnit(
            None,
            Assignment(
              Identifier("x"),
              Literal(10)
            )
          )
        )
      )
    )
  }

  test("Parses thread compositions") {
    val inputSpec =
      """(j = 1) (var x) | (k = 2) (var y) | (l = 3) (var z)""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseThreadComposition().toOption.get
    assert(ast ==
      ThreadComposition(
        ThreadComposition(
          ThreadComprehension(
            BoundedEnumerator(
              Identifier("j"),
              "=",
              Literal(1)
            ),
            List(
              ThreadUnit(
                None,
                Declaration(
                  Identifier("x")
                )
              )
            )
          ),
          ThreadComprehension(
            BoundedEnumerator(
              Identifier("k"),
              "=",
              Literal(2)
            ),
            List(
              ThreadUnit(
                None,
                Declaration(
                  Identifier("y")
                )
              )
            )
          )
        ),
        ThreadComprehension(
          BoundedEnumerator(
            Identifier("l"),
            "=",
            Literal(3)
          ),
          List(
            ThreadUnit(
              None,
              Declaration(
                Identifier("z")
              )
            )
          )
        )
      )
    )
  }

  test("Parses processes") {
    val inputSpec =
      """[[ [x |-> Initialized] <| (j = 1) (<< x := 10 >>) ]]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseProcess().toOption.get
    assert(ast ==
      Process(
        ExplicitFunction(
          List(
            Mapping(
              Identifier("x"),
              Identifier("Initialized")
            )
          )
        ),
        ThreadComprehension(
          BoundedEnumerator(
            Identifier("j"),
            "=",
            Literal(1)
          ),
          List(
            ThreadUnit(
              None,
              Assignment(
                Identifier("x"),
                Literal(10)
              ),
            )
          )
        )
      )
    )
  }

  test("Parses networks with single locations") {
    val inputSpec =
      """(p = 1) [[ [] <| (j = 1) ({{ }}) ]]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      Location(
        BoundedEnumerator(
          Identifier("p"),
          "=",
          Literal(1)
        ),
        Process(
          ExplicitFunction(Nil),
          ThreadComprehension(
            BoundedEnumerator(
              Identifier("j"),
              "=",
              Literal(1)
            ),
            List(
              ThreadUnit(
                None,
                Send(
                  ExplicitMultiSet(Nil)
                )
              )
            )
          )
        )
      )
    )
  }

  test("Parses networks with multiple locations") {
    val inputSpec =
      """(p \in 1..10) [[ [] <| (j = 1) ({{ }}) ]]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      Location(
        BoundedEnumerator(
          Identifier("p"),
          "\\in",
          BinaryExpression(
            "..",
            Literal(1),
            Literal(10)
          )
        ),
        Process(
          ExplicitFunction(Nil),
          ThreadComprehension(
            BoundedEnumerator(
              Identifier("j"),
              "=",
              Literal(1)
            ),
            List(
              ThreadUnit(
                None,
                Send(
                  ExplicitMultiSet(Nil)
                )
              )
            )
          )
        )
      )
    )
  }

  test("Parses networks with message aether") {
    val inputSpec =
      """{{ c_{1 -> 2}<< "Hello" >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      MessageAether(
        ExplicitMultiSet(
          List(
            TravellingMessage(
              Identifier("c_"),
              Literal(1),
              Literal(2),
              Literal("Hello")
            )
          )
        )
      )
    )
  }

  test("Parses network compositions") {
    val inputSpec =
      """(p = 1) [[ [] <| (j = 1) ({{ }}) ]] || {{ c_{1 -> 2}<< "Hello" >> }}""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      NetworkComposition(
        Location(
          BoundedEnumerator(
            Identifier("p"),
            "=",
            Literal(1)
          ),
          Process(
            ExplicitFunction(Nil),
            ThreadComprehension(
              BoundedEnumerator(
                Identifier("j"),
                "=",
                Literal(1)
              ),
              List(
                ThreadUnit(
                  None,
                  Send(
                    ExplicitMultiSet(Nil)
                  )
                )
              )
            )
          )
        ),
        MessageAether(
          ExplicitMultiSet(
            List(
              TravellingMessage(
                Identifier("c_"),
                Literal(1),
                Literal(2),
                Literal("Hello")
              )
            )
          )
        )
      )
    )
  }

  test("Parses network compositions (left-associatively)") {
    val inputSpec =
      """(p = 1) [[ [] <| (j = 1) ({{ }}) ]] || {{ c_{1 -> 2}<< "Hello" >> }} || (q = 2) [[ [] <| (j = 1) ({{ }}) ]]""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      NetworkComposition(
        NetworkComposition(
          Location(
            BoundedEnumerator(
              Identifier("p"),
              "=",
              Literal(1)
            ),
            Process(
              ExplicitFunction(Nil),
              ThreadComprehension(
                BoundedEnumerator(
                  Identifier("j"),
                  "=",
                  Literal(1)
                ),
                List(
                  ThreadUnit(
                    None,
                    Send(
                      ExplicitMultiSet(Nil)
                    )
                  )
                )
              )
            )
          ),
          MessageAether(
            ExplicitMultiSet(
              List(
                TravellingMessage(
                  Identifier("c_"),
                  Literal(1),
                  Literal(2),
                  Literal("Hello")
                )
              )
            )
          )
        ),
        Location(
          BoundedEnumerator(
            Identifier("q"),
            "=",
            Literal(2)
          ),
          Process(
            ExplicitFunction(Nil),
            ThreadComprehension(
              BoundedEnumerator(
                Identifier("j"),
                "=",
                Literal(1)
              ),
              List(
                ThreadUnit(
                  None,
                  Send(
                    ExplicitMultiSet(Nil)
                  )
                )
              )
            )
          )
        ),
      )
    )
  }

  test("Parses network compositions with parentheses") {
    val inputSpec =
      """(p = 1) [[ [] <| (j = 1) ({{ }}) ]] || ( {{ c_{1 -> 2}<< "Hello" >> }} || (q = 2) [[ [] <| (j = 1) ({{ }}) ]] )""".stripMargin

    val ast = new Parser(Tokenizer(Scanner(inputSpec))).parseNetwork().toOption.get
    assert(ast ==
      NetworkComposition(
        Location(
          BoundedEnumerator(
            Identifier("p"),
            "=",
            Literal(1)
          ),
          Process(
            ExplicitFunction(Nil),
            ThreadComprehension(
              BoundedEnumerator(
                Identifier("j"),
                "=",
                Literal(1)
              ),
              List(
                ThreadUnit(
                  None,
                  Send(
                    ExplicitMultiSet(Nil)
                  )
                )
              )
            )
          )
        ),
        NetworkComposition(
          MessageAether(
            ExplicitMultiSet(
              List(
                TravellingMessage(
                  Identifier("c_"),
                  Literal(1),
                  Literal(2),
                  Literal("Hello")
                )
              )
            )
          ),
          Location(
            BoundedEnumerator(
              Identifier("q"),
              "=",
              Literal(2)
            ),
            Process(
              ExplicitFunction(Nil),
              ThreadComprehension(
                BoundedEnumerator(
                  Identifier("j"),
                  "=",
                  Literal(1)
                ),
                List(
                  ThreadUnit(
                    None,
                    Send(
                      ExplicitMultiSet(Nil)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("Parses EPC specifications") {
    val inputSpec =
      """
        |(* --EPC Consensus
        | chan c
        |
        | (l \in 1..n) [[
        |   [ channel |-> c,
        |     x |-> input[l],
        |     r |-> 0,
        |     output |-> Initialized ] <| (j = 1) ( L() ) ]]
        |
        | def L(channel, x, r, output) =
        |   << r := r + 1 >>;
        |   if r <= n
        |     then if self = r
        |             then {{ ( j \in 1..n : j # self) channel@j<< x >> }}; L()
        |             else channel@r?(x); L()
        |          fi
        |     else << output := x>>
        |   fi
        |
        |*)
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseEPCSpec() match {
      case Right(ast) => assert(ast ==
        EPCSpec(
          Identifier("Consensus"),
          Nil,
          List(
            ChannelDefinition(
              List(Identifier("c"))
            ),
            Location(
              BoundedEnumerator(
                Identifier("l"),
                "\\in",
                BinaryExpression(
                  "..",
                  Literal(1),
                  Identifier("n")
                )
              ),
              Process(
                ExplicitFunction(
                  List(
                    Mapping(Identifier("channel"), Identifier("c")),
                    Mapping(
                      Identifier("x"),
                      FunctionApplication(
                        Identifier("input"),
                        List(
                          Identifier("l")
                        )
                      )
                    ),
                    Mapping(Identifier("r"), Literal(0)),
                    Mapping(Identifier("output"), Identifier("Initialized"))
                  )
                ),
                ThreadComprehension(
                  BoundedEnumerator(
                    Identifier("j"),
                    "=",
                    Literal(1)
                  ),
                  List(
                    ThreadUnit(
                      None,
                      ThreadInstantiation(Identifier("L"))
                    )
                  )
                )
              )
            ),
            ThreadDefinition(
              Identifier("L"),
              List(
                Identifier("channel"),
                Identifier("x"),
                Identifier("r"),
                Identifier("output")
              ),
              List(
                ThreadUnit(
                  None,
                  Assignment(
                    Identifier("r"),
                    BinaryExpression(
                      "+",
                      Identifier("r"),
                      Literal(1)
                    )
                  )
                ),
                ThreadUnit(
                  None,
                  ConditionalThread(
                    BinaryExpression(
                      "<=",
                      Identifier("r"),
                      Identifier("n")
                    ),
                    List(
                      ThreadUnit(
                        None,
                        ConditionalThread(
                          BinaryExpression(
                            "=",
                            Identifier("self"),
                            Identifier("r"),
                          ),
                          List(
                            ThreadUnit(
                              None,
                              Send(
                                // {{ ( j \in 1..n : i # self) channel@j<< x >> }}
                                QuantifiedMultiSet(
                                  BoundedEnumeratorWithFilter(
                                    Identifier("j"),
                                    "\\in",
                                    BinaryExpression(
                                      "..",
                                      Literal(1),
                                      Identifier("n")
                                    ),
                                    BinaryExpression(
                                      "#",
                                      Identifier("j"),
                                      Identifier("self")
                                    )
                                  ),
                                  OutgoingMessage(
                                    Identifier("channel"),
                                    Identifier("j"),
                                    Identifier("x")
                                  )
                                )
                              )
                            ),
                            ThreadUnit(
                              None,
                              ThreadInstantiation(
                                Identifier("L")
                              )
                            )
                          ),
                          // channel@r?(x); L()
                          List(
                            ThreadUnit(
                              None,
                              Receive(
                                Identifier("channel"),
                                Identifier("r"),
                                Identifier("x")
                              )
                            ),
                            ThreadUnit(
                              None,
                              ThreadInstantiation(
                                Identifier("L")
                              )
                            )
                          )
                        )
                      )
                    ),
                    // << output := x >>
                    List(
                      ThreadUnit(
                        None,
                        Assignment(
                          Identifier("output"),
                          Identifier("x")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      case Left(value) => fail(value)
    }
  }


}
