package com.todormoskov

import com.todormoskov.Lexer.*

import org.scalatest.funsuite.AnyFunSuite

class AnalyzerTest extends AnyFunSuite {

  test("Analyzer sees definitions before EPC spec") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        |
        | EXTENDS Integers
        |
        | CONSTANTS N
        |
        | successorOfN == N + 1
        |
        | successor[n \in Nat] == n + 1
        |
        | (* --EPC SomeEPCSpec
        |
        | (p = 0) [[
        |   [x |-> 10] <| (j = 1) ( << x := x + 1 >> ) | (k = 2) ( << x := x + 1 >> )
        | ]]
        | ||
        | (q \in 1..N) [[
        |   [y |-> 100] <| (m \in 1..N) ( << y := y + 1 >> ) | (n \in (N+1)..(N*2)) ( << y := y + 1 >> )
        | ]]
        | ||
        | (r = N + 1) [[
        |   [z |-> 100] <| (s \in 1..N) ( << z := z + 1 >> ) | (t \in (N+1)..(N*2)) ( << z := z + 1 >> )
        | ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast)
      case Left(value) => fail(value)
    }
  }

  test("Analyzer recognizes that empty send actions should be skipped.") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | (p = 1) [[
        |   [] <|
        |   (j = 1) (
        |     {{ }};
        |     var x;
        |     {{ }};
        |     << x := 10 >>;
        |     {{ }};
        |     {{ }};
        |     << x := 10 >>;
        |     {{ }};
        |     {{ }};
        |     {{ }}
        |   )
        | ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast) match {
          case Right(epcCtx) =>
            assert(
              epcCtx.locThdEnumsToOperations(
                (BoundedEnumerator(Identifier("p"), "=", Literal(1)), BoundedEnumerator(Identifier("j"), "=", Literal(1)))
              ) ==
                List(
                  LeafOperation(1, List(2), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(2, List(4), ThreadUnit(None, Declaration(Identifier("x")))),
                  LeafOperation(3, List(4), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(4, List(7), ThreadUnit(None, Assignment(Identifier("x"), Literal(10)))),
                  LeafOperation(5, List(7), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(6, List(7), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(7, List(-1), ThreadUnit(None, Assignment(Identifier("x"), Literal(10)))),
                  LeafOperation(8, List(-1), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(9, List(-1), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                  LeafOperation(10, List(-1), ThreadUnit(None, Send(ExplicitMultiSet(List())))), // empty send
                )
            )
          case Left(value) => fail(value)
        }
      case Left(value) => fail(value)
    }
  }

  test("Analyzer recognizes the control flow of conditional threads") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | (p = 1) [[
        |   [x |-> 0, y |-> 0] <|
        |   (j = 1) (
        |     if x = 1
        |       then if y = 2
        |               then << x := 3 >>; << y := 4 >>
        |               else << x := 5 >>; << y := 6 >>
        |            fi;
        |            << x := 7 >>;
        |            << y := 8 >>
        |       else << x := 9 >>;
        |            << y := 10 >>
        |     fi;
        |     << x := 11 >>
        |   )
        | ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast) match {
        case Right(epcCtx) =>
          assert(
            epcCtx.locThdEnumsToOperations(
              (BoundedEnumerator(Identifier("p"), "=", Literal(1)), BoundedEnumerator(Identifier("j"), "=", Literal(1)))
            ) ==
              List(
                CompositeOperation(1, List(2, 9), ThreadUnit(None, ConditionalThread(BinaryExpression("=", Identifier("x"), Literal(1)), List(ThreadUnit(None, ConditionalThread(BinaryExpression("=", Identifier("y"), Literal(2)), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(3))), ThreadUnit(None, Assignment(Identifier("y"), Literal(4)))), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(5))), ThreadUnit(None, Assignment(Identifier("y"), Literal(6)))))), ThreadUnit(None, Assignment(Identifier("x"), Literal(7))), ThreadUnit(None, Assignment(Identifier("y"), Literal(8)))), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(9))), ThreadUnit(None, Assignment(Identifier("y"), Literal(10)))))),
                  List(
                    List(
                      CompositeOperation(2, List(3, 5), ThreadUnit(None, ConditionalThread(BinaryExpression("=", Identifier("y"), Literal(2)), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(3))), ThreadUnit(None, Assignment(Identifier("y"), Literal(4)))), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(5))), ThreadUnit(None, Assignment(Identifier("y"), Literal(6)))))),
                        List(
                          List(
                            LeafOperation(3, List(4), ThreadUnit(None, Assignment(Identifier("x"), Literal(3)))),
                            LeafOperation(4, List(7), ThreadUnit(None, Assignment(Identifier("y"), Literal(4))))
                          ),
                          List(
                            LeafOperation(5, List(6), ThreadUnit(None, Assignment(Identifier("x"), Literal(5)))),
                            LeafOperation(6, List(7), ThreadUnit(None, Assignment(Identifier("y"), Literal(6))))
                          )
                        )
                      ),
                      LeafOperation(7, List(8), ThreadUnit(None, Assignment(Identifier("x"), Literal(7)))),
                      LeafOperation(8, List(11), ThreadUnit(None, Assignment(Identifier("y"), Literal(8))))
                    ),
                    List(
                      LeafOperation(9, List(10), ThreadUnit(None, Assignment(Identifier("x"), Literal(9)))),
                      LeafOperation(10, List(11), ThreadUnit(None, Assignment(Identifier("y"), Literal(10))))
                    )
                  )
                ),
                LeafOperation(11, List(-1), ThreadUnit(None, Assignment(Identifier("x"), Literal(11)))))
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }


  test("Analyzer recognizes a the control flow of actions") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | (p = 1) [[
        |   [] <|
        |   (j = 1) (
        |     var x;
        |     << x := 10 >>;
        |     {{ c@2<< x >> }};
        |     c@2?(x);
        |     if x = 10
        |       then << x := x + 1 >>
        |       else << x := 11 >>
        |     fi;
        |     {{ }};
        |     << output := x >>
        |   )
        | ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast) match {
        case Right(epcCtx) =>
          assert(
            epcCtx.locThdEnumsToOperations(
              (BoundedEnumerator(Identifier("p"), "=", Literal(1)), BoundedEnumerator(Identifier("j"), "=", Literal(1)))
            ) == List(
              List(
                LeafOperation(1, List(2), ThreadUnit(None, Declaration(Identifier("x")))),
                LeafOperation(2, List(3), ThreadUnit(None, Assignment(Identifier("x"), Literal(10)))),
                LeafOperation(3, List(4), ThreadUnit(None, Send(ExplicitMultiSet(List(OutgoingMessage(Identifier("c"), Literal(2), Identifier("x"))))))),
                LeafOperation(4, List(5), ThreadUnit(None, Receive(Identifier("c"), Literal(2), Identifier("x")))),
                CompositeOperation(5, List(6, 7), ThreadUnit(None,
                  ConditionalThread(BinaryExpression("=", Identifier("x"), Literal(10)), List(ThreadUnit(None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1))))), List(ThreadUnit(None, Assignment(Identifier("x"), Literal(11)))))),
                  List(
                    List(LeafOperation(6, List(9), ThreadUnit(None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1)))))),
                    List(LeafOperation(6, List(9), ThreadUnit(None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1))))))
                  )
                ),
                LeafOperation(8, List(9), ThreadUnit(None, Send(ExplicitMultiSet(List())))),
                LeafOperation(9, List(-1), ThreadUnit(None, Assignment(Identifier("output"), Identifier("x")))))
            )
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }


}
