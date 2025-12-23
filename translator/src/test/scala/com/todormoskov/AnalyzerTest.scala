package com.todormoskov

import com.todormoskov.Lexer.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

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
      case Right(ast) => Analyzer.analyze(ast) match {
        case Right(epcCtx) => epcCtx.visibleTLAOperators == mutable.Set[String]("N", "SuccessorOfN", "successor")
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }

  test("Control flow analyzer computes edges between commands and conditional threads") {
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
        |       else << x := x + 1 >>
        |     fi;
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
            ) ==
              (
                List(
                  LeafOperation(1, None, Declaration(Identifier("x"))),
                  LeafOperation(2, None, Assignment(Identifier("x"), Literal(10))),
                  LeafOperation(3, None, Send(ExplicitMultiSet(List(OutgoingMessage(Identifier("c"), Literal(2), Identifier("x")))))),
                  LeafOperation(4, None, Receive(Identifier("c"), Literal(2), Identifier("x"))),
                  IfOperation(5, None, BinaryExpression("=", Identifier("x"), Literal(10)),
                    List(LeafOperation(6, None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1))))),
                    List(LeafOperation(7, None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1)))))
                  ),
                  LeafOperation(8, None, Assignment(Identifier("output"), Identifier("x")))
                ),
                List(
                  DiEdge(0, 1, None),
                  DiEdge(1, 2, None),
                  DiEdge(2, 3, None),
                  DiEdge(3, 4, None),
                  DiEdge(4, 5, None),
                  DiEdge(5, 6, Some("then")),
                  DiEdge(5, 7, Some("else")),
                  DiEdge(6, 8, None),
                  DiEdge(7, 8, None),
                  DiEdge(8, -1, None),
                )
              )
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }

  test("Control flow analyzer skips empty send actions while computing edges.") {
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
                (
                  List(
                    LeafOperation(1, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(2, None, Declaration(Identifier("x"))),
                    LeafOperation(3, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(4, None, Assignment(Identifier("x"), Literal(10))),
                    LeafOperation(5, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(6, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(7, None, Assignment(Identifier("x"), Literal(10))),
                    LeafOperation(8, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(9, None, Send(ExplicitMultiSet(List()))), // empty send
                    LeafOperation(10, None, Send(ExplicitMultiSet(List()))), // empty send
                  ),
                  List(
                    DiEdge(0, 2, None),
                    DiEdge(1, 2, None),
                    DiEdge(2, 4, None),
                    DiEdge(3, 4, None),
                    DiEdge(4, 7, None),
                    DiEdge(5, 7, None),
                    DiEdge(6, 7, None),
                    DiEdge(7, -1, None),
                    DiEdge(8, -1, None),
                    DiEdge(9, -1, None),
                    DiEdge(10, -1, None),
                  )
                )
            )
          case Left(value) => fail(value)
        }
      case Left(value) => fail(value)
    }
  }

  test("Control flow analyzer computes edges within and around nested conditional threads") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | (p = 1) [[
        |   [ x |-> 0, y |-> 0 ]
        |   <|
        |   (j = 1) (
        |     if x = 1
        |       then if y = 2
        |               then << x := 3 >>;
        |                    << y := 4 >>
        |               else << x := 5 >>;
        |                    << y := 6 >>
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
              (
                List(
                  IfOperation(1, None, BinaryExpression("=", Identifier("x"), Literal(1)),
                    List(
                      IfOperation(2, None, BinaryExpression("=", Identifier("y"), Literal(2)),
                        List(
                          LeafOperation(3, None, Assignment(Identifier("x"), Literal(3))),
                          LeafOperation(4, None, Assignment(Identifier("y"), Literal(4)))
                        ),
                        List(
                          LeafOperation(5, None, Assignment(Identifier("x"), Literal(5))),
                          LeafOperation(6, None, Assignment(Identifier("y"), Literal(6)))
                        )
                      ),
                      LeafOperation(7, None, Assignment(Identifier("x"), Literal(7))),
                      LeafOperation(8, None, Assignment(Identifier("y"), Literal(8)))
                    ),
                    List(
                      LeafOperation(9, None, Assignment(Identifier("x"), Literal(9))),
                      LeafOperation(10, None, Assignment(Identifier("y"), Literal(10)))
                    )
                  ),
                  LeafOperation(11, None, Assignment(Identifier("x"), Literal(11)))
                ),
                List(
                  DiEdge(0, 1, None),
                  DiEdge(1, 2, Some("then")),
                  DiEdge(1, 9, Some("else")),
                  DiEdge(2, 3, Some("then")),
                  DiEdge(2, 5, Some("else")),
                  DiEdge(3, 4, None),
                  DiEdge(4, 7, None),
                  DiEdge(5, 6, None),
                  DiEdge(6, 7, None),
                  DiEdge(7, 8, None),
                  DiEdge(8, 11, None),
                  DiEdge(9, 10, None),
                  DiEdge(10, 11, None),
                  DiEdge(11, -1, None),
                )
              )
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }

  test("Control flow analyzer skips empty send actions while computing within and around nested conditional threads") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | (p = 1) [[
        |   [ x |-> 0, y |-> 0 ]
        |   <|
        |   (j = 1) (
        |     if x = 1
        |       then if y = 2
        |               then {{ }};
        |                    << y := 4 >>
        |               else << x := 5 >>;
        |                    {{ }}
        |            fi;
        |            << x := 7 >>;
        |            {{ }}
        |       else {{ }}
        |     fi;
        |     {{ }};
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
              (
                List(
                  IfOperation(1, None, BinaryExpression("=", Identifier("x"), Literal(1)),
                    List(
                      IfOperation(2, None, BinaryExpression("=", Identifier("y"), Literal(2)),
                        List(
                          LeafOperation(3, None, Send(ExplicitMultiSet(Nil))),
                          LeafOperation(4, None, Assignment(Identifier("y"), Literal(4)))
                        ),
                        List(
                          LeafOperation(5, None, Assignment(Identifier("x"), Literal(5))),
                          LeafOperation(6, None, Send(ExplicitMultiSet(Nil)))
                        )
                      ),
                      LeafOperation(7, None, Assignment(Identifier("x"), Literal(7))),
                      LeafOperation(8, None, Send(ExplicitMultiSet(Nil)))
                    ),
                    List(
                      LeafOperation(9, None, Send(ExplicitMultiSet(Nil))),
                    )
                  ),
                  LeafOperation(10, None, Send(ExplicitMultiSet(Nil))),
                  LeafOperation(11, None, Assignment(Identifier("x"), Literal(11)))
                ),
                List(
                  DiEdge(0, 1, None),
                  DiEdge(1, 2, Some("then")),
                  DiEdge(1, 11, Some("else")),
                  DiEdge(2, 4, Some("then")),
                  DiEdge(2, 5, Some("else")),
                  DiEdge(3, 4, None),
                  DiEdge(4, 7, None),
                  DiEdge(5, 7, None),
                  DiEdge(6, 7, None),
                  DiEdge(7, 11, None),
                  DiEdge(8, 11, None),
                  DiEdge(9, 11, None),
                  DiEdge(10, 11, None),
                  DiEdge(11, -1, None),
                )
              )
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }

  test("Control flow analyzer computes edges in defined threads and their instantiations") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | def incrementX(x) = << x := x + 1 >>; incrementX()
        |
        | (p = 1) [[
        |   [x |-> 0] <|
        |   (j = 1) ( incrementX() )
        | ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast) match {
        case Right(epcCtx) =>
          assert(
            epcCtx.thdDefinitionsToOperations(Identifier("incrementX")) ==
              (
                List(
                  LeafOperation(1, None, Assignment(Identifier("x"), BinaryExpression("+", Identifier("x"), Literal(1)))),
                  LeafOperation(2, None, ThreadInstantiation(Identifier("incrementX")))
                ),
                List(
                  DiEdge(0, 1, None),
                  DiEdge(1, 2, None),
                  DiEdge(2, -1, Some("incrementX")),
                )
              )
          )
          assert(
            epcCtx.locThdEnumsToOperations(
              (BoundedEnumerator(Identifier("p"), "=", Literal(1)), BoundedEnumerator(Identifier("j"), "=", Literal(1)))
            ) ==
              (
                List(
                  LeafOperation(1, None, ThreadInstantiation(Identifier("incrementX")))
                ),
                List(
                  DiEdge(0, 1, None),
                  DiEdge(1, -1, Some("incrementX")),
                )
              )
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }

  test("Analyzer computes the outgoing bag mappings (operation ID to bag of messages)") {
    val inputSpec =
      """
        | ---- MODULE TlaModuleWithEPCSpec ----
        | EXTENDS Integers
        |
        | CONSTANTS N
        |
        | (* --EPC SomeEPCSpec
        | new c
        |
        | def sendMessages(x) = {{ (j \in 1..N) c@j<< x >> }}
        |
        | (p = 0) [[
        |   [x |-> 0] <|
        |   (j = 0) (
        |     {{ c@1<< "Hello" >> }};
        |     {{ c@1<< "Hello" >>, c@1<< "Hello" >> }};
        |     sendMessages()
        |   )
        | ]]
        | || (q \in 1..N) [[ [] <| (j = 1) ( {{ }} ) ]]
        |
        | *)
        | ====
        |""".stripMargin

    new Parser(Tokenizer(Scanner(inputSpec))).parseModule() match {
      case Right(ast) => Analyzer.analyze(ast) match {
        case Right(epcCtx) =>
          assert(epcCtx.thdDefinitionsToOutgoingBagMappings.size == 1)
          assert(
            epcCtx.thdDefinitionsToOutgoingBagMappings(Identifier("sendMessages")) ==
              mutable.LinkedHashMap[Int, MultiSet[OutgoingMessage]]
                // The first action (with ID = 1) of the defined thread (with the name "sendMessages") is sending these messages
                (1 ->
                  QuantifiedMultiSet(
                    BoundedEnumerator(
                      Identifier("j"),
                      "\\in",
                      BinaryExpression(
                        "..",
                        Literal(1),
                        Identifier("N")
                      )
                    ),
                    OutgoingMessage(
                      Identifier("c"),
                      Identifier("j"),
                      Identifier("x")
                    )
                  )
                )
          )
          // We expect that both (location, thread) enumerators are mapped to their outgoing bag mappings
          assert(epcCtx.locThdEnumsToOutgoingBagMappings.size == 2)
          // The pair ("p", "j") does not contain the messages from the first action (because it is only one).
          // The pair ("p", "j") contains the messages from the second action (because they are more than one).
          assert(
            epcCtx.locThdEnumsToOutgoingBagMappings(
              (BoundedEnumerator(Identifier("p"), "=", Literal(0)), BoundedEnumerator(Identifier("j"), "=", Literal(0)))
            ) ==
              mutable.LinkedHashMap[Int, MultiSet[OutgoingMessage]]
                // The second action of the (location, thread) pair (with ID = 2) is sending these messages
                (2 ->
                  ExplicitMultiSet(
                    List(
                      OutgoingMessage(
                        Identifier("c"),
                        Literal(1),
                        Literal("Hello")
                      ),
                      OutgoingMessage(
                        Identifier("c"),
                        Literal(1),
                        Literal("Hello")
                      )
                    )
                  )
                )
          )
          // The pair ("q", "j") does not contain any outgoing bag mappings on its own.
          assert(
            epcCtx.locThdEnumsToOutgoingBagMappings(
              (BoundedEnumerator(Identifier("q"), "\\in", BinaryExpression("..", Literal(1), Identifier("N"))), BoundedEnumerator(Identifier("j"), "=", Literal(1)))
            ) ==
              mutable.LinkedHashMap.empty[Int, MultiSet[OutgoingMessage]]
          )
        case Left(value) => fail(value)
      }
      case Left(value) => fail(value)
    }
  }


}
