package com.todormoskov

import com.todormoskov.Lexer.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait AST
/**
 * ModuleUnit trait for the TLA+ declarations and definitions.
 *
 * ModuleUnit := VariableDeclaration
 *            | ConstantDeclaration
 *            | ['LOCAL']? OperatorDefinition
 *            | ['LOCAL']? FunctionDefinition
 *            | ['LOCAL']? Instance
 *            | ['LOCAL']? ModuleDefinition
 *            | Assumption
 *            | Theorem
 *            | Module
 *            | MinusSeparator ( = AtLeast4("-"))
 */
sealed trait ModuleUnit extends AST
/**
 * An AST representation of a TLA+ module.
 *
 * Module :=      MinusSeparator 'MODULE' Identifier MinusSeparator
 *                ['EXTENDS' CommaList(Identifier)]?
 *                ModuleUnit*
 *                EqualSeparator
 */
case class Module(identifier: Identifier, extendsStmt: ExtendsStatement, moduleUnits: List[ModuleUnit]) extends ModuleUnit
/**
 * A module extends statement. Contains the list with extended modules.
 *
 * ExtendsStatement := ['EXTENDS' CommaList(Identifier)]?
 *
 */
case class ExtendsStatement(extendedModules: List[Identifier]) extends AST
/**
 * Variable declarations.
 *
 * VariableDeclaration := ['VARIABLE' | 'VARIABLES'] CommaList(Identifier)
 */
case class VariableDeclaration(variableToken: Token, variables: List[Identifier]) extends ModuleUnit
/**
 * Constant declarations.
 *
 * ConstantDeclaration := ['CONSTANT' | 'CONSTANTS'] CommaList(OperatorDeclaration)
 */
case class ConstantDeclaration(constantToken: Token, constants: List[OperatorDeclaration]) extends ModuleUnit
/**
 * The declaration of a single operator.
 *
 * OperatorDeclaration := Identifier
 *                      | Identifier '(' CommaList('_') ')'
 *                      | PrefixOp '_'
 *                      | '_' InfixOp '_'
 *                      | '_' PostfixOp
 *
 * Prefix, infix and postfix operator declarations are currently not supported!
 */
case class OperatorDeclaration(operator: Identifier | Token, arguments: List[Token]) extends AST
/**
 * Local definitions.
 *
 * LocalDefinition := ['LOCAL']? OperatorDefinition
 *                 | ['LOCAL']? FunctionDefinition
 *                 | ['LOCAL']? Instance
 *                 | ['LOCAL']? ModuleDefinition
 */
case class LocalDefinition(definition: OperatorDefinition | FunctionDefinition | Instance | ModuleDefinition) extends ModuleUnit
/**
 * OperatorDefinition :=  ( NonFixLHS
 *                        | PrefixOp Identifier
 *                        | Identifier InfixOp Identifier
 *                        | Identifier PostfixOp )
 *                        '=='
 *                        Expression
 *
 * Prefix, infix and postfix operator definitions are currently not supported!
 */
case class OperatorDefinition(signature: NonFixLHS, body: Expression) extends ModuleUnit
/**
 * Non-fix left-hand-side of an operator definition.
 *
 * NonFixLHS := Identifier ['(' CommaList(Identifier | OperatorDeclaration) ')']?
 */
case class NonFixLHS(identifier: Identifier, arguments: List[Identifier | OperatorDeclaration]) extends AST
/**
 * FunctionDefinition :=  Identifier '[' CommaList(QuantifierBound) ']' '==' Expression
 */
case class FunctionDefinition(identifier: Identifier, arguments: List[QuantifierBound], body: Expression) extends ModuleUnit
/**
 * Instance := 'INSTANCE' Identifier ['WITH' CommaList(Substitution)]
 */
case class Instance(identifier: Identifier, substitutions: List[Substitution]) extends ModuleUnit
/**
 * Substitution := (Identifier | PrefixOp | InfixOp | PostfixOp) '<-' Argument
 */
case class Substitution(identifier: Identifier | Token, arguments: Expression) extends AST
/**
 * Single part of an instance prefix.
 *
 * InstancePrefixPart := Identifier [ '(' CommaList(Expression) ')' ]? '!'
 *
 * Examples: M!       -> InstancePrefixPart(Identifier("M"), Nil)
 *           M(a, b)! -> InstancePrefixPart(Identifier("M"), List(Identifier("a"), Identifier("b"))
 */
case class InstancePrefixPart(identifier: Identifier, arguments: List[Expression]) extends AST
/**
 * The whole instance prefix is a list of instance prefix parts.
 *
 * InstancePrefix := InstancePrefixPart*
 *
 * Examples: A(arg1)! B! C(arg2)! -> List(InstancePrefixPart(Identifier("A"), List(Identifier("arg1"))),
 *                                        InstancePrefixPart(Identifier("B"), List()),
 *                                        InstancePrefixPart(Identifier("C"), List(Identifier("arg2")))
 *                                   )
 */
type InstancePrefix = List[InstancePrefixPart]
/**
 * General identifier, which contains an optional prefix and a base identifier.
 * General identifiers are also expressions themselves!
 */
case class GeneralIdentifier(prefix: InstancePrefix, identifier: Identifier) extends Expression
/**
 * Prefix operator (such as -, ~, \lnot, \neg, [], <>, DOMAIN, ENABLED, SUBSET, UNCHANGED, UNION)
 * with an instance prefix.
 * General prefix operators are not expressions on their own, only AST nodes!
 *
 * Examples: M!-, M!~, ...
 */
case class GeneralPrefixOp(prefix: InstancePrefix, prefixOp: String) extends AST
/**
 * Infix operator with an instance prefix.
 * General infix operators are not expressions on their own, only AST nodes!
 *
 * Examples: M!++, ...
 */
case class GeneralInfixOp(prefix: InstancePrefix, infixOp: String) extends AST
/**
 * Postfix operator with an instance prefix.
 * General postfix operators are not expressions on their own, only AST nodes!
 */
case class GeneralPostfixOp(prefix: InstancePrefix, postfixOp: String) extends AST
/**
 * ModuleDefinition := NonFixLHS '==' Instance
 */
case class ModuleDefinition(signature: NonFixLHS, instance: Instance) extends ModuleUnit
/**
 * Assumption ::= ['ASSUME' | 'ASSUMPTION' | 'AXIOM'] Expression
 */
case class Assumption(token: Token, expression: Expression) extends ModuleUnit
/**
 * Theorem ::= 'THEOREM' Expression
 */
case class Theorem(expression: Expression) extends ModuleUnit
/**
 * Minus (= AtLest4('-')) or equal separator(= AtLest4('=')).
 */
case class Separator(token: Token) extends ModuleUnit

/**
 * Expression trait.
 */
sealed trait Expression extends AST
/**
 * Literal value. E.g. TRUE, FALSE, BOOLEAN, 1, 2, 3, ..., "a", "aa", ...
 */
case class Literal(value: String |  Integer | Boolean) extends Expression
/**
 * Single identifier. E.g. x or y.
 */
case class Identifier(name: String) extends Expression
/**
 * TLA+ tuple with identifiers. E.g. << x, y >>.
 */
case class IdentifierTuple(identifiers: List[Identifier]) extends Expression
/**
 * Examples for quantifier bounds: "n \in Nat", "n, m \in Nat" or "<< n, m >> \in Nat \times Nat".
 */
case class QuantifierBound(variable: Identifier | List[Identifier] | IdentifierTuple, domain: Expression) extends Expression
/**
 * An identifier to expression mapping. E.g. x |-> 1 or x : 1..10.
 */
case class Mapping(identifier: Identifier, expression: Expression) extends Expression
/**
 * A TLA+ tuple of expressions. E.g. << 1, 2, 3 >>.
 */
case class Tuple(values: List[Expression]) extends Expression
/**
 * Unary expressions. E.g. -1, -(1+1).
 */
case class UnaryExpression(operator: String, expression: Expression) extends Expression
/**
 * Binary expressions. E.g. 1 + 1, x * x.
 */
case class BinaryExpression(operator: String, lhs: Expression, rhs: Expression) extends Expression
/**
 * Identifier application expressions. E.g. f(1, 2, 3).
 */
case class IdentifierApplication(identifier: Identifier, operands: List[Expression]) extends Expression
/**
 * Operator application expressions. E.g. M!f(1, 2, 3) or M(a, b)!(1, 2, 3)
 */
case class GeneralIdentifierApplication(identifier: GeneralIdentifier, operands: List[Expression]) extends Expression
/**
 * Function application expressions. E.g. f[1, 2, 3].
 */
case class FunctionApplication(expression: Expression, operands: List[Expression]) extends Expression
/**
 * A primed expression. E.g. x'
 */
case class PrimedExpression(expression: Expression) extends Expression
/**
 * Expression with parenthesis around it.
 */
case class ParenthesizedExpression(expression: Expression) extends Expression
/**
 * TLA+ IF-THEN_ELSE expression.
 */
case class IfThenElse(condition: Expression, thenExpr: Expression, elseExpr: Expression) extends Expression
/**
 * TLA+ CASE expression.
 *
 * CaseExpression := 'CASE' CaseArm [ '[]' CaseArm ]* [ '[]' 'OTHER' '->' Expression ]
 */
case class CaseExpression(caseArms: List[CaseArm], defaultExpression: Option[Expression]) extends Expression
/**
 * TLA+ case arm expression.
 *
 * CaseArm := Expression '->' Expression
 */
case class CaseArm(lhs: Expression, rhs: Expression) extends AST
/**
 * TLA+ CHOOSE expression.
 */
case class ChooseExpression(variable: Identifier | IdentifierTuple, domain: Expression, predicate: Expression) extends Expression
/**
 * TLA+ bounded quantified expression. E.g. \E x \in S: ...; \A x \in S: ...
 */
case class BoundedQuantifiedExpression(quantifier: String, binders: List[QuantifierBound], body: Expression) extends Expression
/**
 * TLA+ quantified expression. E.g. \E x, y: ...; \A x, y : ...
 */
case class QuantifiedExpression(quantifier: String, binders: List[Identifier], body: Expression) extends Expression
/**
 * Conjunction lists. E.g. /\ x = 10
 *                         /\ y = 11
 */
case class ConjunctionList(listItems: List[Expression]) extends Expression
/**
 * Disjunction lists. E.g. \/ x = 10
 *                         \/ y = 11
 */
case class DisjunctionList(listItems: List[Expression]) extends Expression


/**
 * Set trait.
 */
sealed trait Set extends Expression
/**
 * Explicitly given set. E.g. { 1, 2, 3}.
 */
case class ExplicitSet(values: List[Expression]) extends Set
/**
 * Set, which is defined through a single quantifier bound. E.g. { x \in S : P(x) }.
 */
case class BoundedSetComprehension(variable: Identifier | IdentifierTuple, domain: Expression, predicate: Expression) extends Set
/**
 * General quantified set comprehension. E.g. { f(x, y) : x \in S, y \in T }.
 */
case class QuantifiedSetComprehension(mappingExpression: Expression, quantifierBounds: List[QuantifierBound]) extends Set

sealed trait Function extends Expression
case class ExplicitFunction(mappings: List[Mapping]) extends Function
case class BoundedMapping(quantifierBounds: List[QuantifierBound], expression: Expression) extends Function
case class SetOfAllFunctions(domain: Expression, codomain: Expression) extends Function
case class SetOfAllRecords(mappings: List[Mapping]) extends Function

// Except Expressions
sealed trait SelectorPart extends AST
case class DotSelector(name: Identifier) extends SelectorPart
case class IndexSelector(indices: List[Expression]) extends SelectorPart
case class SelectorPath(parts: List[SelectorPart]) extends AST
case class ExceptClause(selector: SelectorPath, value: Expression) extends AST
case class ExceptExpression(exceptTarget: Expression, clauses: List[ExceptClause]) extends Function

/**
 * Weak stuttering expression. E.g. [A]_e
 */
case class WeakStuttering(action: Expression, state: Expression) extends Expression
/**
 * Strong stuttering expression. E.g. <<A>>_e
 */
case class StrongStuttering(action: Expression, state: Expression) extends Expression
/**
 * Weak fairness expression. E.g. WF_v(A)
 */
case class WeakFairness(state: Expression, action: Expression) extends Expression
/**
 * Strong stuttering node. E.g. SF_v(A)
 */
case class StrongFairness(state: Expression, action: Expression) extends Expression
/**
 * At sign (@).
 */
case class AtSign() extends Expression

/**
 * EPC Specifications.
 *
 * EPCSpec := '--EPC' [ '(' CommaList(Identifier) ')' ]? Identifier EPCSpecUnit*
 *
 */
case class EPCSpec(identifier: Identifier, modifiers: List[Identifier], specUnits: List[EPCSpecUnit]) extends ModuleUnit
/**
 * EPCSpecUnit := ChannelDefinition | ThreadDefinition | Network
 */
sealed trait EPCSpecUnit extends AST
/**
 * Enumerator := BoundedEnumerator | BoundedEnumeratorWithFilter
 */
sealed trait Enumerator
/**
 * BoundedEnumerator := '(' Identifier ['=' | '\in'] Expression ')'
 *
 * E.g. (A = 1), (Q = "a"), (p \in 1..10), (q \in {"a", "b"}), ...
 */
case class BoundedEnumerator(name: Identifier, equalOrIn: String, domain: Expression) extends Enumerator
/**
 * BoundedEnumeratorWithFilter := '(' Identifier ['=' | '\in'] Expression ':' Expression ')'
 *
 * E.g. (j \in 1..10 : j # self)
 */
case class BoundedEnumeratorWithFilter(name: Identifier, equalOrIn: String, domain: Expression, filter: Expression) extends Enumerator
/**
 * ChannelDefinition := 'new' Identifier
 *
 * E.g. new c
 */
case class ChannelDefinition(identifier: Identifier) extends EPCSpecUnit
/**
 * TravellingMessage := Identifier '_' '{' Literal '->' Literal '}' '<<' Expression '>>'
 *
 * E.g. c_{1 -> 2}<< "Hello" >>, c_{"a" -> "b"}<< "Hello again" >>, ...
 */
case class TravellingMessage(channel: Identifier, source: Expression, target: Expression, value: Expression) extends AST
/**
 * An outgoing message. These messages are part of send actions.
 *
 * OutgoingMessage := Expression '@' Expression '<<' Expression '>>'
 *
 * E.g. c@2<<"Hello">>, c@"a"<< <<1,2,3>> >>
 */
case class OutgoingMessage(channel: Expression, target: Expression, value: Expression) extends AST
/**
 * MultiSet := ExplicitMultiSet | QuantifiedMultiSet
 */
sealed trait MultiSet[T] extends AST
/**
 * ExplicitMultiSet := '{{' CommaList(Expression) '}}'
 */
case class ExplicitMultiSet[T](values: List[T]) extends MultiSet[T]
/**
 * QuantifiedMultiSet := '{{' Enumerator Expression '}}'
 */
case class QuantifiedMultiSet[T](enumerator: Enumerator, value: T) extends MultiSet[T]
/**
 * Action := Declaration | Assignment | Receive | Send
 */
sealed trait Action extends AST
/**
 * Declaration := 'var' Identifier
 *
 * E.g. var x, var y, ...
 */
case class Declaration(identifier: Identifier) extends Action
/**
 * Assignment := '<<' Identifier ':=' Expression '>>'
 *
 * E.g. << x := 10 >>
 */
case class Assignment(identifier: Identifier, expression: Expression) extends Action
/**
 * Receive := Expression '@' Expression '?' '(' Identifier ')'
 *
 * E.g. c@2?(x)
 */
case class Receive(channel: Expression, location: Expression, variable: Identifier) extends Action
/**
 * Send := MultiSet(OutgoingMessage)
 *
 * E.g. {{ c@1<< ... >>,  c@2<< ... >> }}, {{ (i \in 1..2 : i # self) c@i<< ... >> }}
 */
case class Send(messages: MultiSet[OutgoingMessage]) extends Action
/**
 * ConditionalThread := 'if' Expression 'then' Thread 'else' Thread 'fi'
 *
 * E.g. if ... then ... else ... fi
 */
case class ConditionalThread(cond: Expression, thenThread: Thread, elseThread: Thread) extends AST
/**
 * ThreadInstantiation := Identifier'()'
 *
 * E.g. foo(), bar()
 */
case class ThreadInstantiation(thread: Identifier) extends AST
/**
 * Command := Action | ConditionalThread | ThreadInstantiation
 */
type Command = Action | ConditionalThread | ThreadInstantiation
/**
 * ThreadUnit := [Identifier ':']? Command
 *
 * E.g. (declareX: var x) or (assignX: << x := 10 >>)
 */
case class ThreadUnit(label: Option[Identifier], command: Command)
/**
 * Thread := Identifier ':' Command | Thread ';' Thread
 *
 * E.g. ( var x; << x := 10 >> ) or ( declareX: var x; assign10: << x := 10 >> )
 */
type Thread = List[ThreadUnit]
/**
 * OuterThread := ThreadComprehension | ThreadComposition
 */
sealed trait OuterThread extends AST
/**
 * ThreadComprehension := Enumerator List(Command)
 *
 * E.g. (j = 1) (var x; << x := 10 >>)
 *      (j \in 1..10) (<< x := j >>)
 */
case class ThreadComprehension(enumerator: Enumerator, body: Thread) extends OuterThread
/**
 * ThreadComposition := ThreadComprehension '|' ThreadComprehension
 *
 * E.g. (j = 1) (<< x := j >>) | (k = 2) (<< x := k >>)
 */
case class ThreadComposition(lhs: OuterThread, rhs: OuterThread) extends OuterThread
/**
 * ThreadDefinition := 'def' Identifier [ '(' CommaList(Identifier) ')' ] ? '=' Thread
 *
 * E.g. def declareXAndStop = var x. 0
 *      def assignXAndStop(x) = << x := 10 >> . 0
 */
case class ThreadDefinition(identifier: Identifier, freeVariables: List[Identifier], body: Thread) extends EPCSpecUnit
/**
 * Network := Location | MessageAether | NetworkComposition
 */
sealed trait Network extends EPCSpecUnit
/**
 * Process := '[[' ExplicitFunction '<|' Thread ']]'
 *
 * E.g. [[ [x |-> Undefined] <| var x. << x := 10 >> . 0 ]]
 */
case class Process(memory: ExplicitFunction, body: OuterThread) extends AST
/**
 * LocationComprehension := Enumerator Process
 *
 * E.g. (p = 1) [[ [x |-> Undefined] |> var x. << x := 10 >>. 0 ]]
 *      (p \in 1..10) [[ [x |-> Undefined] |> var x. << x := 10 >>. 0 ]]
 */
case class Location(enumerator: Enumerator, body: Process) extends Network
/**
 * MessageAether := ExplicitMultiset(TravellingMessage) | QuantifiedMultiSet(TravellingMessage)
 *
 * E.g. {{ c_{1 -> 2}<< ... >>, c_{"a" -> "b"}<< ... >> }} or {{ (i \in 1..2) c@i<< ... >> }}
 */
case class MessageAether(messages: MultiSet[TravellingMessage]) extends Network
/**
 * NetworkComposition := Network '||' Network
 *
 * E.g. (p = 1) [[ [x |-> Initialized] <| c@2(x) . 0 ]] || {{ c_{2 -> 1}<< "Hello from 2" >> }}
 */
case class NetworkComposition(lhs: Network, rhs: Network) extends Network

class Parser(lexer: Tokenizer) {

  /**
   * Check that the next token is of a given kind. If a list of n TokenKinds is given,
   * check that the next n TokenKinds match the next expected ones.
   *
   * @param expected The kind of the token we expect or a list of expected
   *                 token kinds if we look ahead more than one token at
   *                 a time.
   * @return True if the next token has the expected token kind,
   *         false otherwise.
   */
  private def check(expected: TokenKind | List[TokenKind]): Boolean = {
    val tokenKinds: List[TokenKind] = expected match {
      case tk: TokenKind => List(tk)
      case tks: List[TokenKind] => tks
    }
    val tokens = lexer.peek(tokenKinds.length) match {
      case t: Token => List(t)
      case ts: List[Token] => ts
    }
    tokenKinds.zip(tokens).forall(_ == _.kind)
  }

  /**
   * Consumes a token by first checking the token kind.
   * In case the token is of the expected kind, we consume the token.
   * If a token with an unexpected token kind is encountered,
   * an error is reported by returning a string.
   *
   * @param expected The kind of the token we expect.
   * @return The consumed token if the next token has the expected token kind,
   *         otherwise a parsing error is reported.
   */
  def consume(expected: TokenKind) : Either[String, Token] =
    if check(expected)
      then Right(lexer.consume())
      else Left(error(s"token of kind $expected not found"))

  private def error(token: Token, message: String): String = {
    val lineContents = lexer.scanner.getLine(token.line)
    s"""SyntaxError (line ${token.line}, column ${token.column}): ${message}.
       |>>> ${lineContents}
       |>>> ${"-".repeat(token.column - 1)}^
       |""".stripMargin
  }

  private def error(message: String): String = {
    val token = lexer.peek().asInstanceOf[Token]
    error(token, message)
  }

  /**
   * Parses a comma-separated list of objects (identifiers, expressions, ...).
   *
   * @param consumer Consumer function for the separate objects.
   *                 Contains the logic for the parsing of a single object.
   *                 (E.g. () => consume(TokenKind.IDENTIFIER))
   * @tparam T Type argument for the type of the consumed object. (E.g. Token)
   * @return A list with consumed objects.
   */
  private def parseCommaList[T](consumer: () => Either[String, T]): Either[String, List[T]] =
    for {
      // Consume the object
      firstObject <- consumer()
      // To get the whole result, we first declare a recursive procedure
      // which will check on each call if there is a comma
      // if yes, it will consume the comma and the object; and then continue recursively
      // if no, it will return the accumulated result
      result <- {
        // Initial accumulator contains the first object
        val initialAcc = ListBuffer(firstObject)

        // Recursive procedure for the next objects
        def loop(accumulator: ListBuffer[T]): Either[String, List[T]] = {
          // Here we check if there is a comma. If yes, we consume it
          // and call the consumer function to consume the next object,
          // which we append to the accumulator list.
          // After that, we return the result of the recursive call.
          if check(TokenKind.COMMA) then for {
            _ <- consume(TokenKind.COMMA)
            nextObject <- consumer()
            intermediateResult <- {
              accumulator += nextObject; loop(accumulator)
            }
          } yield intermediateResult
          // If we cannot find any further commas, we return the accumulator as an immutable list.
          else Right(accumulator.toList)
        }
        // Start of the recursive procedure.
        loop(initialAcc)
      }
    } yield result

  /**
   * Parse a TLA+ module.
   *
   * Module := MinusSeparator 'MODULE' Identifier MinusSeparator
   *           ['EXTENDS' CommaList(Identifier)]?
   *           ModuleUnit*
   *           EqualSeparator
   * @return
   */
  def parseModule(): Either[String, Module] = {
    for {
      _ <- consume(TokenKind.MINUS_SEPARATOR)
      _ <- consume(TokenKind.MODULE)
      moduleName <- parseIdentifier()
      _ <- consume(TokenKind.MINUS_SEPARATOR)
      extendsStmt <- parseExtendsStatement()
      moduleUnits <- parseModuleUnits()
      _ <- consume(TokenKind.EQUAL_SEPARATOR)
    } yield Module(moduleName, extendsStmt, moduleUnits)
  }

  /**
   * Parse an EXTENDS statement.
   *
   * ExtendsStmt := ['EXTENDS' CommaList(Identifier) ]?
   *
   */
  def parseExtendsStatement(): Either[String, ExtendsStatement] =
    if check(TokenKind.EXTENDS) then for {
        _ <- consume(TokenKind.EXTENDS)
        extendedModules <- parseCommaListOfIdentifiers()
      } yield ExtendsStatement(extendedModules)
    else Right(ExtendsStatement(Nil))

  /**
   * Set with the possible first tokens of a module unit.
   */
  private val moduleUnitFirstSet = Set(
    TokenKind.VARIABLE,
    TokenKind.VARIABLES,
    TokenKind.CONSTANT,
    TokenKind.CONSTANTS,
    TokenKind.LOCAL,
    TokenKind.IDENTIFIER,
    TokenKind.INSTANCE,
    TokenKind.ASSUME,
    TokenKind.THEOREM,
    TokenKind.MINUS_SEPARATOR,
    TokenKind.EPC_CONTEXT_OPEN)

  /**
   * Check if the next token is one of the possible first tokens of a module unit.
   */
  private def checkModuleUnitFirstSet(): Boolean =
    lexer.peek() match {
      case Token(kind, value, _, _) if moduleUnitFirstSet contains kind => true
      case _ => false
    }

  /**
   * Parse a single module unit.
   *
   * Unit := VariableDeclaration
   *        | ConstantDeclaration
   *        | ['LOCAL']? OperatorDefinition
   *        | ['LOCAL']? FunctionDefinition
   *        | ['LOCAL']? Instance
   *        | ['LOCAL']? ModuleDefinition
   *        | Assumption
   *        | Theorem
   *        | Module
   *        | MinusSeparator ( = AtLeast4("-"))
   *        | EPCSpec
   */
  def parseModuleUnit(): Either[String, ModuleUnit] =
    lexer.peek() match {
      case Token(TokenKind.VARIABLE, _, _, _) => parseVariableDeclaration()
      case Token(TokenKind.VARIABLES, _, _, _) => parseVariableDeclaration()
      case Token(TokenKind.CONSTANT, _, _, _) => parseConstantDeclaration()
      case Token(TokenKind.CONSTANTS, _, _, _) => parseConstantDeclaration()
      case Token(TokenKind.LOCAL, _, _, _) => parseLocalDefinition()
      case Token(TokenKind.IDENTIFIER, _, _, _) => parseDefinition()
      case Token(TokenKind.INSTANCE, _, _, _) => parseInstance()
      case Token(TokenKind.ASSUME, _, _, _) => parseAssumption()
      case Token(TokenKind.THEOREM, _, _, _) => parseTheorem()
      case Token(TokenKind.MINUS_SEPARATOR, _, _, _) => parseNestedModuleOrSeparator()
      // Here we also treat EPC specs as module units!!!
      case Token(TokenKind.EPC_CONTEXT_OPEN, _, _, _) => parseEPCSpec()
      case _ => Left(error("Module unit expected"))
    }

  /**
   * Parses a nested module or a separator.
   *
   * NestedModuleOrSeparator := Module | Separator
   *
   */
  private def parseNestedModuleOrSeparator(): Either[String, ModuleUnit] =
    if check(List(TokenKind.MINUS_SEPARATOR, TokenKind.MODULE))
      then parseModule()
      else parseSeparator()

  /**
   * Parse a sequence of module units.
   *
   * @return A list of consumed module units.
   */
  private def parseModuleUnits(): Either[String, List[ModuleUnit]] = {
    val units = new ListBuffer[ModuleUnit]
    while (checkModuleUnitFirstSet()) {
      (for moduleUnit <- parseModuleUnit() yield moduleUnit) match {
        case Right(value) => units addOne value
        case Left(value) => return Left(value)
      }
    }
    Right(units.toList)
  }

  /**
   * Parses a local definition.
   * @return
   */
  private def parseLocalDefinition(): Either[String, LocalDefinition] =
    for {
      _ <- consume(TokenKind.LOCAL)
      moduleUnit <- parseDefinition()
    } yield LocalDefinition(moduleUnit)

  /**
   * Parses an operator, function, instance of module definition.
   */
  private def parseDefinition(): Either[String, OperatorDefinition | FunctionDefinition | Instance | ModuleDefinition] =
    lexer.peek() match {
      case Token(TokenKind.IDENTIFIER, _, _, _) =>
        if check(List(TokenKind.IDENTIFIER, TokenKind.LEFT_SQUARE_BRACKET))
        then parseFunctionDefinition()
        else parseOperatorDefinitionOrModuleDefinition()
      case Token(TokenKind.INSTANCE, _, _, _) => parseInstance()
      case _ => Left(error("Definition expected"))
    }

  /**
   * Parse a variable declaration.
   *
   * [ ['VARIABLE' | 'VARIABLES'] CommaList(Identifier)
   *
   * @return
   */
  private def parseVariableDeclaration(): Either[String, VariableDeclaration] =
    for {
      token <- if check(TokenKind.VARIABLE) then consume(TokenKind.VARIABLE) else consume(TokenKind.VARIABLES)
      declaredVariables <- parseCommaListOfIdentifiers()
    } yield VariableDeclaration(token, declaredVariables)

  /**
   * Parse a constant declaration.
   *
   * [ ['CONSTANT' | 'CONSTANTS'] CommaList(OperatorDeclaration)
   *
   * @return
   */
  private def parseConstantDeclaration(): Either[String, ConstantDeclaration] =
    for {
      token <- if check(TokenKind.CONSTANT) then consume(TokenKind.CONSTANT) else consume(TokenKind.CONSTANTS)
      operatorDeclarations <- parseCommaListOfOperatorDeclarations()
    } yield ConstantDeclaration(token, operatorDeclarations)

  /**
   * Parse an operator declaration.
   *
   * OperatorDeclaration := Identifier
   *                      | Identifier '(' CommaList('_') ')'
   *                      | PrefixOp '_'
   *                      | '_' InfixOp '_'
   *                      | '_' PostfixOp
   *
   * Prefix, infix and postfix operators are currently not supported!
   */
  private def parseOperatorDeclaration(): Either[String, OperatorDeclaration] =
    for {
      identifier <- parseIdentifier()
      args <- if check(TokenKind.LEFT_ROUND_BRACKET) then for {
        _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
        args <- parseCommaListOfUnderscoreTokens()
        _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
      } yield args
      else Right(Nil)
    } yield OperatorDeclaration(identifier, args)

  /**
   * Parse a comma-separated list of underscore tokens.
   *
   * @return A list of consumed underscored tokens.
   */
  private def parseCommaListOfUnderscoreTokens(): Either[String, List[Token]] =
    parseCommaList[Token](() => for token <- consume(TokenKind.UNDERSCORE) yield token)

  /**
   * Parse a comma-separated list of operator declarations.
   *
   * @return A list of parsed operator declarations.
   */
  private def parseCommaListOfOperatorDeclarations(): Either[String, List[OperatorDeclaration]] =
    parseCommaList[OperatorDeclaration](() => parseOperatorDeclaration())

  /**
   * Parse an operator definition or module definition.
   * The reason for parsing them in the same method is
   * that they both can start with a NonFixLHS. In that
   * case we have to parse the '==' and look at the
   * following symbol: If it is 'INSTANCE', then we have
   * a module definition, otherwise an operator definition.
   *
   * OperatorDefinition :=  ( NonFixLHS
   *                        | PrefixOp Identifier
   *                        | Identifier InfixOp Identifier
   *                        | Identifier PostfixOp )
   *                        '=='
   *                        Expression
   *
   * Prefix, infix and postfix operator definitions are currently not supported!
   *
   * ModuleDefinition := NonFixLHS '==' Instance
   */
  private def parseOperatorDefinitionOrModuleDefinition(): Either[String, OperatorDefinition | ModuleDefinition] =
    for {
      nonFixLHS <- parseNonFixLHS()
      _ <- consume(TokenKind.DEFINE)
      result <- if check(TokenKind.INSTANCE)
        then for {
          subs <- parseInstance()
        } yield ModuleDefinition(nonFixLHS, subs)
        else for {
          body <- parseExpression()
        } yield OperatorDefinition(nonFixLHS, body)
    } yield result

  /**
   * Parses non-fix LHS.
   *
   * NonFixLHS := Identifier ['(' CommaList(Identifier | OperatorDeclaration) ')']?
   */
  private def parseNonFixLHS(): Either[String, NonFixLHS] = for {
    identifier <- parseIdentifier()
    parameters <- if check(TokenKind.LEFT_ROUND_BRACKET) then for {
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      parameters <- parseCommaListOfIdentifiersAndOperatorDeclarations()
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield parameters
    else Right(Nil)
  } yield NonFixLHS(identifier, parameters)

  /**
   * Parses an identifier or an operator declaration.
   */
  private def parseIdentifierOrOperatorDeclaration(): Either[String, Identifier | OperatorDeclaration] =
    for {
      result <- if check(List(TokenKind.IDENTIFIER, TokenKind.LEFT_ROUND_BRACKET))
        then parseOperatorDeclaration()
        else parseIdentifier()
    } yield result

  /**
   * Parse a comma-separated list of identifiers and operator declarations.
   *
   * @return A list of parsed identifiers and operator declarations.
   */
  private def parseCommaListOfIdentifiersAndOperatorDeclarations(): Either[String, List[Identifier | OperatorDeclaration]] =
    parseCommaList[Identifier | OperatorDeclaration](() => parseIdentifierOrOperatorDeclaration())

  /**
   * Parse a function definition.
   *
   * FunctionDefinition :=  Identifier '[' CommaList(QuantifierBound) ']' '==' Expression
   */
  private def parseFunctionDefinition(): Either[String, FunctionDefinition] = for {
    identifier <- parseIdentifier()
    _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
    arguments <- parseCommaListOfQuantifierBounds()
    _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
    _ <- consume(TokenKind.DEFINE)
    body <- parseExpression()
  } yield FunctionDefinition(identifier, arguments, body)

  /**
   * Parse an INSTANCE top-level statement.
   *
   * Instance := 'INSTANCE' Identifier ['WITH' CommaList(Substitution)]
   *
   */
  private def parseInstance(): Either[String, Instance] =
    for {
      _ <- consume(TokenKind.INSTANCE)
      identifier <- parseIdentifier()
      substitutions <- if check(TokenKind.WITH)
      then for {
          _ <- consume(TokenKind.WITH)
          subs <- parseCommaListOfSubstitutions()
        } yield subs
      else Right(Nil)
    } yield Instance(identifier, substitutions)

  /**
   * Parse a substitution of identifier with some argument.
   *
   * Substitution := (Identifier | PrefixOp | InfixOp | PostfixOp) '<-' Argument
   *
   * Currently, we only allow the substitution of identifiers!
   */
  private def parseSubstitution(): Either[String, Substitution] =
    for {
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.LEFT_ARROW)
      argument <- parseArgument()
    } yield Substitution(identifier, argument)

  /**
   * Parse a comma-separated list of substitutions.
   *
   * @return A list of parsed substitutions.
   */
  private def parseCommaListOfSubstitutions(): Either[String, List[Substitution]] =
    parseCommaList[Substitution](() => parseSubstitution())

  /**
   * Parse an argument.
   *
   * Argument := Expression
   *          | GeneralPrefixOp
   *          | GeneralInfixOp
   *          | GeneralPostfixOp
   *
   * Currently, we only allow expressions as arguments!
   */
  private def parseArgument(): Either[String, Expression] =
    parseExpression()

  /**
   * Parses an assumption.
   *
   * Assumption ::= ['ASSUME' | 'ASSUMPTION' | 'AXIOM'] Expression
   */
  def parseAssumption(): Either[String, Assumption] =
    for {
      token <- consume(TokenKind.ASSUME)
      expression <- parseExpression()
    } yield Assumption(token, expression)

  /**
   * Parses a theorem.
   *
   * Theorem ::= 'THEOREM' Expression
   */
  private def parseTheorem(): Either[String, Theorem] =
    for {
      token <- consume(TokenKind.THEOREM)
      expression <- parseExpression()
    } yield Theorem(expression)

  private def parseSeparator(): Either[String, Separator] =
    for {
      token <- if check(TokenKind.MINUS_SEPARATOR) then consume(TokenKind.MINUS_SEPARATOR) else consume(TokenKind.EQUAL_SEPARATOR)
    } yield Separator(token)

  /**
   * Parse an expression.
   * @return
   */
  def parseExpression(): Either[String, Expression] =
    parseImplication()

  /**
   * Parse a comma-separated list of expressions.
   *
   * @return A list of parsed expressions.
   */
  private def parseCommaListOfExpressions(): Either[String, List[Expression]] =
    parseCommaList[Expression](() => parseExpression())

  /**
   * Parse a binary implication.
   * Implications have precedence range 1-1.
   *
   * Implication := Expression '=>' Expression
   *
   * @return
   */
  private def parseImplication(): Either[String, Expression] = for {
    expression <- parseEquivalence()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] =
        lexer.peek() match {
          case Token(kind, value, _, _) if kind == TokenKind.IMPLICATION => for {
            _ <- consume(kind)
            rhs <- parseEquivalence()
            intermediateResult <- loop(BinaryExpression(value, lhs, rhs))
          } yield intermediateResult
          case _ => Right(lhs)
        }

      loop(expression)
    }
  } yield finalResult

  /**
   * Parse a binary equivalence.
   * Equivalences have precedence range 2-2.
   *
   * Equivalence := Expression '\equiv' Expression
   *
   * @return
   */
  private def parseEquivalence(): Either[String, Expression] = for {
    expression <- parseConjunctionOrDisjunction()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] =
        lexer.peek() match {
          case Token(kind, value, _, _) if kind == TokenKind.EQUIVALENCE => for {
            _ <- consume(kind)
            rhs <- parseConjunctionOrDisjunction()
            intermediateResult <- loop(BinaryExpression(value, lhs, rhs))
          } yield intermediateResult
          case _ => Right(lhs)
        }

      loop(expression)
    }
  } yield finalResult

  /**
   * A stack of AND/OR tokens that begin a conjunction/disjunction list.
   * We add these tokens to the stack, in order to know on which column the conjunction list starts.
   * This allows us to decide if an AND/OR token we encounter is a new conjunct of a conjunction list
   * or if it is part of the current expression.
   *
   * We push AND/OR tokens to the stack if they are the first symbol of a primary expression.
   * We pop the top token, when we finish parsing a conjunction or disjunction list.
   *
   * When we parse a normal conjunction or disjunction, we make sure that the current AND/OR token
   * is not the beginning of a conjunct in the conjunction list.
   *
   * E.g. /\ ... <-- this /\ will be pushed onto the stack
   *      /\ \/ ... <-- this \/ will be pushed onto the stack
   *         \/ ...
   *      /\ ...
   */
  private val conjunctionStack: mutable.Stack[Token] = mutable.Stack.empty[Token]

  /**
   * Parse a binary conjunction or disjunction.
   * Conjunctions and disjunctions have precedence range 3-3 and are left-associative.
   *
   * ConjunctionOrDisjunction := Expression ['/\' | '\/'] Expression
   *
   * @return
   */
  private def parseConjunctionOrDisjunction(): Either[String, Expression] = for {
    expression <- parsePrefixOperatorsWithPrecedenceRange4()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] =
        lexer.peek() match {
          case Token(kind, value, line, column)
            if (kind == TokenKind.AND || kind == TokenKind.OR) && conjunctionStack.forall(_.column != column) =>
            for {
              _ <- consume(kind)
              rhs <- parsePrefixOperatorsWithPrecedenceRange4()
              intermediateResult <- loop(BinaryExpression(value, lhs, rhs))
            } yield intermediateResult
          case _ => Right(lhs)
        }

      loop(expression)
    }
  } yield finalResult

  /**
   * Set with the token kinds '~', 'ENABLED', 'UNCHANGED', '[]' and '<>'.
   */
  private val prefixOperatorsWithPrecedenceRange4 = Set(
    TokenKind.NOT, TokenKind.ENABLED, TokenKind.UNCHANGED, TokenKind.SQUARE, TokenKind.DIAMOND)

  /**
   * Parse the ~, ENABLED, UNCHANGED, SQUARE, DIAMOND prefix operators.
   * The operator ~ has precedence range 4-4
   * These rest of the operators have precedence range 4-15.
   *
   * ['~' | 'ENABLED' | 'UNCHANGED' | '[]' | '<>'] Expression
   *
   * @return
   */
  private def parsePrefixOperatorsWithPrecedenceRange4(): Either[String, Expression] =
    lexer.peek() match {
      case Token(kind, value, _, _) if prefixOperatorsWithPrecedenceRange4 contains kind => for {
        _ <- consume(kind)
        innerExpression <- parsePrefixOperatorsWithPrecedenceRange4()
      } yield UnaryExpression(value, innerExpression)
      case _ => for expression <- parseRelationalOperators() yield expression
    }

  /**
   * Set with the token kinds '=', '/=', '<', '<=', '>', '>=', '\in', '\notin' and '\subset'.
   */
  private val relationalTokens = Set(TokenKind.EQ, TokenKind.NEQ, TokenKind.LT, TokenKind.LE, TokenKind.GT, TokenKind.GE,
    TokenKind.SET_IN, TokenKind.NOTIN, TokenKind.SUBSET)

  /**
   * Parse an equality / relational / membership operator.
   * These operators have precedence range 5-5 (and currently are implemented left-associatively).
   *
   * SetOperation := Expression [ '=' | '/='  | '<' | '<=' | '>' | '>=' | '\in' | '\notin' | '\subset' ] Expression
   *
   * @return
   */
  private def parseRelationalOperators(): Either[String, Expression] = for {
    expression <- parseSubsetOrUnionExpression()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] =
        lexer.peek() match {
          case Token(kind, value, _, _) if relationalTokens contains kind => for {
            _ <- consume(kind)
            rhs <- parseSubsetOrUnionExpression()
            intermediateResult <- loop(BinaryExpression(value, lhs, rhs))
          } yield intermediateResult
          case _ => Right(lhs)
        }

      loop(expression)
    }
  } yield finalResult

  /**
   * Parse the SUBSET and UNION prefix operators.
   * These operators have precedence range 8-8.
   *
   * ['SUBSET' | 'Union'] Expression
   *
   * @return
   */
  private def parseSubsetOrUnionExpression(): Either[String, Expression] =
    for expression <- if check(TokenKind.BIG_SUBSET) || check(TokenKind.BIG_UNION)
      then for {
        token <- if check(TokenKind.BIG_SUBSET) then consume(TokenKind.BIG_SUBSET) else consume(TokenKind.BIG_UNION)
        innerExpression <- parseSubsetOrUnionExpression()
      } yield UnaryExpression(token.value, innerExpression)
      else parseSetOperations()
    yield expression

  /**
   * Parse a set expression (set-difference, union, disjunction).
   * These operators have precedence range 8-8
   * and the union and intersection are left-associative.
   *
   * SetOperation := Expression [ '\' | '\union' | '\intersect'] Expression
   *
   * @return
   */
  private def parseSetOperations(): Either[String, Expression] = for {
    expression <- parseDomainExpression()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] = {
        if check(TokenKind.SET_DIFFERENCE) then for {
          token <- consume(TokenKind.SET_DIFFERENCE)
          rhs <- parseDomainExpression()
          intermediateResult <- loop(BinaryExpression(token.value, lhs, rhs))
        } yield intermediateResult
        else if check(TokenKind.UNION) then for {
          token <- consume(TokenKind.UNION)
          rhs <- parseDomainExpression()
          intermediateResult <- loop(BinaryExpression(token.value, lhs, rhs))
        } yield intermediateResult
        else if check(TokenKind.INTERSECT) then for {
          token <- consume(TokenKind.INTERSECT)
          rhs <- parseDomainExpression()
          intermediateResult <- loop(BinaryExpression(token.value, lhs, rhs))
        } yield intermediateResult
        else Right(lhs)
      }
      loop(expression)
    }
  } yield finalResult

  /**
   * Parse the DOMAIN prefix operator.
   * The operator DOMAIN has precedence range 9-9.
   *
   * 'DOMAIN' Expression
   *
   * @return
   */
  private def parseDomainExpression(): Either[String, Expression] =
    for expression <- if check(TokenKind.DOMAIN)
      then for {
          token <- consume(TokenKind.DOMAIN)
          innerExpression <- parseDomainExpression()
        } yield UnaryExpression(token.value, innerExpression)
      else parseRangeExpression()
    yield expression

  /**
   * Parse range expressions.
   * The range operator .. has precedence range 9-9.
   *
   * RangeExpression := Expression '..' Expression
   *
   * @return
   */
  private def parseRangeExpression(): Either[String, Expression] = {
    for {
      lhs <- parseAdditiveExpression()
      result <- if check(TokenKind.DOT_DOT) then for {
        _ <- consume(TokenKind.DOT_DOT)
        rhs <- parseAdditiveExpression()
      } yield BinaryExpression("..", lhs, rhs)
      else Right(lhs)
    } yield result
  }

  /**
   * Parse an additive expression (left-associatively).
   * The plus operator has precedence range 10-10.
   * The minus operator has precedence range 11-11.
   * However, here there are implemented in the same function
   * (which means that the bind equally strong).
   *
   * @return
   */
  private def parseAdditiveExpression(): Either[String, Expression] = for {
    expression <- parseMultiplicativeExpression()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] = {
        if check(TokenKind.PLUS) then for {
          _ <- consume(TokenKind.PLUS)
          rhs <- parseMultiplicativeExpression()
          intermediateResult <- loop(BinaryExpression("+", lhs, rhs))
        } yield intermediateResult
        else if check(TokenKind.MINUS) then for {
          _ <- consume(TokenKind.MINUS)
          rhs <- parseMultiplicativeExpression()
          intermediateResult <- loop(BinaryExpression("-", lhs, rhs))
        } yield intermediateResult
        else Right(lhs)
      }
      loop(expression)
    }
  } yield finalResult

  /**
   * Set with the token kinds '*', '/', '%', '\X', '\times'
   */
  private val multiplicativeTokens = Set(TokenKind.MUL, TokenKind.DIV, TokenKind.MOD, TokenKind.TIMES)

  /**
   * Parse a multiplication expression (left-associatively).
   * The *, /, %, \X operators have precedence range 13-13 and are left-associative.
   *
   * @return
   */
  private def parseMultiplicativeExpression(): Either[String, Expression] = for {
    expression <- parseUnaryMinusExpression()
    finalResult <- {
      def loop(lhs: Expression): Either[String, Expression] =
        lexer.peek() match {
          case Token(kind, value, _, _) if multiplicativeTokens contains kind => for {
            _ <- consume(kind)
            rhs <- parseUnaryMinusExpression()
            intermediateResult <- loop(BinaryExpression(value, lhs, rhs))
          } yield intermediateResult
          case _ => Right(lhs)
        }
      loop(expression)
    }
  } yield finalResult

  /**
   * Parse a unary minus expression.
   * The unary minus operator has precedence range 12-12.
   *
   * @return
   */
  private def parseUnaryMinusExpression(): Either[String, Expression] =
    for expression <- if check(TokenKind.MINUS)
    then for {
        _ <- consume(TokenKind.MINUS)
        innerExpression <- parseUnaryMinusExpression()
      } yield UnaryExpression("-", innerExpression)
    else parseExponentiation()
    yield expression

  /**
   * Parse an exponentiation operator (right-associatively).
   * The exponentiation has precedence range 14-14 (and is implemented right-associatively).
   *
   * @return
   */
  private def parseExponentiation(): Either[String, Expression] = for {
    lhs <- parsePostfix()
    result <- if check(TokenKind.EXP) then for {
        _ <- consume(TokenKind.EXP)
        rhs <- parseExponentiation() /* right-associative */
      } yield BinaryExpression("^", lhs, rhs)
      else Right(lhs)
  } yield result

  /**
   * Parse a postfix operation (such as the prime operator),
   * function application or record field access.
   *
   * According to the "Summary of TLA+" document:
   * - the prime operator has precedence range 15-15
   * - the record field access operator has precedence range 17-17
   * - function applications are not mentioned.
   *
   * However, for now they are all implemented with the same precedence.
   * This might be changed in the future!
   *
   * G.GeneralPostfixOp := InstancePrefix PostfixOp
   *
   * Postfix := Expression GeneralPostfixOp
   *
   * @return
   */
  private def parsePostfix(): Either[String, Expression] = {
    for {
      primaryExpr <- parsePrimaryExpression()
      result <- {
        /**
         * Recursive loop for the inspection if there are any postfix operators
         * behind an expression, which we have already parsed.
         *
         * @param primaryOrPostfixExpr A parsed expression.
         * @return The given parsed expression, or a postfix expression applied to it.
         */
        def loop(primaryOrPostfixExpr: Expression): Either[String, Expression] = {
          // If we encounter a left square bracket, we know that
          // the arguments of a function application should follow.
          if check(TokenKind.LEFT_SQUARE_BRACKET) then for {
            // Here we parse the arguments of the function. (The function itself is already parsed.)
            fnApplication <- parseFunctionApplicationArguments(primaryOrPostfixExpr)
            // Before returning the function application expression, we look for further postfix operators.
            // This is the recursive call. The result of the call might be the function application or another
            // postfix expression.
            finalResult <- loop(fnApplication)
          } yield finalResult
          // If we encounter the dot operator, we know that we are looking at a field access.
          else if check(TokenKind.DOT) then for {
            // Here we parse the rhs of the field access. (The lhs itself is already parsed.)
            fieldAccess <- parseFieldAccessRHS(primaryOrPostfixExpr)
            // Here we look for further postfix operators
            finalResult <- loop(fieldAccess)
          } yield finalResult
          // If we encounter the prime operator, we know that we are looking at a primed expression.
          else if check(TokenKind.PRIME) then for {
            // Here we parse the primed expression.
            primedExpr <- parsePrimedExpression(primaryOrPostfixExpr)
            // And look for further postfix operators.
            finalResult <- loop(primedExpr)
          } yield finalResult
          // If we did not encounter any postfix operators, we simply return the input expression.
          // This is the bottom of the recursion.
          else Right(primaryOrPostfixExpr)
        }

        // We start looping with our parsed primary expression.
        loop(primaryExpr)
      }
    } yield result
  }

  private def parsePrimedExpression(expression: Expression) : Either[String, Expression] =
    for _ <- consume(TokenKind.PRIME) yield PrimedExpression(expression)

  /**
   * Parse the right-hand side of a field access expression, expecting the left-hand side to be given as input.
   *
   * The field access is a level 17 infix operator, so it binds stronger than all other operators.
   * This is the reason for calling it directly before the parsing of function applications and primary expressions.
   *
   * FieldAccess := Expression `.` Expression
   *
   * @return The field access as an AST-Node.
   */
  private def parseFieldAccessRHS(maybeFieldAccessLHS: Expression): Either[String, Expression] = {
    if check(TokenKind.DOT) then for {
      _ <- consume(TokenKind.DOT)
      rhsExpression <- parseIdentifier()
      modifiedExpression <- parseFieldAccessRHS(BinaryExpression(".", maybeFieldAccessLHS, rhsExpression))
    } yield modifiedExpression
    else Right(maybeFieldAccessLHS)
  }

  /**
   * Parses the arguments of a function application. This function is a recursive function.
   * If the opening bracket is not found, the function simply returns the given function expression.
   * If the opening bracket is found, the function consumes it, consumes the comma separated list of expressions, and the closing bracket.
   * Before returning, it tries to parse further function applications to arguments. (E.g. as in f[0][0].)
   *
   * FunctionApplicationArguments := [ '[' CommaList(Expression) ']' ]*
   *
   * @param appliedFunction The function that is applied.
   * @return The function application or expression as an AST-Node.
   */
  private def parseFunctionApplicationArguments(appliedFunction: Expression): Either[String, Expression] = {
    if check(TokenKind.LEFT_SQUARE_BRACKET) then for {
      _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
      operands <- parseCommaListOfExpressions()
      _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
      functionApplication <- parseFunctionApplicationArguments(FunctionApplication(appliedFunction, operands))
    } yield functionApplication
    else Right(appliedFunction)
  }

  /**
   * Parses a primary expression.
   *
   * PrimaryExpr := GeneralIdentifier
   *              | GeneralIdentifierApplication
   *              | Literal
   *              | QuantifiedExpression
   *              | ChooseExpression
   *              | Set
   *              | Function
   *              | Tuple
   *              | ParenthesizedExpression
   *              | FairnessExpression
   *              | IfThenElseExpression
   *              | AtSign
   *
   * @return The primary expression as an AST-Node.
   */
  private def parsePrimaryExpression(): Either[String, Expression] =
    if check(TokenKind.IDENTIFIER)
      then for generalIdentifier <- parseAndSimplifyGeneralIdentifier() yield generalIdentifier
    else if checkLiteral()
      then for literal <- parseLiteral() yield literal
    else if check(TokenKind.EXISTS) || check(TokenKind.FORALL)
      then for quantifiedExpression <- parseQuantifiedExpression() yield quantifiedExpression
    else if check(TokenKind.CHOOSE)
      then for chooseExpression <- parseChooseExpression() yield chooseExpression
    else if check(TokenKind.LEFT_CURLY_BRACKET)
      then for set <- parseSet() yield set
    else if check(TokenKind.LEFT_SQUARE_BRACKET)
      then for function <- parseFunctionOrWeakStuttering() yield function
    else if check(TokenKind.LEFT_ANGLE_BRACKET)
      then for tuple <- parseTupleOrStrongStuttering() yield tuple
    else if check(TokenKind.LEFT_ROUND_BRACKET)
      then for parenExpression <- parseParenthesizedExpression() yield parenExpression
    else if check(List(TokenKind.WF, TokenKind.UNDERSCORE)) || check(List(TokenKind.SF, TokenKind.UNDERSCORE))
      then for fairnessExpression <- parseFairnessExpression() yield fairnessExpression
    else if check(TokenKind.IF)
      then for ifThenElse <- parseIfThenElse() yield ifThenElse
    else if check(TokenKind.AT)
      then for _ <- consume(TokenKind.AT) yield AtSign()
    else if check(TokenKind.CASE)
      then for caseExpr <- parseCaseExpression() yield caseExpr
    else if check(TokenKind.AND)
      then for conjunctionList <- parseConjunctionList() yield conjunctionList
    else if check(TokenKind.OR)
      then for disjunctionList <- parseDisjunctionList() yield disjunctionList
    else Left(error("TLA+ Expression expected"))

  /**
   * Parse a general identifier and then "simplify" it.
   *
   * The simplification is the following:
   * If the general identifier has an empty prefix, we return an Identifier.
   * If the general identifier application has an empty prefix, we return an IdentifierApplication.
   */
  private def parseAndSimplifyGeneralIdentifier(): Either[String, Expression] = {
    for generalIdentifier <- parseGeneralIdentifier(Nil) yield generalIdentifier match {
      case GeneralIdentifierApplication(GeneralIdentifier(Nil, identifier), args) => IdentifierApplication(identifier, args)
      case GeneralIdentifier(Nil,identifier) => identifier
      case _ => generalIdentifier
    }
  }

  /**
   * Parse a general identifier or a general identifier application.
   *
   * GeneralIdentifier := InstancePrefix Identifier
   *
   * GeneralIdentifierApplication := GeneralIdentifier '(' CommaList(Expression) ')'
   *
   */
  private def parseGeneralIdentifier(parsedPrefixParts: List[InstancePrefixPart]): Either[String, Expression] = {
    for {
      ast <- parseInstancePrefixPartOrIdentifierApplicationOrIdentifier()
      result <- ast match {
        // If we just parsed an instance prefix part, we try to parse the next syntactic object.
        // This is a recursive step.
        case part: InstancePrefixPart => parseGeneralIdentifier(parsedPrefixParts ++ List(part))
        // If we just parsed as identifier application, we need to transform it into a general identifier application.
        case IdentifierApplication(identifier, args) => Right(
          GeneralIdentifierApplication(
            GeneralIdentifier(
              parsedPrefixParts,
              identifier
            ),
            args)
          )
        // If we just parsed an identifier, we need to transform it into a general identifier.
        case identifier: Identifier => Right(
          GeneralIdentifier(parsedPrefixParts, identifier)
        )
      }
    } yield result
  }

  /**
   * Parse an instance prefix part, or an identifier application or an identifier.
   *
   * Examples for instance prefix parts: M! or M!(a, b)!
   * Examples for identifier applications: foo(bar)
   * Examples for identifiers: M, a, b, foo, bar
   *
   * InstancePrefixPart := Identifier [ '(' CommaList(Expression) ')' ]? '!'
   *
   * IdentifierApplication := Identifier '(' CommaList(Expression) ')'
   *
   */
  private def parseInstancePrefixPartOrIdentifierApplicationOrIdentifier(): Either[String, AST] = {
    for {
      identifier <- parseIdentifier()
      args <- if check(TokenKind.LEFT_ROUND_BRACKET) then for {
          _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
          args <- parseCommaListOfExpressions()
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield args
        else Right(Nil)
      result <- check(TokenKind.BANG) match {
        // If we see the ! symbol, we know that this is an InstancePrefixPart.
        case true => for _ <- consume(TokenKind.BANG) yield InstancePrefixPart(identifier, args)
        // If we don't see the ! symbol and the arguments list is not empty, we know that this is an IdentifierApplication.
        case false if args != Nil => Right(IdentifierApplication(identifier, args))
        // If we don't see the ! symbol and the arguments list is empty, we know that his is an Identifier.
        case false => Right(identifier)
      }
    } yield result
  }

  /**
   * Parse an identifier.
   *
   * @return Identifier as AST-Node.
   */
  private def parseIdentifier(): Either[String, Identifier] = {
    for token <- consume(TokenKind.IDENTIFIER) yield Identifier(token.value)
  }

  /**
   * Parses a comma-separated list of identifiers.
   *
   * @return A list of parsed identifier tokens.
   */
  private def parseCommaListOfIdentifiers(): Either[String, List[Identifier]] =
    parseCommaList[Identifier](() => parseIdentifier())

  /**
   * Check if the next token is a literal.
   *
   * @return True if the next token is a literal, else false.
   */
  private def checkLiteral(): Boolean = {
    check(TokenKind.TRUE)
      || check(TokenKind.FALSE)
      || check(TokenKind.BOOLEAN)
      || check(TokenKind.INTEGER)
      || check(TokenKind.STRING)
  }

  /**
   * Parse a literal.
   *
   * literal := 'TRUE', 'FALSE', 'BOOLEAN', Integer, String
   *
   * @return Literal as AST-Node.
   */
  private def parseLiteral(): Either[String, Literal] = {
    if check(TokenKind.TRUE)
    then for token <- consume(TokenKind.TRUE) yield Literal(token.value == "TRUE")
    else if check(TokenKind.FALSE)
    then for token <- consume(TokenKind.FALSE) yield Literal(token.value != "FALSE")
    else if check(TokenKind.BOOLEAN)
    then for token <- consume(TokenKind.BOOLEAN) yield Literal(token.value)
    else if check(TokenKind.INTEGER)
    then for token <- consume(TokenKind.INTEGER) yield Literal(Integer.valueOf(token.value))
    else if check(TokenKind.STRING)
    then for token <- consume(TokenKind.STRING) yield Literal(token.value)
    else Left(error("Literal not recognized"))
  }

  /**
   * Parse a (bounded) quantified expression.
   *
   * BoundedQuantifiedExpression := ['\E' | '\A'] CommaList(QuantifierBound) ':' Expression
   *
   * QuantifiedExpression := ['\E' | '\A'] CommaList(Identifier) ':' Expression
   *
   * @return The filtered set an AST-Node.
   */
  private def parseQuantifiedExpression(): Either[String, BoundedQuantifiedExpression | QuantifiedExpression] =
    for {
      token <- if check(TokenKind.EXISTS) then consume(TokenKind.EXISTS) else consume(TokenKind.FORALL)
      expr <- if checkCommaListOfQuantifierBounds()
        then for {
          binders <- parseCommaListOfQuantifierBounds()
          _ <- consume(TokenKind.COLON)
          body <- parseExpression()
        } yield BoundedQuantifiedExpression(token.value, binders, body)
        else for {
          binders <- parseCommaListOfIdentifiers()
          _ <- consume(TokenKind.COLON)
          body <- parseExpression()
        } yield QuantifiedExpression(token.value, binders, body)
    } yield expr

  /**
   * Parse a CHOOSE expression.
   *
   * ChooseExpression := 'CHOOSE' Identifier ['\in' Expression]? ':' Expression
   *
   * @return The filtered set an AST-Node.
   */
  private def parseChooseExpression(): Either[String, ChooseExpression] =
    for {
      _ <- consume(TokenKind.CHOOSE)
      variable <- parseIdentifierOrTuple()
      domain <- if check(TokenKind.SET_IN)
      then for {
          _ <- consume(TokenKind.SET_IN)
          domainExpression <- parseExpression()
        } yield domainExpression
      else Right(null)
      _ <- consume(TokenKind.COLON)
      predicate <- parseExpression()
    } yield ChooseExpression(variable, domain, predicate)

  /**
   * Parses a set.
   *
   * Set := ExplicitSet | BoundedSetComprehension | QuantifiedSetComprehension
   *
   * @return The set as an AST-Node.
   */
  private def parseSet(): Either[String, Set] =
    if check(List(TokenKind.LEFT_CURLY_BRACKET, TokenKind.IDENTIFIER, TokenKind.SET_IN))
      then parseBoundedSetComprehension()
    else parseExplicitSetOrQuantifiedSetComprehension()

  /**
   * Parses a filtered set constructor.
   *
   * BoundedSetComprehension := '{' Identifier '\in' Expression ':' Expression '}'
   *
   * @return The filtered set an AST-Node.
   */
  private def parseBoundedSetComprehension(): Either[String, BoundedSetComprehension] =
    for {
      _ <- consume(TokenKind.LEFT_CURLY_BRACKET)
      variable <- parseIdentifierOrTuple()
      _ <- consume(TokenKind.SET_IN)
      domain <- parseExpression()
      _ <- consume(TokenKind.COLON)
      mappingExpression <- parseExpression()
      _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
    } yield BoundedSetComprehension(variable, domain, mappingExpression)

  /**
   * Parse an explicitly given set or a quantified comprehension.
   *
   * ExplicitSet := '{' CommaList(Expression)? '}'
   *
   * QuantifiedSetComprehension := '{' Expression ':' Identifier '\in' Expression '}'
   *
   * @return An explicitly-given set or a quantified set comprehension as an AST-Node.
   */
  private def parseExplicitSetOrQuantifiedSetComprehension(): Either[String, Set] =
    for {
      _ <- consume(TokenKind.LEFT_CURLY_BRACKET)
      set <- {
        // If we directly see the closing bracket,
        // we know that this is an empty set, so we return it
        if check(TokenKind.RIGHT_CURLY_BRACKET) then {
          for {
            _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
          } yield ExplicitSet(Nil)
        }
        // Otherwise, we expect an expression, but we don't know
        // if this expression is part of an explicitly given set
        // or is an expression, which indicates how to construct
        // the elements of a set based on another set:
        // (e.g. { 1+1, 2+2, 3+3 } )
        // (e.g. the expression x*x in { x+x | x \in Nat })
        else {
          for {
            // For this, we first parse the first expression
            firstExpr <- parseExpression()
            explicitSetOrQuantifiedComprehension <- {
              // And we check if after the expression we have a colon
              // If this is the case, we know that we have a quantified set comprehension.
              if check(TokenKind.COLON) then for {
                // Here we parse the rest as if we are parsing a quantified set comprehension.
                _ <- consume(TokenKind.COLON)
                quantifierBounds <- parseCommaListOfQuantifierBounds()
                _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
              } yield QuantifiedSetComprehension(firstExpr, quantifierBounds)
              // If we find a comma instead, we know that we have
              // an explicitly given set of expressions, and we can
              // simply parse them
              else if check(TokenKind.COMMA) then for {
                _ <- consume(TokenKind.COMMA)
                followingExpressions <- parseCommaListOfExpressions()
                _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
              } yield ExplicitSet(List(firstExpr) ++ followingExpressions)
              // If we were not able to find a colon or a comma,
              // we simply try to look for the closing bracket.
              // (This is the case when we have an explicitly given set
              // with only one element,.)
              else for {
                _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
              } yield ExplicitSet(List(firstExpr))
            }
          } yield explicitSetOrQuantifiedComprehension
        }
      }
    } yield set

  /**
   * Parse an identifier tuple.
   *
   * IdentifierTuple := '<<' CommaList(Identifier) '>>'
   *
   * @return
   */
  private def parseIdentifierTuple(): Either[String, IdentifierTuple] =
    for {
      _ <- consume(TokenKind.LEFT_ANGLE_BRACKET)
      identifiers <- parseCommaListOfIdentifiers()
      _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
    } yield IdentifierTuple(identifiers)

  /**
   * Parse an identifier or a tuple.
   *
   * IdentifierOrTuple := Identifier | IdentifierTuple
   *
   * @return
   */
  private def parseIdentifierOrTuple(): Either[String, Identifier | IdentifierTuple] =
    if check(TokenKind.IDENTIFIER)
      then parseIdentifier()
    else if check(TokenKind.LEFT_ANGLE_BRACKET)
      then parseIdentifierTuple()
    else Left(error("Identifier or tuple not recognized"))

  /**
   * Check if the next tokens are part of a quantifier bound.
   * Here check for at most 3 identifiers!
   *
   * Examples for quantifier bounds:
   * - n \in Nat
   * - n, m \in Nat
   * - n \in Nat, m \in Nat
   * - << n, m >> \in Nat \times Nat
   *
   * @return True if the next tokens are part of a quantifier bound.
   */
  private def checkCommaListOfQuantifierBounds(): Boolean = {
    if check(TokenKind.IDENTIFIER)
    then check(List(TokenKind.IDENTIFIER, TokenKind.SET_IN))
      || check(List(TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.SET_IN))
      || check(List(TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.SET_IN))
    else if check(TokenKind.LEFT_ANGLE_BRACKET)
    then check(List(TokenKind.LEFT_ANGLE_BRACKET, TokenKind.IDENTIFIER, TokenKind.RIGHT_ANGLE_BRACKET, TokenKind.SET_IN))
      || check(List(TokenKind.LEFT_ANGLE_BRACKET, TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.RIGHT_ANGLE_BRACKET, TokenKind.SET_IN))
      || check(List(TokenKind.LEFT_ANGLE_BRACKET, TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.COMMA, TokenKind.IDENTIFIER, TokenKind.RIGHT_ANGLE_BRACKET, TokenKind.SET_IN))
    else false
  }

  /**
   * Parse a quantifier bound.
   *
   * QuantifierBound := [Identifier | CommaList(Identifier) | IdentifierTuple] '\in' Expression
   *
   * @return
   */
  private def parseQuantifierBound(): Either[String, QuantifierBound] =
    for {
      identifierOrTuple <- if check(List(TokenKind.IDENTIFIER, TokenKind.COMMA))
        then parseCommaListOfIdentifiers()
        else parseIdentifierOrTuple()
      _ <- consume(TokenKind.SET_IN)
      expression <- parseExpression()
    } yield QuantifierBound(identifierOrTuple, expression)

  private def parseCommaListOfQuantifierBounds(): Either[String, List[QuantifierBound]] =
    parseCommaList[QuantifierBound](() => parseQuantifierBound())

  /**
   * Parse an identifier to expression mapping.
   *
   * Mapping := Identifier '|->' Expression
   *
   * @return
   */
  private def parseMapping(): Either[String, Mapping] =
    for {
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.MAPS_TO)
      expression <- parseExpression()
    } yield Mapping(identifier, expression)

  private def parseCommaListOfMappings(): Either[String, List[Mapping]] =
    parseCommaList[Mapping](() => parseMapping())

  /**
   * Parse a record key to domain mapping.
   *
   * RecordKeyToDomainMapping := Identifier ':' Expression
   *
   * @return
   */
  private def parseRecordKeyToDomainMapping(): Either[String, Mapping] =
    for {
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.COLON)
      expression <- parseExpression()
    } yield Mapping(identifier, expression)

  private def parseCommaListOfRecordKeyToDomainMappings(): Either[String, List[Mapping]] =
    parseCommaList[Mapping](() => parseRecordKeyToDomainMapping())

  /**
   * Parse a function.
   *
   * Function :=  ExplicitFunction | BoundedMapping
   *
   * @return The function as an AST-Node.
   */
  private def parseFunctionOrWeakStuttering(): Either[String, Expression] =
    for {
      _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
      functionOrSetOfFunctions <- {
        // First, we check if there is an explicit mapping of elements
        // from a domain to an expression (e.g. [n \in Nat |-> n^2] ).
        if checkCommaListOfQuantifierBounds()
        then for {
          quantifierBounds <- parseCommaListOfQuantifierBounds()
          _ <- consume(TokenKind.MAPS_TO)
          expression <- parseExpression()
          _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
        } yield BoundedMapping(quantifierBounds, expression)
        else for {
          // Otherwise, we need to parse the first expression and
          // inspect it (and the following tokens) in order to know
          // how to proceed with the parsing. This is because we need
          // to create different AST nodes, depending on whether
          // we see the `->`, `EXCEPT`, `|->` or `:` symbol after
          // the first expression.
          //
          // Additionally, we parse here weak stuttering nodes, such as [A]_e.
          firstExpression <- parseExpression()
          result <- {
            // If we encounter `->`, we know that this is the set of
            // all functions (as in A -> B). We can simply parse it.
            if check(TokenKind.RIGHT_ARROW) then for {
              _ <- consume(TokenKind.RIGHT_ARROW)
              secondExpression <- parseExpression()
              _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
            } yield SetOfAllFunctions(firstExpression, secondExpression)
            // If we encounter `EXCEPT`, we also know that a function
            // update must follow.
            else if check(TokenKind.EXCEPT) then for {
              exceptExpression <- parseExceptExpression(firstExpression)
            } yield exceptExpression
            else if check(List(TokenKind.RIGHT_SQUARE_BRACKET, TokenKind.UNDERSCORE)) then for {
              _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
              _ <- consume(TokenKind.UNDERSCORE)
              stateExpression <- parsePrimaryExpression()
            } yield WeakStuttering(firstExpression, stateExpression)
            // Here we have 2 more cases:
            // 1) The set of all records with specific keys and their domains
            //    (e.g. [foo: {"FOO"}, bar: {"BAR"}] )
            // 2) explicit mapping of identifiers to expressions
            //    (e.g. [foo |-> "FOO", bar |-> "BAR"] )
            else firstExpression match {
              // Here we check for the set of all records with some specific
              // keys and the corresponding key domains.
              // E.g. [foo: {"FOO"}, bar: {"BAR"}].
              case identifier: Identifier if check(TokenKind.COLON) =>
                for {
                  _ <- consume(TokenKind.COLON)
                  secondExpression <- parseExpression()
                  // Up to here, we have parse the first key to key domain mapping
                  // Next, we check if there is a comma, and if yes we parse the rest.
                  tailMappings <- if check(TokenKind.COMMA)
                    then for {
                      _ <- consume(TokenKind.COMMA)
                      tailRecordKeyToKeyDomainMappings <- parseCommaListOfRecordKeyToDomainMappings()
                    } yield tailRecordKeyToKeyDomainMappings
                    else Right(Nil)
                  _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
                // Here we return the final AST-Node.
                } yield SetOfAllRecords(List(Mapping(identifier, secondExpression)) ++ tailMappings)
              // Next, we check for the set of explicit identifier to expression mappings.
              // E.g. [foo |-> "FOO", bar |-> "BAR"].
              case identifier: Identifier if check(TokenKind.MAPS_TO) =>
                for {
                  - <- consume(TokenKind.MAPS_TO)
                  secondExpression <- parseExpression()
                  // Up to here, we have parse the first key to expression mapping.
                  // Next, we check if there is a comma, and if yes we parse the rest.
                  tailMappings <- if check(TokenKind.COMMA)
                    then for {
                      _ <- consume(TokenKind.COMMA)
                      mappings <- parseCommaListOfMappings()
                    } yield mappings
                    else Right(Nil)
                  _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
                // Here we return the final AST-Node.
                } yield ExplicitFunction(List(Mapping(identifier, secondExpression)) ++ tailMappings)
              // If we reach this point, we can not recognize the function,
              // so we throw an error.
              case default => Left(error("Function not recognized"))
            }
          }
        } yield result
      }
    } yield functionOrSetOfFunctions

  /**
   * Parse an EXCEPT Expression.
   *
   * ExceptExpression := '[' Expression 'EXCEPT' CommaList(ExceptClause) ']'
   *
   * @param exceptTarget The except target (which is an expression and is already parsed).
   * @return ExceptExpression as an AST node.
   */
  private def parseExceptExpression(exceptTarget: Expression): Either[String, ExceptExpression] =
    for {
      _ <- consume(TokenKind.EXCEPT)
      exceptClauses <- parseCommaListOfExceptClauses()
      _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
    } yield ExceptExpression(exceptTarget, exceptClauses)

  private def parseCommaListOfExceptClauses(): Either[String, List[ExceptClause]] =
    parseCommaList[ExceptClause](() => parseExceptClause())

  /**
   * Parse an EXCEPT clause within an EXCEPT expression.
   *
   * ExceptClause := '!' SelectorPath '=' Expression
   *
   * @return The except clause as an AST node, with its selector path and value expression.
   */
  private def parseExceptClause(): Either[String, ExceptClause] =
    for {
      _ <- consume(TokenKind.BANG)
      selector <- parseSelectorPath()
      _ <- consume(TokenKind.EQ)
      value <- parseExpression()
    } yield ExceptClause(selector, value)

  /**
   * Parse a selector path within an EXCEPT clause.
   *
   * SelectorPath := [DotSelector | IndexSelector]*
   *
   * @return The selector path as an AST node.
   */
  private def parseSelectorPath(): Either[String, SelectorPath] = {
    val selectorParts = new ListBuffer[SelectorPart]
    while (check(TokenKind.DOT) || check(TokenKind.LEFT_SQUARE_BRACKET)) {
      if check(TokenKind.DOT)
        then parseDotSelector() match {
          case Right(value) => selectorParts.addOne(value)
          case Left(value) => return Left(value)
        }
        else parseIndexSelector() match {
          case Right(value) => selectorParts.addOne(value)
          case Left(value) => return Left(value)
        }
    }
    Right(SelectorPath(selectorParts.toList))
  }

  /**
   * Parse a dot selector within an EXCEPT clause.
   *
   * DotSelector := '.' Identifier
   *
   * @return The dot selector as an AST node with its identifier.
   */
  private def parseDotSelector(): Either[String, DotSelector] =
    for {
      _ <- consume(TokenKind.DOT)
      name <- parseIdentifier()
    } yield DotSelector(name)

  /**
   * Parse an index selection within an EXCEPT clause.
   *
   * IndexSelector := '[' CommaList(Expression) ']'
   *
   * @return The index selection as an AST node with its indices.
   */
  private def parseIndexSelector(): Either[String, IndexSelector] =
    for {
      _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
      indices <- parseCommaListOfExpressions()
      _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
    } yield IndexSelector(indices)

  /**
   * Parse a TLA+ list (aka. a tuple) or a strong stuttering node (e.g. <<A>>_e).
   *
   * Tuple := '<<' CommaList(Expression) '>>'
   *
   * StrongStuttering := '<<' Expression '>>' '_' Expression
   *
   * @return The tuple as an AST node with its elements.
   */
  private def parseTupleOrStrongStuttering(): Either[String, Expression] =
    for {
      _ <- consume(TokenKind.LEFT_ANGLE_BRACKET)
      result <- {
        // If we directly see the closing bracket, we know that this is an empty tuple, so we return it
        if check(TokenKind.RIGHT_ANGLE_BRACKET)
        then for _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET) yield Tuple(Nil)
        else for {
          firstExpression <- parseExpression()
          nonEmptyResult <- if check(List(TokenKind.RIGHT_ANGLE_BRACKET, TokenKind.UNDERSCORE)) then for {
            _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
            _ <- consume(TokenKind.UNDERSCORE)
            stateExpression <- parseExpression()
          } yield StrongStuttering(firstExpression, stateExpression)
          else if check(TokenKind.RIGHT_ANGLE_BRACKET) then for {
            _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
          } yield Tuple(List(firstExpression))
          else for {
            _ <- consume(TokenKind.COMMA)
            tailExpressions <- parseCommaListOfExpressions()
            _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
          } yield Tuple(List(firstExpression) ++ tailExpressions)
        } yield nonEmptyResult
      }
    } yield result

  /**
   * Parse a parenthesized expression.
   *
   * ParenthesizedExpression := '(' Expression ')'
   *
   * @return The parenthesized expression as an AST node.
   */
  private def parseParenthesizedExpression(): Either[String, ParenthesizedExpression] =
    for {
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      expression <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield ParenthesizedExpression(expression)

  /**
   * Parse a weak or strong fairness expression.
   *
   * FairnessExpression := ('WF_' | 'SF_') Expression '(' Expression ')'
   *
   * @return
   */
  private def parseFairnessExpression(): Either[String, Expression] =
    for {
      token <- if check(TokenKind.WF) then consume(TokenKind.WF) else consume(TokenKind.SF)
      _ <- consume(TokenKind.UNDERSCORE)
      state <- parseIdentifierOrTuple()
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      action <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield if token.value == "WF" then WeakFairness(state, action) else StrongFairness(state, action)

  /**
   * Parse an IF-THEN-ELSE expression.
   *
   * IfThenElse := 'IF' Expression 'THEN' Expression 'ELSE' Expression
   *
   * @return
   */
  private def parseIfThenElse(): Either[String, IfThenElse] =
    for {
      _ <- consume(TokenKind.IF)
      cond <- parseExpression()
      _ <- consume(TokenKind.THEN)
      thenExpr <- parseExpression()
      _ <- consume(TokenKind.ELSE)
      elseExpr <- parseExpression()
    } yield IfThenElse(cond, thenExpr, elseExpr)

  /**
   * Parse an CASE expression.
   *
   * CaseExpression := 'CASE' CaseArm [ '[]' CaseArm ]* [ '[]' 'OTHER' '->' Expression ]
   */
  private def parseCaseExpression(): Either[String, CaseExpression] =
    for {
      _ <- consume(TokenKind.CASE)
      firstCaseArm <- parseCaseArm()
      tailCaseArms <- parseCaseArmSeq()
      defaultExpr <- {
        if check(List(TokenKind.SQUARE, TokenKind.OTHER)) then for {
          _ <- consume(TokenKind.SQUARE)
          _ <- consume(TokenKind.OTHER)
          expr <- parseExpression()
        } yield Some(expr) else Right(None)
      }
    } yield CaseExpression(List(firstCaseArm) ++ tailCaseArms, defaultExpr)

  /**
   * Parse a case arm AST node.
   *
   * CaseArm := Expression '->' Expression
   */
  private def parseCaseArm(): Either[String, CaseArm] = for {
    lhs <- parseExpression()
    _ <- consume(TokenKind.RIGHT_ARROW)
    rhs <- parseExpression()
  } yield CaseArm(lhs, rhs)

  /**
   * Parse a sequence of case arm AST nodes.
   *
   * CaseArmSeq := [ '[]' CaseArm ]*
   */
  private def parseCaseArmSeq(): Either[String, List[CaseArm]] = {
    val caseArms = ListBuffer[CaseArm]()
    while (check(TokenKind.SQUARE) && !check(List(TokenKind.SQUARE, TokenKind.OTHER))) {
      (for caseArm <- parseCaseArm() yield caseArm) match {
        case Right(value) => caseArms addOne value
        case Left(value) => return Left(value)
      }
    }
    Right(caseArms.toList)
  }

  /**
   * Parse a conjunction list.
   *
   * E.g. /\ x = 10
   *      /\ y = 11
   *
   * E.g. for nested conjunction lists:
   * /\ /\ A
   *    /\ B
   * /\ /\ C
   *    /\ D
   */
  private def parseConjunctionList(): Either[String, ConjunctionList] =
    val conjuncts = new ListBuffer[Expression]
    while {
      lexer.peek() match {
        case token@Token(TokenKind.AND, value, line, column)
          if !conjunctionStack.exists(t => t.kind == TokenKind.AND && t.column == column)
          // If this is the first AND token of a new conjunction list,
          // we push it onto the stack and continue with the loop.
          => conjunctionStack.push(token); true
        case token@Token(TokenKind.AND, value, line, column)
          if conjunctionStack.headOption.exists(top => top.kind == token.kind && top.column == token.column)
          // If this is an AND token, which is on the same column as the first
          // AND token of the current conjunction list, we continue with the loop.
          => true
        case _ => false
      }
    } do {
      (for {_ <- consume(TokenKind.AND); conjunct <- parseExpression() } yield conjunct) match {
        case Right(value) => conjuncts addOne value
        case Left(value) => {
          // If we fail to parse an expression, we also pop the top element of the stack to "clean it up".
          conjunctionStack.pop()
          return Left(value)
        }
      }
    }
    // Pop the top element of the conjunction stack to remove the conjunction list from the stack.
    conjunctionStack.pop()
    Right(ConjunctionList(conjuncts.toList))

  /**
   * Parse a disjunction list.
   *
   * E.g. \/ x = 10
   *      \/ y = 11
   *
   * E.g. for nested disjunction lists:
   * \/ \/ A
   *    \/ B
   * \/ \/ C
   *    \/ D
   */
  private def parseDisjunctionList(): Either[String, DisjunctionList] =
    val disjuncts = new ListBuffer[Expression]
    while {
      lexer.peek() match {
        case token@Token(TokenKind.OR, value, line, column)
          if !conjunctionStack.exists(t => t.kind == TokenKind.OR && t.column == column)
          // If this is the first OR token of a new disjunction list,
          // we push it onto the stack and continue with the loop.
        => conjunctionStack.push(token); true
        case token@Token(TokenKind.OR, value, line, column)
          if conjunctionStack.headOption.exists(top => top.kind == token.kind && top.column == token.column)
          // If this is an OR token, which is on the same column as the first
          // OR token of the current disjunction list, we continue with the loop.
        => true
        case _ => false
      }
    } do {
      (for {_ <- consume(TokenKind.OR); disjunct <- parseExpression()} yield disjunct) match {
        case Right(value) => disjuncts addOne value
        case Left(value) => {
          // If we fail to parse an expression, we also pop the top element of the stack to "clean it up".
          conjunctionStack.pop()
          return Left(value)
        }
      }
    }
    // Pop the top element of the conjunction stack to remove the disjunction list from the stack.
    conjunctionStack.pop()
    Right(DisjunctionList(disjuncts.toList))

  /*********************************************************************************************************************
   *  EPC Specifications.
   * ******************************************************************************************************************/

  /**
   * Parse an EPC specification.
   *
   * EPCSpec := '--EPC' [ '(' CommaList(Identifier) ')' ]? Identifier EPCSpecUnit*
   *
   */
  def parseEPCSpec(): Either[String, EPCSpec] =
    for {
      _ <- consume(TokenKind.EPC_CONTEXT_OPEN)
      modifiers <- if (check(TokenKind.LEFT_ROUND_BRACKET)) then for {
          _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
          modifiers <- parseCommaListOfIdentifiers()
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield modifiers
        else Right(Nil)
      identifier <- parseIdentifier()
      specUnits <- parseEPCSpecUnits()
      _ <- consume(TokenKind.EPC_CONTEXT_CLOSE)
    } yield EPCSpec(identifier, modifiers, specUnits)

  /**
   * Parse a single EPC specification unit.
   *
   * EPCSpecUnit := ChannelDefinition | ThreadDefinition | Network
   */
  private def parseEPCSpecUnit(): Either[String, EPCSpecUnit] =
    lexer.peek() match {
      case Token(TokenKind.EPC_NEW, _, _, _) => parseChannelDefinition()
      case Token(TokenKind.EPC_DEF, _, _, _) => parseThreadDefinition()
      case _ => parseNetwork()
    }

  /**
   * Parse a sequence of EPC specification units.
   *
   * @return A list of consumed EPC specification units.
   */
  private def parseEPCSpecUnits(): Either[String, List[EPCSpecUnit]] = {
    val units = new ListBuffer[EPCSpecUnit]
    while (!check(TokenKind.EPC_CONTEXT_CLOSE) && !check(TokenKind.EOF)) {
      (for specUnit <- parseEPCSpecUnit() yield specUnit) match {
        case Right(value) => units addOne value
        case Left(value) => return Left(value)
      }
    }
    Right(units.toList)
  }

  /**
   * Parses an EPC enumerator.
   *
   * Enumerator := '(' Identifier ['=' | '\in'] Expression ')'
   * BoundedEnumeratorWithFilter := '(' Identifier ['=' | '\in'] Expression ':' Expression ')'
   *
   * E.g.
   * Bounded enumerators: (A = 1), (Q = "a"), (p \in 1..10), (q \in {"a", "b"}), ...
   * Bounded enumerators with filters: (j \in 1..10 : j # self)
   */
  def parseEnumerator(): Either[String, Enumerator] =
    for {
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      identifier <- parseIdentifier()
      token <- if check(TokenKind.EQ) then consume(TokenKind.EQ) else consume(TokenKind.SET_IN)
      domain <- parseExpression()
      enumerator <-
        if check(TokenKind.COLON) then for {
          _ <- consume(TokenKind.COLON)
          guardExpr <- parseExpression()
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield BoundedEnumeratorWithFilter(identifier, token.value, domain, guardExpr)
        else for {
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield BoundedEnumerator(identifier, token.value, domain)
    } yield enumerator

  /**
   * Parse a channel definition.
   *
   * ChannelDefinition := 'new' Identifier
   *
   * E.g. new c
   */
  def parseChannelDefinition(): Either[String, ChannelDefinition] =
    for {
      _ <- consume(TokenKind.EPC_NEW)
      identifier <- parseIdentifier()
    } yield ChannelDefinition(identifier)

  /**
   * Parse a travelling message.
   *
   * TravellingMessage := Identifier '_' '{' Literal '->' Literal '}' '<<' Expression '>>'
   *
   * E.g. c_{1 -> 2}<< "Hello" >>, c_{"a" -> "b"}<< "Hello again" >>, ...
   */
  def parseTravellingMessage(): Either[String, TravellingMessage] =
    for {
      channel <- parseIdentifier()
      // _ gets recognized as part of the identifier.
      // _ <- consume(TokenKind.UNDERSCORE)
      _ <- consume(TokenKind.LEFT_CURLY_BRACKET)
      source <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ARROW)
      target <- parseExpression()
      _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
      _ <- consume(TokenKind.LEFT_ANGLE_BRACKET)
      message <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
    } yield TravellingMessage(channel, source, target, message)

  /**
   * Parse a comma-separated list of travelling messages.
   */
  private def parseCommaListOfTravellingMessages(): Either[String, List[TravellingMessage]] =
    parseCommaList[TravellingMessage](() => parseTravellingMessage())

  /**
   * Parse an outgoing message.
   *
   * OutgoingMessage := Expression '@' Expression '<<' Expression '>>'
   *
   * E.g. c@2<<"Hello">>, c@"a"<< <<1,2,3>> >>
   */
  def parseOutgoingMessage(): Either[String, OutgoingMessage] =
    for {
      channel <- parseExpression()
      _ <- consume(TokenKind.AT)
      target <- parseExpression()
      _ <- consume(TokenKind.LEFT_ANGLE_BRACKET)
      value <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
    } yield OutgoingMessage(channel, target, value)

  /**
   * Parse a comma-separated list of outgoing messages.
   */
  private def parseCommaListOfOutgoingMessages(): Either[String, List[OutgoingMessage]] =
    parseCommaList[OutgoingMessage](() => parseOutgoingMessage())

  /**
   * Parse an empty explicit multiset.
   *
   * ExplicitMultiSet := '{{' '}}'
   */
  private def parseEmptyExplicitMultiSet[T](): Either[String, MultiSet[T]] =
    for {
      _ <- consume(TokenKind.EPC_MULTISET_OPEN)
      _ <- consume(TokenKind.EPC_MULTISET_CLOSE)
    } yield ExplicitMultiSet(Nil)

  /**
   * Parse an explicit multiset.
   *
   * ExplicitMultiSet := '{{' CommaList(Expression) '}}'
   */
  private def parseExplicitMultiSet[T](consumer: () => Either[String, List[T]]): Either[String, MultiSet[T]] =
    for {
      _ <- consume(TokenKind.EPC_MULTISET_OPEN)
      values <- consumer()
      _ <- consume(TokenKind.EPC_MULTISET_CLOSE)
    } yield ExplicitMultiSet(values)

  /**
   * Parse an explicit multiset of travelling messages.
   */
  def parseExplicitMultiSetOfTravellingMessages(): Either[String, MultiSet[TravellingMessage]] =
    parseExplicitMultiSet[TravellingMessage](() => parseCommaListOfTravellingMessages())

  /**
   * Parse an explicit multiset of outgoing messages.
   */
  def parseExplicitMultiSetOfOutgoingMessages(): Either[String, MultiSet[OutgoingMessage]] =
    parseExplicitMultiSet[OutgoingMessage](() => parseCommaListOfOutgoingMessages())

  /**
   * Parse a quantified multiset.
   *
   * QuantifiedMultiSet := '{{' Enumerator '}}'
   */
  private def parseQuantifiedMultiSet[T](consumer: () => Either[String, T]) =
    for {
      _ <- consume(TokenKind.EPC_MULTISET_OPEN)
      enumerator <- parseEnumerator()
      value <- consumer()
      _ <- consume(TokenKind.EPC_MULTISET_CLOSE)
    } yield QuantifiedMultiSet(enumerator, value)

  /**
   * Parse a quantified multiset of travelling messages.
   */
  def parseQuantifiedMultiSetOfTravellingMessages(): Either[String, MultiSet[TravellingMessage]] =
    parseQuantifiedMultiSet[TravellingMessage](() => parseTravellingMessage())

  /**
   * Parse a quantified multiset of outgoing messages.
   */
  def parseQuantifiedMultiSetOfOutgoingMessages(): Either[String, MultiSet[OutgoingMessage]] =
    parseQuantifiedMultiSet[OutgoingMessage](() => parseOutgoingMessage())

  /**
   * Action := Declaration | Assignment | Receive | Send
   */
  private def parseAction(): Either[String, Action] =
    for {
      action <-
        if check(TokenKind.EPC_VAR)
          then parseDeclaration()
        else if check(TokenKind.LEFT_ANGLE_BRACKET)
          then parseAssignment()
        else if check(TokenKind.IDENTIFIER)
          then parseReceive()
        else if check(TokenKind.EPC_MULTISET_OPEN)
          then parseSend()
        else Left(error("EPC action expected"))
    } yield action

  /**
   * Parse declaration of a variable.
   *
   * Declaration := 'var' Identifier
   *
   * E.g. var x, var y, ...
   */
  def parseDeclaration(): Either[String, Declaration] =
    for {
      _ <- consume(TokenKind.EPC_VAR)
      identifier <- parseIdentifier()
    } yield Declaration(identifier)

  /**
   * Parse an assignment to a variable.
   *
   * Assignment := '<<' Identifier ':=' Expression '>>'
   *
   * E.g. << x := 10 >>
   */
  def parseAssignment(): Either[String, Assignment] =
    for {
      _ <- consume(TokenKind.LEFT_ANGLE_BRACKET)
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.EPC_ASSIGN)
      expression <- parseExpression()
      _ <- consume(TokenKind.RIGHT_ANGLE_BRACKET)
    } yield Assignment(identifier, expression)

  /**
   * Parse a "receive" action.
   *
   * Receive := Expression '@' Expression '?' '(' Identifier ')'
   *
   * e.g. c@2?(x)
   */
  def parseReceive(): Either[String, Receive] =
    for {
      channel <- parseExpression()
      _ <- consume(TokenKind.AT)
      location <- parseExpression()
        _ <- consume(TokenKind.QUESTION_MARK)
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      variable <- parseIdentifier()
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield Receive(channel, location, variable)

  /**
   * Parse a send action.
   *
   * Send := MultiSet(OutgoingMessage)
   *
   * e.g. {{ c@1<< ... >>,  c@2<< ... >> }}, {{ (i \in 1..2 : i # self) c@i<< ... >> }}
   */
  def parseSend(): Either[String, Send] =
    for {
      multiSet <-
        if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.EQ))
          then parseQuantifiedMultiSetOfOutgoingMessages()
        else if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.SET_IN))
          then parseQuantifiedMultiSetOfOutgoingMessages()
        else if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.EPC_MULTISET_CLOSE))
          then parseEmptyExplicitMultiSet()
        else parseExplicitMultiSetOfOutgoingMessages()
    } yield Send(multiSet)

  /**
   * Parse a command.
   *
   * Command := Action | ConditionalThread | Instantiation
   */
  private def parseCommand(): Either[String, Command] =
    if check(List(TokenKind.IDENTIFIER, TokenKind.LEFT_ROUND_BRACKET, TokenKind.RIGHT_ROUND_BRACKET))
      then parseThreadInstantiation()
    else lexer.peek() match {
      // Actions
      case Token(TokenKind.EPC_VAR, _, _, _) => parseAction()
      case Token(TokenKind.LEFT_ANGLE_BRACKET, _, _, _) => parseAction()
      case Token(TokenKind.IDENTIFIER, _, _, _) => parseAction()
      case Token(TokenKind.EPC_MULTISET_OPEN, _, _, _) => parseAction()
      // Conditional Threads
      case Token(TokenKind.EPC_IF, _, _, _) => parseConditionalThread()
      case _ => Left(error("EPC command expected"))
    }

  /**
   * Parse a thread instantiation.
   *
   * ThreadInstantiation := Identifier'()'
   *
   * E.g. foo(), bar()
   */
  private def parseThreadInstantiation(): Either[String, ThreadInstantiation] =
    for {
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield ThreadInstantiation(identifier)

  /**
   * Parse a conditional thread.
   *
   * ConditionalThread := 'if' Expression 'then' Thread 'else' Thread 'fi'
   *
   * E.g. if ... then ... else ... fi
   */
  private def parseConditionalThread(): Either[String, ConditionalThread] =
    for {
      _ <- consume(TokenKind.EPC_IF)
      condition <- parseExpression()
      _ <- consume(TokenKind.EPC_THEN)
      thenThread <- parseThread()
      _ <- consume(TokenKind.EPC_ELSE)
      elseThread <- parseThread()
      _ <- consume(TokenKind.EPC_FI)
    } yield ConditionalThread(condition, thenThread, elseThread)

  /**
   * ThreadUnit := [Identifier ':']? Command
   *
   * E.g. (declareX: var x) or (assignX: << x := 10 >>)
   */
  private def parseThreadUnit(): Either[String, ThreadUnit] =
    if check(List(TokenKind.IDENTIFIER, TokenKind.COLON)) then for {
      identifier <- parseIdentifier()
      _ <- consume(TokenKind.COLON)
      command <- parseCommand()
    } yield ThreadUnit(Some(identifier), command)
    else for {
      command <- parseCommand()
    } yield ThreadUnit(None, command)

  /**
   * Set with the possible first tokens of a EPC thread unit.
   */
  private val threadUnitFirstSet = Set(
    TokenKind.IDENTIFIER,
    TokenKind.EPC_VAR,
    TokenKind.LEFT_ANGLE_BRACKET,
    TokenKind.EPC_MULTISET_OPEN,
    TokenKind.EPC_IF)

  /**
   * Check if the next token is one of the possible first tokens of a thread unit.
   */
  private def checkThreadUnitFirstSet(): Boolean =
    lexer.peek() match {
      case Token(kind, value, _, _) if threadUnitFirstSet contains kind => true
      case _ => false
    }

  /**
   * Thread := Identifier ':' Command | Thread ';' Thread
   *
   * E.g. ( var x; << x := 10 >> ) or ( declareX: var x; assign10: << x := 10 >> )
   */
  def parseThread(): Either[String, Thread] =
    val units = new ListBuffer[ThreadUnit]
    while {
      (for threadUnit <- parseThreadUnit() yield threadUnit) match {
        case Right(value) => units addOne value
        case Left(value) => return Left(value)
      }
      val repeatCondition = check(TokenKind.SEMICOLON)
      if (!repeatCondition && checkThreadUnitFirstSet()) {
        return Left(error("sequencing operator ';' not found before next command"))
      }
      if (repeatCondition) {
        consume(TokenKind.SEMICOLON) match {
          case Right(token) if !checkThreadUnitFirstSet() =>
            return Left(error(token, "sequencing operator ';' not followed by a command"))
          case _ =>
        }
      }
      repeatCondition
    }
    do ()
    Right(units.toList)

  /**
   * ThreadComprehension := Enumerator List(Command)
   *
   * E.g. (j = 1) (var x; << x := 10 >>)
   *      (j \in 1..10) (<< x := j >>)
   */
  private def parseThreadComprehension(): Either[String, ThreadComprehension] =
    for {
      enumerator <- parseEnumerator()
      _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
      thread <- parseThread()
      _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
    } yield ThreadComprehension(enumerator, thread)

  /**
   * ThreadComposition := ThreadComprehension '|' ThreadComprehension
   *
   * E.g. (j = 1) (<< x := j >>) | (k = 2) (<< x := k >>)
   */
  def parseThreadComposition(): Either[String, OuterThread] =
    for {
      lhs <- parseThreadComprehension()
      finalResult <- {
        def loop(lhs: OuterThread): Either[String, OuterThread] = {
          if check(TokenKind.EPC_THREAD_COMPOSITION) then for {
            _ <- consume(TokenKind.EPC_THREAD_COMPOSITION)
            rhs <- parseThreadComprehension()
            intermediateResult <- loop(ThreadComposition(lhs, rhs))
          } yield intermediateResult
          else Right(lhs)
        }
        loop(lhs)
      }
    } yield finalResult

  /**
   * ThreadDefinition := 'def' Identifier [ '(' CommaList(Identifier) ')' ] ? '=' Thread
   *
   * E.g. def declareXAndStop = var x. 0
   *      def assignXAndStop(x) = << x := 10 >> . 0
   */
  def parseThreadDefinition(): Either[String, ThreadDefinition] =
    for {
      _ <- consume(TokenKind.EPC_DEF)
      identifier <- parseIdentifier()
      freeVariables <-
        if check(TokenKind.LEFT_ROUND_BRACKET) then for {
          _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
          identifiers <- parseCommaListOfIdentifiers()
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield identifiers
        else Right(Nil)
      _ <- consume(TokenKind.EQ)
      body <- parseThread()
    } yield ThreadDefinition(identifier, freeVariables, body)

  /**
   * Parse a network.
   *
   * Network := Location | MessageAether | NetworkComposition
   */
  def parseNetwork(): Either[String, Network] =
    parseNetworkComposition()

  /**
   * Parse a network composition.
   *
   * NetworkComposition := Network '||' Network
   *
   * E.g. (p = 1) [[ [x |-> Initialized] <| c@2(x) . 0 ]] || {{ c_{2 -> 1}<< "Hello from 2" >> }}
   */
  private def parseNetworkComposition(): Either[String, Network] =
    for {
      lhs <- parseLocationOrMessageAether()
      finalResult <- {
        def loop(lhs: Network): Either[String, Network] = {
          if check(TokenKind.EPC_LOC_COMPOSITION) then for {
            _ <- consume(TokenKind.EPC_LOC_COMPOSITION)
            rhs <- parseLocationOrMessageAether()
            intermediateResult <- loop(NetworkComposition(lhs, rhs))
          } yield intermediateResult
          else Right(lhs)
        }
        loop(lhs)
      }
    } yield finalResult

  /**
   * Parse a location, a message aether, or network within parentheses.
   *
   * LocationOrMessageAether := Location | MessageAether
   *
   */
  private def parseLocationOrMessageAether(): Either[String, Network] =
    // Locations
    if check(List(TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.EQ))
      then parseLocation()
    else if check(List(TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.SET_IN))
      then parseLocation()
    // Message Aether
    else if check(TokenKind.EPC_MULTISET_OPEN)
      then parseMessageAether()
    // Network within parenthesis
    else if check(TokenKind.LEFT_ROUND_BRACKET)
      then for {
        _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
        network <- parseNetwork()
        _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
      } yield network
    else Left(error("Network expected"))

  /**
   * Parse a location.
   *
   * Location := Enumerator Process
   *
   * Examples:
   * (p = 1) [[ [x |-> Undefined] |> var x. << x := 10 >>. 0 ]]
   * (p \in 1..10) [[ [x |-> Undefined] |> var x. << x := 10 >>. 0 ]]
   */
  private def parseLocation(): Either[String, Location] =
    for {
      enumerator <- parseEnumerator()
      process <- parseProcess()
    } yield Location(enumerator, process)

  /**
   * Parse a process.
   *
   * Process := '[[' ExplicitFunction '<|' Thread ']]'
   *
   * E.g. [[ [x |-> Undefined] <| var x. << x := 10 >> . 0 ]]
   */
  def parseProcess(): Either[String, Process] =
    for {
      _ <- consume(TokenKind.EPC_LOC_SCOPE_OPEN)
      memory <- parseMemory()
      _ <- consume(TokenKind.EPC_LOC_MEMORY_CODE_SEP)
      thread <- parseThreadComposition()
      _ <- consume(TokenKind.EPC_LOC_SCOPE_CLOSE)
    } yield Process(memory, thread)

  /**
   * Parse a process memory.
   *
   * ProcessMemory := ExplicitFunction
   *
   */
  private def parseMemory(): Either[String, ExplicitFunction] =
    if check(TokenKind.SQUARE)
    then for { _ <- consume(TokenKind.SQUARE) } yield ExplicitFunction(Nil)
    else for {
      _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
      mappings <- parseCommaListOfMappings()
      _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
    } yield ExplicitFunction(mappings)

  /**
   * Parse a message aether.
   *
   * MessageAether := MultiSet(TravellingMessage)
   *
   * E.g. {{ c_{1 -> 2}<< ... >>, c_{"a" -> "b"}<< ... >> }} or {{ (i \in 1..2) c@i<< ... >> }}
   */
  private def parseMessageAether(): Either[String, MessageAether] =
    for {
      multiSet <-
        if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.EQ))
          then parseQuantifiedMultiSetOfTravellingMessages()
        else if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.LEFT_ROUND_BRACKET, TokenKind.IDENTIFIER, TokenKind.SET_IN))
          then parseQuantifiedMultiSetOfTravellingMessages()
        else if check(List(TokenKind.EPC_MULTISET_OPEN, TokenKind.EPC_MULTISET_CLOSE))
          then parseEmptyExplicitMultiSet()
        else parseExplicitMultiSetOfTravellingMessages()
    } yield MessageAether(multiSet)

}
