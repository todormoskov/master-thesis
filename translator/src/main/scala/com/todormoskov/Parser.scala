package com.todormoskov

import com.todormoskov.Lexer.*

import scala.collection.mutable.ListBuffer

sealed trait AST
case class ExtendsStatement(extendedModules: List[String]) extends AST
case class ConstantDeclaration(constants: List[String]) extends AST
case class VariableDeclaration(variables: List[String]) extends AST

sealed trait Expression extends AST
case class Literal(value: String |  Integer | Boolean) extends Expression
case class Identifier(name: String) extends Expression
case class OperatorApplication(operatorName: String, operands: List[Expression]) extends Expression
case class FunctionApplication(expression: Expression, operands: List[Expression]) extends Expression

sealed trait Set extends Expression
case class FilteredSet(variable: Identifier, domain: Expression, predicate: Expression) extends Set
case class ExplicitSet(values: List[Expression]) extends Set
case class MappedSet(mappingExpression: Expression, variable: Identifier, domain: Expression) extends Set

case class Statement() extends AST
case class Module(name: String, extendsStmt: ExtendsStatement, statements: List[Statement]) extends AST

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
      else Left(s"Syntax error: token of kind $expected not found!")

  /**
   * Check if the next token is a literal.
   *
   * @return True if the next token is a literal, else false.
   */
  private def checkLiteral(): Boolean = {
    check(TokenKind.TRUE)
      || check(TokenKind.TRUE)
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
  def parseLiteral(): Either[String, Literal] = {
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
    else Left("Error: Literal not recognized")
  }

  /**
   * Parse an identifier.
   *
   * @return Identifier as AST-Node.
   */
  def parseIdentifier(): Either[String, Identifier] = {
    for token <- consume(TokenKind.IDENTIFIER) yield Identifier(token.value)
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
   * Parses a comma-separated list of identifiers.
   *
   * @return A list of parsed identifier tokens.
   */
  private def parseCommaListOfIdentifiers: Either[String, List[Token]] =
    parseCommaList[Token](() => consume(TokenKind.IDENTIFIER))

  /**
   * Parse an EXTENDS statement.
   *
   * ExtendsStmt := ['EXTENDS' Identifier [',' Identifier]* ]?
   *
   * @return
   */
  def parseExtendsStatement(): Either[String, ExtendsStatement] =
    if check(TokenKind.EXTENDS) then for {
        _ <- consume(TokenKind.EXTENDS)
        extendedModules <- parseCommaListOfIdentifiers
      } yield ExtendsStatement(extendedModules.map(_.value))
    else Right(ExtendsStatement(Nil))

  /**
   * Parse a constant declaration.
   *
   * [ ['CONSTANT' | 'CONSTANTS'] Identifier [',' Identifier]* ]?
   *
   * @return
   */
  def parseConstantDeclaration(): Either[String, ConstantDeclaration] =
    if check(TokenKind.CONSTANT) || check(TokenKind.CONSTANTS) then for {
        _ <- if check(TokenKind.CONSTANT) then consume(TokenKind.CONSTANT) else consume(TokenKind.CONSTANTS)
        declaredConstants <- parseCommaListOfIdentifiers
      } yield ConstantDeclaration(declaredConstants.map(_.value))
    else Right(ConstantDeclaration(Nil))

  /**
   * Parse a variable declaration.
   *
   * [ ['VARIABLE' | 'VARIABLES'] Identifier [',' Identifier]* ]?
   *
   * @return
   */
  def parseVariableDeclaration(): Either[String, VariableDeclaration] =
    if check(TokenKind.VARIABLE) || check(TokenKind.VARIABLES) then for {
        _ <- if check(TokenKind.VARIABLE) then consume(TokenKind.VARIABLE) else consume(TokenKind.VARIABLES)
        declaredVariables <- parseCommaListOfIdentifiers
      } yield VariableDeclaration(declaredVariables.map(_.value))
    else Right(VariableDeclaration(Nil))

  /**
   * Parse an expression.
   * @return
   */
  def parseExpression(): Either[String, Expression] =
    parseFunctionApplication()

  /**
   * Parse a comma-separated list of expressions.
   *
   * @return A list of parsed expressions.
   */
  private def parseCommaListOfExpressions: Either[String, List[Expression]] =
    parseCommaList[Expression](() => parseExpression())

  /**
   * Parse a function application.
   *
   * FunctionApplication := Expression [ '[' Expression [',' Expression]* ']' ]?
   *
   * @return The function application as an AST-Node.
   */
  private def parseFunctionApplication(): Either[String, Expression] =
    for {
      expression <- parsePrimaryExpression()
      maybeFunctionApplication <- parseFunctionApplicationArguments(expression)
    } yield maybeFunctionApplication

  /**
   * Parses the arguments of a function application.
   * This function is recursive.
   * If the opening bracket is not found, the function
   * simply returns the given function expression.
   * If the opening bracket is found, the function consumes it,
   * consumes the comma separated list of expressions, and the
   * closing bracket. Before returning it tries to parse further
   * function applications to arguments. (E.g. as in f[0][0].)
   *
   * FunctionApplicationArguments := [ '[' CommaList(Expression) ']' ]*
   *
   * @param appliedFunction The function that is applied.
   * @return The function application or expression as an AST-Node.
   */
  private def parseFunctionApplicationArguments(appliedFunction: Expression): Either[String, Expression] = {
    if check(TokenKind.LEFT_SQUARE_BRACKET) then for {
      _ <- consume(TokenKind.LEFT_SQUARE_BRACKET)
      operands <- parseCommaListOfExpressions
      _ <- consume(TokenKind.RIGHT_SQUARE_BRACKET)
      functionApplication <- parseFunctionApplicationArguments(FunctionApplication(appliedFunction, operands))
    } yield functionApplication
    else Right(appliedFunction)
  }

  /**
   * Parses a primary expression.
   *
   * PrimaryExpr := OperatorApplication | Identifier | Literal | Set
   *
   * @return The primary expression as an AST-Node.
   */
  private def parsePrimaryExpression(): Either[String, Expression] =
    if check(List(TokenKind.IDENTIFIER, TokenKind.LEFT_ROUND_BRACKET))
      then for operatorApplication <- parseOperatorApplication() yield operatorApplication
    else if check(TokenKind.IDENTIFIER)
      then for identifier <- parseIdentifier() yield identifier
    else if checkLiteral()
      then for literal <- parseLiteral() yield literal
    else if check(TokenKind.LEFT_CURLY_BRACKET)
      then for set <- parseSet() yield set
    else Left("Syntax Error: Expression expected")

  /**
   * Parse an operator application.
   *
   * OperatorApplication := Identifier ['(' Expression [',' Expression]* ')']?
   *
   * @return The operator application as an AST-Node.
   */
  private def parseOperatorApplication(): Either[String, OperatorApplication] =
    for {
      operatorName <- consume(TokenKind.IDENTIFIER)
      operands <- if check(TokenKind.LEFT_ROUND_BRACKET) then for {
          _ <- consume(TokenKind.LEFT_ROUND_BRACKET)
          parsedOperators <- parseCommaListOfExpressions
          _ <- consume(TokenKind.RIGHT_ROUND_BRACKET)
        } yield parsedOperators
        else Right(Nil)
    } yield OperatorApplication(operatorName.value, operands)

  /**
   * Parses a set.
   *
   * Set := FilteredSet | MappedSet | ExplicitlyGivenSet
   *
   * @return The set as an AST-Node.
   */
  private def parseSet(): Either[String, Set] =
    if check(List(TokenKind.LEFT_CURLY_BRACKET, TokenKind.IDENTIFIER, TokenKind.SET_IN))
      then parseFilteredSetConstructor()
    else parseExplicitOrMappedSet()

  /**
   * Parses a filtered set constructor.
   *
   * FilteredSetConstructor := '{' Identifier '\in' Expression : Expression '}'
   *
   * @return The filtered set an AST-Node.
   */
  private def parseFilteredSetConstructor(): Either[String, FilteredSet] =
    for {
      _ <- consume(TokenKind.LEFT_CURLY_BRACKET)
      variable <- parseIdentifier()
      _ <- consume(TokenKind.SET_IN)
      domain <- parseExpression()
      _ <- consume(TokenKind.COLON)
      mappingExpression <- parseExpression()
      _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
    } yield FilteredSet(variable, domain, mappingExpression)

  /**
   * Parses a mapped or an explicitly given set.
   *
   * MappedSetConstructor := '{' Expression : Identifier '\in' Expression '}'
   *
   * ExplicitlyGivenSet := '{' CommaList(Expression)? '}'
   *
   * @return The mapped or explicitly-given set as an AST-Node.
   */
  private def parseExplicitOrMappedSet(): Either[String, Set] =
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
            explicitOrMappedSet <- {
              // And we check if after the expression we have a colon
              // If this is the case, we know that we have a mapped set
              if check(TokenKind.COLON) then for {
                // Here we parse the rest as if we are parsing a mapped set
                _ <- consume(TokenKind.COLON)
                variable <- parseIdentifier()
                _ <- consume(TokenKind.SET_IN)
                expression <- parseExpression()
                _ <- consume(TokenKind.RIGHT_CURLY_BRACKET)
              } yield MappedSet(firstExpr, variable, expression)
              // If we find a comma instead, we know that we have
              // an explicitly given set of expressions, and we can
              // simply parse them
              else if check(TokenKind.COMMA) then for {
                _ <- consume(TokenKind.COMMA)
                followingExpressions <- parseCommaListOfExpressions
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
          } yield explicitOrMappedSet
        }
      }
    } yield set




  /**
   * Parse a TLA+ module.
   *
   * Module :=      MinusSeparator 'MODULE' Identifier MinusSeparator
   *                ['EXTENDS' Identifier [',' Identifier]* ]?
   *                Statement*
   *                EqualSeparator
   * @return
   */
  /*
  def parseModule(): Either[String, Module] = {
    for {
      _ <- consume(TokenKind.MINUS_SEPARATOR)
      _ <- consume(TokenKind.MODULE)
      moduleName <- consume(TokenKind.IDENTIFIER)
      _ <- consume(TokenKind.MINUS_SEPARATOR)
      extendsStmt <- parseExtendsStatement()
      _ <- consume(TokenKind.EQUAL_SEPARATOR)
    } yield Module(moduleName.value, extendsStmt, null)
  }
  */
}
