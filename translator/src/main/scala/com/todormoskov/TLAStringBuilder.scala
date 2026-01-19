package com.todormoskov

import com.todormoskov.Lexer.Token

import scala.collection.mutable.ListBuffer

class TLAStringBuilder {
  private val generatedModuleUnits = ListBuffer.empty[ModuleUnit]
  private val sb: StringBuilder = StringBuilder()
  private var line = 1
  private var column = 1

  def appendUnit(moduleUnit: ModuleUnit, newlines: Int = 1): Unit = {
    generatedModuleUnits append moduleUnit
    moduleUnit match {
      case declaration: VariableDeclaration => append(declaration)
      case declaration: ConstantDeclaration => append(declaration)
      case definition: OperatorDefinition => append(definition)
      case _ => println("...")
    }
    if (newlines > 0) {
      for (i <- 1 to newlines) appendNewline()
    }
  }

  def appendString(str: String): Either[String, Unit] = {
    if str.contains(System.lineSeparator())
    then Left("Newlines should only be added through the appendNewlines()-method!")
    else {
      sb append str
      column = column + str.length
      Right(())
    }
  }

  def appendNewline(): Unit = {
    sb append System.lineSeparator()
    line = line + 1
    column = 1
  }

  def getModuleUnits: List[ModuleUnit] = generatedModuleUnits.toList

  def getSpec: String = sb.toString()

  private def append(variableDeclaration: VariableDeclaration): Unit = {
    val VariableDeclaration(variableToken, identifiers) = variableDeclaration
    appendString(variableToken.value)
    appendString(" ")
    appendExpressions(identifiers, ", ")
  }

  private def append(constantDeclaration: ConstantDeclaration): Unit = {
    val ConstantDeclaration(constantToken, constants) = constantDeclaration
    appendString(constantToken.value)
    appendString(" ")
    constants.zipWithIndex.foreach((operatorDeclaration, idx) => {
      append(operatorDeclaration)
      if (idx != constants.length - 1) appendString(", ")
    })
  }

  private def append(operatorDeclaration: OperatorDeclaration): Unit = {
    val OperatorDeclaration(operator, arguments) = operatorDeclaration
    operator match {
      case Identifier(name) => appendString(name)
      case Token(kind, value, _, _) => appendString(value)
    }
    arguments match {
      case Nil =>
      case _ =>
        val argsAsIdentifiers = arguments.map(token => Identifier(token.value))
        appendExpressions(argsAsIdentifiers, "(", ", ", ")")
    }
  }

  def append(operatorDefinition: OperatorDefinition): Unit = {
    val OperatorDefinition(signature, body) = operatorDefinition
    val NonFixLHS(identifier, arguments) = signature
    appendExpression(identifier)
    arguments match {
      case Nil =>
      case _ =>
        appendString("(")
        arguments.zipWithIndex.foreach((identifierOrOperatorDeclaration, idx) => {
          identifierOrOperatorDeclaration match {
            case identifier: Identifier => appendExpression(identifier)
            case operatorDeclaration: OperatorDeclaration => append(operatorDeclaration)
          }
          if (idx != arguments.length - 1) appendString(", ")
        })
        appendString(")")
    }
    appendString(" == ")
    appendExpression(body)
  }

  def appendExpression(expression: Expression): Unit = {
    expression match {
      case Literal(value) =>
        value match {
          case strValue: String =>
            appendString("\"")
            appendString(strValue)
            appendString("\"")
          case intValue: Integer =>
            appendString(intValue.toString)
          case true =>
            appendString("TRUE")
          case false =>
            appendString("FALSE")
        }
      case Identifier(name) => appendString(name)
      case IdentifierTuple(Nil) => appendString("<< >>")
      case IdentifierTuple(identifiers) => appendExpressions(identifiers, "<<", ", ", ">>")
      case QuantifierBound(variable, domain) =>
        val quantifier: Unit = variable match {
          case identifier: Identifier => appendExpression(identifier)
          case identifiers: List[Identifier] =>
            appendExpressions(identifiers, ", ")
          case tuple: IdentifierTuple => appendExpression(tuple)
        }
        appendString(" \\in ")
        appendExpression(domain)
      case Mapping(identifier, expression) =>
        appendExpression(identifier)
        appendString(" |-> ")
        appendExpression(expression)
      case Tuple(Nil) => appendString("<< >>")
      case Tuple(expressions) => appendExpressions(expressions, "<<", ", ", ">>")
      case UnaryExpression(operator, expression) =>
        // Some unary operators look better without space after them.
        if (Set("~", "-", "[]", "<>") contains operator) {
          appendString(operator)
        } else {
          appendString(operator)
          appendString(" ")
        }
        appendExpression(expression)
      case BinaryExpression(operator, lhs, rhs) =>
        appendExpression(lhs)
        // Some binary operators look better without space around them.
        if (Set(".", "..") contains operator) {
          appendString(operator)
        } else {
          appendString(" ")
          appendString(operator)
          appendString(" ")
        }
        appendExpression(rhs)
      case IdentifierApplication(identifier, operands) =>
        appendExpression(identifier)
        appendExpressions(operands, "(", ", ", ")")
      case GeneralIdentifierApplication(identifier, operands) =>
        appendExpression(identifier)
        appendExpressions(operands, "(", ", ", ")")
      case FunctionApplication(expression, operands) =>
        appendExpression(expression)
        appendExpressions(operands, "[", ", ", "]")
      case PrimedExpression(expression) =>
        appendExpression(expression)
        appendString("'")
      case ParenthesizedExpression(expression) =>
        appendString("(")
        appendExpression(expression)
        appendString(")")
      case IfThenElse(cond, thenExpr, elseExpr) =>
        // We calculate the beginning of the THEN keyword
        val thenIndentation = column + 1
        appendString("IF ")
        appendExpression(cond)
        // We add a new line, indent the row and write the "THEN ..." part
        appendNewline()
        appendString(" ".repeat(thenIndentation))
        appendString("THEN ")
        appendExpression(thenExpr)
        // We do this one more time for the "ELSE ..." part
        appendNewline()
        appendString(" ".repeat(thenIndentation))
        appendString("ELSE ")
        appendExpression(elseExpr)
      case CaseExpression(caseArms, defaultExpression) =>
        // We get the beginning of the CASE keyword
        val caseIndentation = column - 1
        caseArms.zipWithIndex.foreach((caseArm, idx) => {
          if (idx == 0) {
            appendString("CASE ")
          }
          else {
            appendNewline()
            appendString(" ".repeat(caseIndentation))
            appendString("[]   ")
          }
          appendExpression(caseArm.lhs)
          appendString(" -> ")
          appendExpression(caseArm.rhs)
        })
        defaultExpression.foreach(expression => {
          appendNewline()
          appendString(" ".repeat(caseIndentation))
          appendString("[] OTHER -> ")
          appendExpression(expression)
        })
      case ChooseExpression(identifier, domain, predicate) =>
        appendString("CHOOSE ")
        appendExpression(identifier)
        appendString(" \\in ")
        appendExpression(domain)
        appendString(" : ")
        appendExpression(predicate)
      case BoundedQuantifiedExpression(quantifier, binders, body) =>
        // We calculate the beginning of the body
        val bodyIndentation = column + 1
        appendString(quantifier)
        appendString(" ")
        appendExpressions(binders, ", ")
        appendString(":")
        // We add a new line, indent the row and write the body
        appendNewline()
        appendString(" ".repeat(bodyIndentation))
        appendExpression(body)
      case QuantifiedExpression(quantifier, binders, body) =>
        // We calculate the beginning of the body
        val bodyIndentation = column + 1
        appendString(quantifier)
        appendString(" ")
        appendExpressions(binders, ", ")
        appendString(":")
        // We add a new line, indent the row and write the body
        appendNewline()
        appendString(" ".repeat(bodyIndentation))
        appendExpression(body)
      case ConjunctionList(listItems) =>
        val conjunctionListIndentation = column - 1
        listItems.zipWithIndex.foreach((expression, idx) => {
          if (idx != 0) {
            appendNewline()
            appendString(" ".repeat(conjunctionListIndentation))
          }
          appendString("/\\ ")
          appendExpression(expression)
        })
      case DisjunctionList(listItems) =>
        val disjunctionListIndentation = column - 1
        listItems.zipWithIndex.foreach((expression, idx) => {
          if (idx != 0) {
            appendNewline()
            appendString(" ".repeat(disjunctionListIndentation))
          }
          appendString("\\/ ")
          appendExpression(expression)
        })
      case ExplicitSet(values) =>
        if values == Nil then appendString("{}") else appendExpressions(values, "{", ", ", "}")
      case BoundedSetComprehension(variable, domain, predicate) =>
        appendString("{")
        appendExpression(variable)
        appendString(" \\in ")
        appendExpression(domain)
        appendString(": ")
        appendExpression(predicate)
        appendString("}")
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) =>
        appendString("{")
        appendExpression(mappingExpression)
        appendString(": ")
        appendExpressions(quantifierBounds, ", ")
        appendString("}")
      case ExplicitFunction(mappings) =>
        appendExpressions(mappings, "[", ", ", "]")
      case BoundedMapping(quantifierBounds, expression) =>
        appendString("[")
        appendExpressions(quantifierBounds, ", ")
        appendString(" |-> ")
        appendExpression(expression)
        appendString("]")
      case GeneralIdentifier(_, _) => ???
      case SetOfAllFunctions(_, _) => ???
      case SetOfAllRecords(_) => ???
      case ExceptExpression(exceptTarget, clauses) =>
        // Example outputs:
        // [foo EXCEPT !.foo1.foo2 = "FOO", !.bar1.bar2 = "BAR"]
        // [foo EXCEPT ![foo1][foo2] = "FOO", ![bar1, bar2] = "BAR"]
        appendString("[")
        appendExpression(exceptTarget)
        appendString(" EXCEPT ")
        val clausesIndentation = column - 1
        for ((clause, idx) <- clauses.zipWithIndex) {
          // We write each clause on a new line, all clauses aligned with the ! symbol.
          if (idx != 0) { appendNewline(); appendString(" ".repeat(clausesIndentation)) }
          appendString("!")
          for (selectorPart <- clause.selector.parts) {
            selectorPart match {
              case DotSelector(name) =>
                appendString(".")
                appendExpression(name)
              case IndexSelector(indices) =>
                appendExpressions(indices, "[", ", ", "]")
            }
          }
          appendString(" = ")
          appendExpression(clause.value)
          if (idx != clauses.length - 1) appendString(",")
        }
        appendString("]")
      case WeakStuttering(action, state) =>
        appendString("[")
        appendExpression(action)
        appendString("]_")
        appendExpression(state)
      case StrongStuttering(action, state) =>
        appendString("<<")
        appendExpression(action)
        appendString(">>_")
        appendExpression(state)
      case WeakFairness(state, action) =>
        appendString("WF_")
        appendExpression(state)
        appendString("(")
        appendExpression(action)
        appendString(")")
      case StrongFairness(state, action) =>
        appendString("SF_")
        appendExpression(state)
        appendString("(")
        appendExpression(action)
        appendString(")")
      case AtSign() =>
        appendString("@")
    }
  }

  def appendExpressions(expressions: List[Expression], separator: String): Unit = {
    // For empty lists, we don't do anything
    if (expressions == Nil) return
    // Lists with one or more elements are handled here
    for (expression <- expressions.init) {
      appendExpression(expression)
      appendString(separator)
    }
    appendExpression(expressions.last)
  }

  def appendExpressions(expressions: List[Expression], start: String, separator: String, end: String): Unit = {
    // For empty lists, we don't do anything
    if (expressions == Nil) return
    // Lists with one or more elements are handled here
    appendString(start)
    for (expression <- expressions.init) {
      appendExpression(expression)
      appendString(separator)
    }
    appendExpression(expressions.last)
    appendString(end)
  }
}
