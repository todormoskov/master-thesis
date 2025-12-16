package com.todormoskov

import com.todormoskov.Lexer.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Operation {
  def id: Int
  def successors: List[Int]
}
type Region = List[Operation]
case class LeafOperation(id: Int, successors: List[Int], unit: ThreadUnit) extends Operation
case class CompositeOperation(id: Int, successors: List[Int], unit: ThreadUnit, regions: List[Region]) extends Operation

/**
 *  Context collected during analysis pass.
 */
case class EPCSpecContext(
   channels: mutable.LinkedHashSet[Identifier],
   locationVariables: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Identifier]],
   locationThreads: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Enumerator]],
   locThdEnumsToOperations: mutable.LinkedHashMap[(Enumerator, Enumerator), Region],
   threadDefinitions: mutable.LinkedHashMap[Identifier, Thread],
)

object Analyzer {
  def analyze(tlaModule: Module): Either[String, EPCSpecContext] = {
    val epcSpecIndex = tlaModule.moduleUnits.indexWhere(_.isInstanceOf[EPCSpec])
    if epcSpecIndex == -1 then {
      ???
    }
    else {
      val namesBeforeEPCSpec = mutable.TreeSet.empty[String]
      // Go over all TLA+ module units before the EPC spec and collect
      // the names of the defined constants, operators and functions.
      // These can then be used as in the EPC spec.
      tlaModule.moduleUnits.take(epcSpecIndex).foreach(
        (moduleUnit: ModuleUnit) => moduleUnit match {
          case ConstantDeclaration(_, operatorDeclarations) =>
            operatorDeclarations.foreach(_.operator match {
              case identifier: Identifier => namesBeforeEPCSpec addOne identifier.name
              case token: Token => namesBeforeEPCSpec addOne token.value
            })
          case OperatorDefinition(signature, _) => namesBeforeEPCSpec addOne signature.identifier.name
          case FunctionDefinition(identifier, _, _) => namesBeforeEPCSpec addOne identifier.name
          case _ =>
        }
      )

      val channels = mutable.LinkedHashSet.empty[Identifier]
      val locationVariables = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Identifier]]
      val locationThreads = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Enumerator]]
      val locThdEnumsToThread = mutable.LinkedHashMap.empty[(Enumerator, Enumerator), Thread]
      val threadDefinitions = mutable.LinkedHashMap.empty[Identifier, Thread]

      /**
       * Utility method to work with the "MultiMaps", such as locationVariables and locationThreads
       */
      def addBinding[K, V](map: mutable.LinkedHashMap[K, mutable.ListBuffer[V]], key: K, value: V): Unit =
        // We check if we have already initialized the list with values for this key in the map.
        if map contains key
        // If this is the case, we simply add the current value to the list behind the key
        then map(key) addOne value
        // Otherwise, we create a new mapping from the key to its list values.
        // The list is initialized with the given value as its only element.
        else map addOne key -> ListBuffer(value)

      def visitEPCSpecUnit(ast: EPCSpecUnit): Unit = ast match {
        case ChannelDefinition(identifier) => channels += identifier
        case ThreadDefinition(identifier, freeVariables, body) => threadDefinitions addOne identifier -> body
        case Location(enumerator, body) =>
          // Create a binding from the location to all of its variables
          body.memory.mappings.foreach(mapping => addBinding[Enumerator, Identifier](locationVariables, enumerator, mapping.identifier))
          // Visit the threads in the process
          visitOuterThread(enumerator, body.body)
        case MessageAether(messages) => ???
        case NetworkComposition(lhs, rhs) => visitEPCSpecUnit(lhs); visitEPCSpecUnit(rhs)
      }

      def visitOuterThread(locationEnumerator: Enumerator, ast: OuterThread): Unit = ast match {
        case ThreadComprehension(threadEnumerator, threadBody) =>
          // First, we add a binding between the location enumerator and the thread enumerator
          addBinding[Enumerator, Enumerator](locationThreads, locationEnumerator, threadEnumerator)
          // Next, we add a mapping from the pair (location, thread) to the thread body associated with it.
          // This will allow us to connect the program counters to the correct initial actions.
          locThdEnumsToThread addOne (locationEnumerator, threadEnumerator) -> threadBody
          // After that, we visit each thread unit separately.
          threadBody.foreach((threadUnit) => visitThreadUnit(locationEnumerator, threadEnumerator, threadUnit))
        case ThreadComposition(lhs, rhs) => visitOuterThread(locationEnumerator, lhs); visitOuterThread(locationEnumerator, rhs)
      }

      def visitThreadUnit(locEnum: Enumerator, thdEnum: Enumerator, ast: ThreadUnit): Unit = ast match {
        case ThreadUnit(label, command) => command match {
          case action: Action => action match {
            case Declaration(identifier) => addBinding[Enumerator, Identifier](locationVariables, locEnum, identifier)
            case Assignment(identifier, expression) =>
            case Receive(channel, location, variable) =>
            case Send(messages) =>
          }
          case ConditionalThread(cond, thenThread, elseThread) =>
          case ThreadInstantiation(identifier) =>
            // Assuming all available threads were already added to the map with definitions
            val threadBody = threadDefinitions(identifier)
            // We iterate over all thread units, and we visit them from "the current location and the current thread".
            // That way, we make sure that all "possible" variable declarations are also added to the variables list of the location.
            threadBody.foreach(thdUnit => visitThreadUnit(locEnum, thdEnum, thdUnit))
        }
      }

      tlaModule.moduleUnits(epcSpecIndex).asInstanceOf[EPCSpec].specUnits.foreach(visitEPCSpecUnit)

      val locThdEnumsToOperations = locThdEnumsToThread.map((locThdEnums, thread) =>
        (locThdEnums, ControlFlowAnalyzer().computeCfg(thread)))

      Right(EPCSpecContext(channels, locationVariables, locationThreads, locThdEnumsToOperations, threadDefinitions))
    }
  }
}

class ControlFlowAnalyzer {
  /**
   * Main method for the computation of the CFG.
   */
  def computeCfg(thread: Thread): Region = computeSuccessors(toRegionOfIdentifiableOperations(thread), Nil)

  /**
   * Global counter for the current free action ID.
   */
  private var actionID = 1

  /**
   * Method for the access to the next free action ID and its incrementation.
   */
  private def getAndIncrementActionID(): Int = {
    val currentID = actionID
    actionID = actionID + 1
    currentID
  }

  /**
   * The creation of the CFG happens in two steps and this is the first one. In this function, we simply map
   * each thread unit from the input thread to an operation with an ID (which still contains the thread unit).
   * This means that in this step, we recreate the AST nodes of input thread as operations with IDs, with the
   * exact same structure. This is done recursively for the "then" and "else" threads of the conditional threads.
   *
   * This would allow us, to connect the different operations in CFG by referencing their successor ID in a second step.
   * @param thread
   * @return
   */
  private def toRegionOfIdentifiableOperations(thread: Thread): Region =
    thread.flatMap {
      case threadUnit@ThreadUnit(_, ConditionalThread(_, thenThread, elseThread)) =>
        List(CompositeOperation(getAndIncrementActionID(), Nil, threadUnit,
          List(toRegionOfIdentifiableOperations(thenThread), toRegionOfIdentifiableOperations(elseThread))))
      case threadUnit => List(LeafOperation(getAndIncrementActionID(), Nil, threadUnit))
    }

  private def getSuccessorIDInRegion(operation: Operation, region: Region): Int = {
    val opIndex = region.indexWhere(_.id == operation.id)
    val regionAfterOp = region.drop(opIndex + 1)
    getFirstValidSuccessorID(regionAfterOp)
  }

  @tailrec
  private def getFirstValidSuccessorID(region: Region): Int =
    region match {
      case head :: tail =>
        head match {
          case LeafOperation(id, _, threadUnit@ThreadUnit(_, Send(ExplicitMultiSet(Nil)))) => getFirstValidSuccessorID(tail)
          case _ => head.id
        }
      case Nil => -1
    }

  /**
   * Here, we connect each operation with its successor operation in the region
   * (and with this we create something like a control-flow graph).
   *
   * For each operation, which contains a command (declaration, assignment, receive, send),
   * we simply set its successor to point to the ID of the next operation.
   *
   * For each operation, which contains a conditional thread, we set two successors:
   * - the first one corresponds to the first operation of the "then" region
   * - the second one corresponds to the first operation of the "else" region
   * If there is no valid ID in one of these regions (e.g. when there is only the empty operation {{ }}),
   * we set the pointer to the first valid operation ID after the conditional thread.
   *
   * @param region
   * @param continuationID
   * @return
   */
  private def computeSuccessors(region: Region, successorRegion: Region): Region =
    region.map {
      case op@CompositeOperation(id, _, ThreadUnit(_, ConditionalThread(_, _, _)), regions) =>
        // Here we should have exactly 2 regions: one for the "then" operations and one for the "else" operations.
        val thenOperations = regions.head
        val elseOperations = regions.last
        // First, we set the successors of the operation with the conditional thread.
        // The first successor points to the first valid operation of the "then" region.
        // The second successor points to the first valid operation of the "else" region.
        // There is also a special case handling, if there are no valid operations with these regions.
        val opIndex = region.indexWhere(_.id == op.id)
        val regionAfterOp = region.drop(opIndex + 1)
        val firstThenOperationID = getFirstValidSuccessorID(thenOperations ++ regionAfterOp ++ successorRegion)
        val firstElseOperationID = getFirstValidSuccessorID(elseOperations ++ regionAfterOp ++ successorRegion)
        val condSuccessors = List(firstThenOperationID, firstElseOperationID)
        // Secondly, we recursively "connect" the operations in the "then" and "else" regions.
        val thenPath = computeSuccessors(thenOperations, regionAfterOp ++ successorRegion)
        val elsePath = computeSuccessors(elseOperations, regionAfterOp ++ successorRegion)
        val updatedThenOperations = thenPath.take(thenOperations.length)
        val updatedElseOperations = elsePath.take(elseOperations.length)
        op.copy(successors = condSuccessors, regions = List(updatedThenOperations, updatedElseOperations))
      case op@LeafOperation(id, _, ThreadUnit(_, ThreadInstantiation(_))) => ???
      case op@LeafOperation(id, _, _) => op.copy(successors = List(getSuccessorIDInRegion(op, region ++ successorRegion)))
      case _ => ??? // This case is unreachable!
    }
}

object TLAGenerator {
  def generate(epcCtx: EPCSpecContext): List[ModuleUnit] = {
    // Here we start generating the TLA+ specification.
    val generatedModuleUnits = new ListBuffer[ModuleUnit]
    // We add the constants Undefined and Initialized.
    generatedModuleUnits addOne ConstantDeclaration(Token(TokenKind.CONSTANTS, "CONSTANTS"),
      List(OperatorDeclaration(Identifier("Undefined"), Nil), OperatorDeclaration(Identifier("Initialized"), Nil)))

    // We generate the variable declarations. This includes the declaration of all variables
    // which appear in the memory of a location or are declared with the 'var' keyword.
    // Additionally, we create a variable for the program counters (pc) of the different locations
    // and their threads. Finally, we create variables for each channel, with the same name as the channel.
    val declaredVariables = epcCtx.locationVariables.flatMap(_._2).toList
    val pc = List(Identifier("pc"))
    val declaredChannels = epcCtx.channels.toList
    val generatedVariables = declaredVariables ++ pc ++ declaredChannels
    generatedModuleUnits addOne VariableDeclaration(Token(TokenKind.VARIABLES, "VARIABLES"), generatedVariables)

    // We also add the operator vars == << ... >>, which is a list with all variables.
    generatedModuleUnits addOne OperatorDefinition(NonFixLHS(Identifier("vars"), Nil), Tuple(generatedVariables))

    def toUnionOfSets(enumerators: List[Enumerator]) = enumerators.map {
      case BoundedEnumerator(name, "=", domain) => ExplicitSet(List(domain)) // here we create a set with one element
      case BoundedEnumerator(name, "\\in", domain) => domain // the domain in this case is already a set
      case _ => ???
    } match {
      case Nil => ???
      case x :: Nil => x
      case x :: xs => xs.fold(x)((expr1, expr2) => BinaryExpression("\\union", expr1, expr2))
    }

    // Next, we generate the operator ProcSet, which is a set of all process IDs.
    // (e.g. ProcSet == {"P"} \union {"Q"})
    val procSetExpression = toUnionOfSets(epcCtx.locationThreads.keys.toList)
    val procSetDefinition = OperatorDefinition(NonFixLHS(Identifier("ProcSet"), Nil), procSetExpression)
    generatedModuleUnits addOne procSetDefinition

    // Now, we proceed with the generated of the function ThreadSet, which maps each process ID to the set of its threads.
    // E.g. "P" |-> {1} \union {2}; "Q" |-> 1..N
    val locIDtoThreadSetExpression = epcCtx.locationThreads
      .filter((locEnum, _) => locEnum.isInstanceOf[BoundedEnumerator])
      .map((locEnum, threadEnums) => (locEnum.asInstanceOf[BoundedEnumerator].name, toUnionOfSets(threadEnums.toList)))
    // These (identifier, expression) mappings will be rewritten as TLA+ case arms expression.
    // The resulting TLA+ code will look like this:
    // ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1} \union {2}
    //                                    []   self = "Q" -> 1..N ]
    val threadSetDefinition = OperatorDefinition(NonFixLHS(Identifier("ThreadSet"), Nil),
      CaseExpression(locIDtoThreadSetExpression.map((identifier, expression) => CaseArm(identifier, expression)).toList, None))
    generatedModuleUnits addOne threadSetDefinition

    def generateDeclarationEffect(identifier: Identifier): List[Expression] = {
      // We require that the variable in the var action is Undefined at the location.
      // E.g. x[self] = Undefined
      val variableIsUndefined = BinaryExpression(
        "=",
        FunctionApplication(Identifier(identifier.name), List(Identifier("self"))),
        Identifier("Undefined")
      )
      // The effect of the action is that the variable is now Initialized at the location.
      // E.g. x' = [x EXCEPT ![self] = Initialized]
      val variableIsInitializedInNextState = BinaryExpression(
        "=",
        PrimedExpression(Identifier(identifier.name)),
        ExceptExpression(
          Identifier(identifier.name),
          List(
            ExceptClause(
              SelectorPath(List(IndexSelector(List(Identifier("self"))))),
              Identifier("Initialized")
            )
          )
        )
      )
      List(variableIsUndefined, variableIsInitializedInNextState)
    }

    def generateAssignmentEffect(identifier: Identifier, expression: Expression): List[Expression] = {
      // We require that the variable in the assign action is not Undefined at the location.
      // E.g. x[self] # Undefined
      val variableIsNotUndefined = BinaryExpression(
        "#",
        FunctionApplication(Identifier(identifier.name), List(Identifier("self"))),
        Identifier("Undefined")
      )
      // The effect of the action is that the variable at the location is now set to the rhs of the assign action.
      // E.g. x' = [x EXCEPT ![self] = expression...]
      val variableIsSetToExpressionInNextState = BinaryExpression(
        "=",
        PrimedExpression(Identifier(identifier.name)),
        ExceptExpression(
          Identifier(identifier.name),
          List(
            ExceptClause(
              SelectorPath(List(IndexSelector(List(Identifier("self"))))),
              expression // TODO, we need to rewrite all variable read operations like this x -> x[self]
            )
          )
        )
      )
      List(variableIsNotUndefined, variableIsSetToExpressionInNextState)
    }

    def generatePCLabel(locEnum: Enumerator, thdEnum: Enumerator, actionIdx: Int, actionSuffix: String) =
      s"${locEnum.asInstanceOf[BoundedEnumerator].name}_${thdEnum.asInstanceOf[BoundedEnumerator].name}_${actionIdx}${actionSuffix}"

    def generatePCEnablingCondition(locEnum: Enumerator, thdEnum: Enumerator, label: String): Expression =
      // Generate the enabling condition for the program counter
      // E.g. pc[self][thd] = "..."
      BinaryExpression(
        "=",
        FunctionApplication(
          FunctionApplication(
            Identifier("pc"),
            List(Identifier("self"))
          ),
          List(Identifier("thd"))
        ),
        Literal(label)
      )

    generatedModuleUnits.toList
  }
}
