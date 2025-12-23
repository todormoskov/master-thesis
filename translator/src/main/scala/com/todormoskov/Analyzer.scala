package com.todormoskov

import com.todormoskov.Lexer.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Operation {
  def id: Int
  def label: Option[Identifier]
}
type Region = List[Operation]
case class LeafOperation(id: Int, label: Option[Identifier], command: Command) extends Operation
case class IfOperation(id: Int, label: Option[Identifier], condition: Expression, thenRegion: Region, elseRegion: Region) extends Operation
case class DiEdge(start: Int, end: Int, label: Option[String])
type CFG = (List[Operation], List[DiEdge])

/**
 *  Context collected during the analysis pass.
 */
case class EPCSpecContext(
  visibleTLAOperators: mutable.TreeSet[String],
  channels: mutable.LinkedHashSet[Identifier],
  locationVariables: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Identifier]],
  locationThreads: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Enumerator]],
  locThdEnumsToOperations: mutable.LinkedHashMap[(Enumerator, Enumerator), CFG],
  thdDefinitionsToFreeVariables: mutable.LinkedHashMap[Identifier, List[Identifier]],
  thdDefinitionsToOperations: mutable.LinkedHashMap[Identifier, CFG],
  locThdEnumsToOutgoingBagMappings: mutable.LinkedHashMap[(Enumerator, Enumerator), mutable.LinkedHashMap[Int, MultiSet[OutgoingMessage]]],
  thdDefinitionsToOutgoingBagMappings: mutable.LinkedHashMap[Identifier, mutable.LinkedHashMap[Int, MultiSet[OutgoingMessage]]]
)

object Analyzer {
  def analyze(tlaModule: Module): Either[String, EPCSpecContext] = {
    val epcSpecIndex = tlaModule.moduleUnits.indexWhere(_.isInstanceOf[EPCSpec])
    if epcSpecIndex == -1 then {
      ???
    }
    else {
      val visibleTLAOperators = mutable.TreeSet.empty[String]
      // Firstly, we go over all TLA+ module units before the EPC spec and collect the names of the defined constants, operators and functions.
      // These can then be used as in the EPC spec.
      for (moduleUnit <- tlaModule.moduleUnits.take(epcSpecIndex)) {
        moduleUnit match {
          case ConstantDeclaration(_, operatorDeclarations) =>
            operatorDeclarations.foreach(_.operator match {
              case identifier: Identifier => visibleTLAOperators addOne identifier.name
              case token: Token => visibleTLAOperators addOne token.value
            })
          case OperatorDefinition(signature, _) => visibleTLAOperators addOne signature.identifier.name
          case FunctionDefinition(identifier, _, _) => visibleTLAOperators addOne identifier.name
          case _ => // do nothing for all other cases
        }
      }

      val channels = mutable.LinkedHashSet.empty[Identifier]
      val locationVariables = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Identifier]]
      val locationThreads = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Enumerator]]
      val locThdEnumsToThread = mutable.LinkedHashMap.empty[(Enumerator, Enumerator), Thread]
      val threadDefFreeVars = mutable.LinkedHashMap.empty[Identifier, List[Identifier]]
      val threadDefBodies = mutable.LinkedHashMap.empty[Identifier, Thread]
      val messageAether = mutable.ListBuffer.empty[MultiSet[TravellingMessage]]

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
        case ChannelDefinition(identifier) =>
          // Add the channel to the channels set
          channels += identifier
        case ThreadDefinition(identifier, freeVariables, body) =>
          // Associate the thread identifier with its free variables for easier access later
          threadDefFreeVars addOne identifier -> freeVariables
          // Associate the thread identifier with its body for easier access later
          threadDefBodies addOne identifier -> body
        case Location(locEnumerator, body) =>
          // Create a binding from the location to all of its variables
          for (mapping <- body.memory.mappings) addBinding[Enumerator, Identifier](locationVariables, locEnumerator, mapping.identifier)
          // Visit the threads in the process
          visitOuterThread(locEnumerator, body.body)
        case MessageAether(messages) =>
          // Add all travelling messages to the message aether
          messageAether addOne messages
        case NetworkComposition(lhs, rhs) =>
          // Visit both networks recursively
          visitEPCSpecUnit(lhs); visitEPCSpecUnit(rhs)
      }

      def visitOuterThread(locationEnumerator: Enumerator, ast: OuterThread): Unit = ast match {
        case ThreadComprehension(threadEnumerator, threadBody) =>
          // First, we add a binding between the location enumerator and the thread enumerator
          addBinding[Enumerator, Enumerator](locationThreads, locationEnumerator, threadEnumerator)
          // Next, we add a mapping from the pair (location, thread) to the thread body associated with it.
          // This will allow us to connect the program counters to the correct initial actions.
          locThdEnumsToThread addOne (locationEnumerator, threadEnumerator) -> threadBody
          // After that, we visit each thread unit separately.
          for (threadUnit <- threadBody) visitThreadUnit(locationEnumerator, threadEnumerator, threadUnit)
        case ThreadComposition(lhs, rhs) =>
          // In case of thread compositions, visit the lhs and rhs recursively
          visitOuterThread(locationEnumerator, lhs); visitOuterThread(locationEnumerator, rhs)
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
//            // Assuming all available threads were already added to the map with definitions
//            val threadBody = threadDefBodies(identifier)
//            // We iterate over all thread units, and we visit them from "the current location and the current thread".
//            // That way, we make sure that all "possible" variable declarations are also added to the variables list of the location.
//            threadBody.foreach(thdUnit => visitThreadUnit(locEnum, thdEnum, thdUnit))
        }
      }

      // Start the recursive visit of all EPC spec units.
      // This gathers information about the defined channels, locations (with their variables and their threads), and the defined threads.
      tlaModule.moduleUnits(epcSpecIndex).asInstanceOf[EPCSpec].specUnits.foreach(visitEPCSpecUnit)

      // We compute the CFGs for each location thread pair with the same control flow analyzer.
      // This ensures that every action, that appears directly in a location, receives a unique ID.
      val controlFlowAnalyser = ControlFlowAnalyzer()
      val locThdEnumsToOperations = locThdEnumsToThread.map((locThdEnums, thread) => (locThdEnums, controlFlowAnalyser.computeCfg(thread)))
      // We also compute the CFGs for the defined threads. Each thread has its own "scope",
      // so we start generating the IDs from 1 for each thread.
      val threadDefinitionsAsOperations = threadDefBodies.map((identifier, thread) => (identifier, ControlFlowAnalyzer().computeCfg(thread)))

      /**
       * Method for the creation of the O variable (which contains the outgoing messages for each (location, thread) pair).
       * The method takes a region of identifiable operations and constructs a map, which associates the IDs of the send
       * operations (that send more than one message) with their outgoing bags.
       *
       * E.g. If we encounter {{ c@1<< "Hello" >>, c@1<< "Hello" >> }} with ID = 1, we create the mapping 1 -> ExplicitMultiSet(OutgoingMessage(...), ...)
       *      If we encounter {{ (j \in 1..N) cL@j<< ... >> }} with ID = 2, we create the mapping 2 -> QunatifiedMultiSet(...)
       *
       * This will later be used for the construction of the initial state of the O variable and the send actions.
       * @param region
       * @return
       */
      def createOutgoingBagMappings(region: Region): mutable.LinkedHashMap[Int, MultiSet[OutgoingMessage]] = {
        val outgoingMessages = mutable.LinkedHashMap.empty[Int, MultiSet[OutgoingMessage]]
        // Method for the recursive iteration of the operations tree.
        def visitOperation(operation: Operation): Unit = operation match {
          case LeafOperation(id, label, command) => command match {
            case send@Send(ExplicitMultiSet(values)) if values.length > 1 => outgoingMessages addOne (id -> send.messages)
            case send@Send(QuantifiedMultiSet(_, _)) => outgoingMessages addOne (id -> send.messages)
            case _ => // do nothing
          }
          case IfOperation(id, label, condition, thenRegion, elseRegion) =>
            for (operation <- thenRegion) visitOperation(operation)
            for (operation <- elseRegion) visitOperation(operation)
        }
        // Start the recursive iteration of the operations.
        for (operation <- region) visitOperation(operation)
        // The result of the method is the constructed map.
        outgoingMessages
      }

      // Call the method for the creation of the outgoing bags for each (location, thread) pair and each thread definition.
      val locThdEnumsToOutgoingBagMappings = locThdEnumsToOperations.map((locThdEnums, thdCfg) => (locThdEnums, createOutgoingBagMappings(thdCfg._1)))
      val thdDefToOutgoingBagMappings = threadDefinitionsAsOperations.map((identifier, thdCfg) => (identifier, createOutgoingBagMappings(thdCfg._1)))


      // The result of the the analyze()-method is an EpcSpecContext-object with the gathered information, wrapped in an Either container.
      val epcCtx = EPCSpecContext(
        visibleTLAOperators,
        channels,
        locationVariables,
        locationThreads,
        locThdEnumsToOperations,
        threadDefFreeVars,
        threadDefinitionsAsOperations,
        locThdEnumsToOutgoingBagMappings,
        thdDefToOutgoingBagMappings
      )

      TLAGenerator.generate(epcCtx)

      Right(epcCtx)
    }
  }
}

class ControlFlowAnalyzer {
  /**
   * Main method for the computation of the CFG.
   */
  def computeCfg(thread: Thread): CFG = {
    val operations = toOperations(thread)
    val entryEdge = List( DiEdge(0, getFirstValidSuccessorID(operations), None) )
    val edges = computeEdges(operations, Nil)
    (operations, entryEdge ++ edges)
  }

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
  private def toOperations(thread: Thread): Region =
    thread.map {
      case threadUnit@ThreadUnit(label, ConditionalThread(condition, thenThread, elseThread)) =>
        IfOperation(
          getAndIncrementActionID(),
          label,
          condition,
          toOperations(thenThread),
          toOperations(elseThread)
        )
      case threadUnit@ThreadUnit(label, command) => LeafOperation(getAndIncrementActionID(), label, command)
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
          case LeafOperation(id, _, Send(ExplicitMultiSet(Nil))) => getFirstValidSuccessorID(tail)
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
  private def computeEdges(region: Region, successorRegion: Region): List[DiEdge] = {
    region.flatMap {
      case op@IfOperation(id, label, condition, thenOperations, elseOperations) =>
        // First, we compute the successors of the "evaluation"-step of the condition expression.
        // The first successor points to the first valid operation of the "then" region.
        // The second successor points to the first valid operation of the "else" region.
        // There is also a special case handling, if there are no valid operations with these regions.
        val opIndex = region.indexWhere(_.id == id)
        val regionAfterOp = region.drop(opIndex + 1)
        val firstThenOperationID = getFirstValidSuccessorID(thenOperations ++ regionAfterOp ++ successorRegion)
        val firstElseOperationID = getFirstValidSuccessorID(elseOperations ++ regionAfterOp ++ successorRegion)
        List( DiEdge(id, firstThenOperationID, Some("then")), DiEdge(id, firstElseOperationID, Some("else")) )
          // Secondly, we recursively compute the edges in the "then" and "else" regions.
          ++ computeEdges(thenOperations, regionAfterOp ++ successorRegion)
          ++ computeEdges(elseOperations, regionAfterOp ++ successorRegion)
      case op@LeafOperation(id, _, ThreadInstantiation(threadName)) =>
        // Thread Instantiations has -1 as the end node and then the thread name as a label.
        List( DiEdge(id, -1, Some(threadName.name)) )
      case op@LeafOperation(id, _, _) => List( DiEdge(id, getSuccessorIDInRegion(op, region ++ successorRegion), None) )
    }
  }
}

class TLAExpressionRewriter(epcCtx: EPCSpecContext) {

  private def rewriteVariable(identifier: Identifier, epcVariables: List[Identifier]): Expression =
    if epcVariables contains identifier then FunctionApplication(identifier, List(Identifier("self"))) else identifier

  def rewrite(expression: Expression, epcVariables: List[Identifier]): Expression = {
    expression match {
      case literal: Literal => literal
      case identifier: Identifier => rewriteVariable(identifier, epcVariables)
      case IdentifierTuple(identifiers) => Tuple(identifiers.map(rewrite(_, epcVariables)))
      case qb@QuantifierBound(variable, domain) => qb.copy(domain = rewrite(domain, epcVariables))
      case m@Mapping(identifier, expression) => m.copy(expression = rewrite(expression, epcVariables))
      case Tuple(expressions) => Tuple(expressions.map(rewrite(_, epcVariables)))
      case expr@UnaryExpression(_, expression) => expr.copy(expression = rewrite(expression, epcVariables))
      case expr@BinaryExpression(_, lhs, rhs) => expr.copy(lhs = rewrite(lhs, epcVariables), rhs = rewrite(rhs, epcVariables))
      case application@IdentifierApplication(_, operands) => application.copy(operands = operands.map(rewrite(_, epcVariables)))
      case application@GeneralIdentifierApplication(_, operands) => application.copy(operands = operands.map(rewrite(_, epcVariables)))
      case FunctionApplication(expression, operands) => FunctionApplication(rewrite(expression, epcVariables), operands.map(rewrite(_, epcVariables)))
      case PrimedExpression(expression) => PrimedExpression(rewrite(expression, epcVariables))
      case ParenthesizedExpression(expression) => ParenthesizedExpression(rewrite(expression, epcVariables))
      case IfThenElse(cond, thenExpr, elseExpr) =>
        IfThenElse(
          rewrite(cond, epcVariables),
          rewrite(thenExpr, epcVariables),
          rewrite(elseExpr, epcVariables)
        )
      case CaseExpression(caseArms, defaultExpression) =>
        CaseExpression(
          caseArms.map(caseArm => CaseArm(rewrite(caseArm.lhs, epcVariables), rewrite(caseArm.rhs, epcVariables))),
          defaultExpression.map(rewrite(_, epcVariables))
        )
      case ChooseExpression(identifier, domain, predicate) =>
        ChooseExpression(identifier, rewrite(domain, epcVariables), rewrite(predicate, epcVariables))
      case expr@BoundedQuantifiedExpression(quantifiers, binders, body) =>
        expr.copy(
          binders = binders.map((quantifierBound: QuantifierBound) => QuantifierBound(quantifierBound.variable, rewrite(quantifierBound.domain, epcVariables))),
          body = rewrite(body, epcVariables)
        )
      case expr@QuantifiedExpression(quantifier, binders, body) => expr.copy(body = rewrite(body, epcVariables))
      case ConjunctionList(listItems) => ConjunctionList(listItems.map(rewrite(_, epcVariables)))
      case DisjunctionList(listItems) => DisjunctionList(listItems.map(rewrite(_, epcVariables)))
      case ExplicitSet(values) => ExplicitSet(values.map(rewrite(_, epcVariables)))
      case bsc@BoundedSetComprehension(variable, domain, predicate) =>
        bsc.copy(domain = rewrite(domain, epcVariables), predicate = rewrite(predicate, epcVariables))
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) =>
        QuantifiedSetComprehension(
          rewrite(mappingExpression, epcVariables),
          quantifierBounds.map((qb: QuantifierBound) => QuantifierBound(qb.variable, rewrite(qb.domain, epcVariables)))
        )
      case ExplicitFunction(mappings) => ExplicitFunction(mappings.map(mapping => mapping.copy(expression = rewrite(mapping.expression, epcVariables))))
      case BoundedMapping(quantifierBounds, expression) =>
        BoundedMapping(
          quantifierBounds.map((qb: QuantifierBound) => QuantifierBound(qb.variable, rewrite(qb.domain, epcVariables))),
          rewrite(expression, epcVariables)
        )
    }
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

    // Next, we generate the operator ProcSet, which is a set of all process IDs.
    // E.g. ProcSet == {"P"} \union {"Q"}
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

    def flattenOperation(operation: Operation): Region = operation match {
      case LeafOperation(id, label, command) => List(operation)
      case IfOperation(id, label, condition, thenRegion, elseRegion) =>
        List(operation) ++ thenRegion.flatMap(flattenOperation) ++ elseRegion.flatMap(flattenOperation)
    }

//    def toTLAActions(operation: Operation): List[OperatorDefinition] =

    for { ((locEnum, thdEnum), (region, edges)) <- epcCtx.locThdEnumsToOperations } yield {
      val flattenedRegion = region.flatMap(flattenOperation)
      for (operation <- flattenedRegion) {

        def generateTLAAction(operation: Operation,
                              getSuccessorOperation: () => Option[Operation],
                              getActionEffect: () => List[Expression],
                              getUnchangedVars: () => List[Identifier]): OperatorDefinition =
        {
          // Generate the labels for the current operation and the successor operation
          val currentLabel: String = getOperationLabel(locEnum, thdEnum, operation)
          val successorLabel: String = getSuccessorOperation() match {
            case Some(successorOperation) => getOperationLabel(locEnum, thdEnum, successorOperation)
            case None => "Done"
          }

          // Generate the pc enabling condition, the operation effect, the pc update and the unchanged variables conjuncts
          val pcEnablingCondition = generatePCEnablingCondition(locEnum, thdEnum, currentLabel)
          val actionEffect = getActionEffect()
          val pcUpdate = generatePCUpdate(successorLabel)
          val unchangedVars = generateUnchangedExpression(getUnchangedVars())

          val actionConjuncts = List(pcEnablingCondition) ++ actionEffect ++ List(pcUpdate, unchangedVars)
          // Create the new definition
          // E.g. p_j_1D(self, thd) == /\ pc[self][thd] = "p_j_1D"
          //                           /\ ...
          OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(Identifier("self"), Identifier("thd"))), ConjunctionList(actionConjuncts))
        }

        def getSuccessor(operation: Operation, containingRegion: Region): Option[Operation] =
          edges.find(_.start == operation.id).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

        def getSuccessorByEdgeLabel(operation: Operation, containingRegion: Region, edgeLabel: String): Option[Operation] =
          edges.find(edge => edge.start == operation.id && edge.label.contains(edgeLabel)).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

        def getEdgeEndOperation(edge: DiEdge, containingRegion: Region): Option[Operation] =
          edge match {
            case DiEdge(start, -1, Some(threadIdentifier)) =>
              // This is the case when we have a thread instantiation and the program counter
              // should move from the current action to the first action of the instantiated thread.
              // So to find the successor operation, we access the control flow graph of the defined thread,
              // and we recursively call the function on the first one edge from its edge list (which is exactly the entry edge).
              // Important: The operations of the thread definition are not flattened, but this is not a problem,
              // because there is always an entry edge which contains the correct entry operation!
              val threadCfg = epcCtx.thdDefinitionsToOperations(Identifier(threadIdentifier))
              val threadOperations = threadCfg._1
              val threadEdges = threadCfg._2
              threadEdges.headOption.flatMap(entryEdge => getEdgeEndOperation(entryEdge, threadOperations))
            case DiEdge(start, -1, None) => None // there is no successor operation in this case => (label "Done")
            case DiEdge(start, end, _) =>
              // We simply look for the successor operation in the flattened region.
              containingRegion.find(_.id == end)
          }

        val operationAsTLAActions: List[OperatorDefinition] = operation match {
          case LeafOperation(id, label, command) =>
            command match {
              case Declaration(identifier) =>
                List(
                  generateTLAAction(
                    operation = operation,
                    getSuccessorOperation = () => getSuccessor(operation, flattenedRegion),
                    getActionEffect = () => generateDeclarationEffect(identifier),
                    getUnchangedVars = () => declaredVariables.filter(_ != identifier)
                  )
                )
              case Assignment(identifier, expression) =>
                List(
                  generateTLAAction(
                    operation = operation,
                    getSuccessorOperation = () => getSuccessor(operation, flattenedRegion),
                    getActionEffect = () => generateAssignmentEffect(identifier, expression),
                    getUnchangedVars = () => declaredVariables.filter(_ != identifier)
                  )
                )
              case Receive(channel, location, variable) =>
                List(
                  generateTLAAction(
                    operation = operation,
                    getSuccessorOperation = () => getSuccessor(operation, flattenedRegion),
                    getActionEffect = () => generateReceiveEffect(channel, location, variable),
                    getUnchangedVars = () => declaredVariables.filter(_ != variable)
                  )
                )
              case Send(_) => null
              case ThreadInstantiation(identifier) =>
                List(
                  generateTLAAction(
                    operation = operation,
                    getSuccessorOperation = () => getSuccessor(operation, flattenedRegion),
                    getActionEffect = () => generateThreadInstantiationPrecondition(epcCtx.thdDefinitionsToFreeVariables(identifier)),
                    getUnchangedVars = () => declaredVariables
                  )
                )

              case ConditionalThread(_, _, _) => null // this should not happen, since conditional threads are turned into IfOperations!
            }

          case IfOperation(id, label, condition, thenRegion, elseRegion) =>
            List(
              generateTLAAction(
                operation = operation,
                getSuccessorOperation = () => getSuccessorByEdgeLabel(operation, flattenedRegion, "then"),
                getActionEffect = () => generateTruePrecondition(condition),
                getUnchangedVars = () => declaredVariables
              ),
              generateTLAAction(
                operation = operation,
                getSuccessorOperation = () => getSuccessorByEdgeLabel(operation, flattenedRegion, "else"),
                getActionEffect = () => generateFalsePrecondition(condition),
                getUnchangedVars = () => declaredVariables
              )
            )

        }
      }
    }

    generatedModuleUnits.toList
  }

  private def toUnionOfSets(enumerators: List[Enumerator]) = enumerators.map {
    case BoundedEnumerator(name, "=", domain) => ExplicitSet(List(domain)) // here we create a set with one element
    case BoundedEnumerator(name, "\\in", domain) => domain // the domain in this case is already a set
    case _ => ???
  } match {
    case Nil => ???
    case x :: Nil => x
    case x :: xs => xs.fold(x)((expr1, expr2) => BinaryExpression("\\union", expr1, expr2))
  }

  private def getOperationLabel(locEnum: Enumerator, thdEnum: Enumerator, operation: Operation): String =
    operation.label match {
      case Some(value) => value.name
      // If there is no label, we create one using the location identifier, thread identifier, operation id and suffix.
      // E.g. p_j_1A, p_j_2R, p_j_3S, ...
      case None => "%s_%s_%d%s" format(
        locEnum.asInstanceOf[BoundedEnumerator].name.name,
        thdEnum.asInstanceOf[BoundedEnumerator].name.name,
        operation.id,
        getOperationSuffix(operation)
      )
    }

  private def getOperationLabel(thdIdentifier: Identifier, operation: Operation): String =
    operation.label match {
      case Some(value) => value.name
      case None => "%s_%d%s" format (thdIdentifier.name, operation.id, getOperationSuffix(operation))
    }

  private def getOperationSuffix(operation: Operation): String =
    operation match {
      case LeafOperation(id, label, command) =>
        command match {
          case Declaration(_) => "D"
          case Assignment(_, _) => "A"
          case Receive(_, _, _) => "R"
          case Send(_) => "S"
          case ThreadInstantiation(_) => "I"
          case ConditionalThread(_, _, _) => "C" // this should not happen, since conditional threads are turned into IfOperations!
        }
      case IfOperation(id, label, condition, thenRegion, elseRegion) => "C" // "C" for conditional
    }

  private def generatePCEnablingCondition(locEnum: Enumerator, thdEnum: Enumerator, label: String): Expression =
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

  private def generatePCUpdate(successorLabel: String): Expression = {
    // Generate the update for the program counter.
    // E.g. pc' = [pc EXCEPT ![self][thd] = "<successorLabel>" ]
    BinaryExpression(
      "=",
      PrimedExpression(Identifier("pc")),
      ExceptExpression(
        Identifier("pc"),
        List(
          ExceptClause(
            SelectorPath(
              List(
                IndexSelector(List(Identifier("self"))),
                IndexSelector(List(Identifier("thd")))
              )
            ),
            Literal(successorLabel)
          )
        )
      )
    )
  }

  private def generateUnchangedExpression(unchangedVariables: List[Identifier]): Expression = {
    // Generate the unchanged variables conjunct.
    // E.g. UNCHANGED << x, y, ... >>
    UnaryExpression("UNCHANGED", IdentifierTuple(unchangedVariables))
  }

  private def generateDeclarationEffect(identifier: Identifier): List[Expression] = {
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

  private def generateAssignmentEffect(identifier: Identifier, expression: Expression): List[Expression] = {
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

  private def generateReceiveEffect(channel: Expression, location: Expression, variable: Identifier): List[Expression] = {
    // We require that the variable in the "receive" action is not Undefined at the location.
    // E.g. x[self] # Undefined
    val variableIsNotUndefined = BinaryExpression(
      "#",
      FunctionApplication(variable, List(Identifier("self"))),
      Identifier("Undefined")
    )
    // Another precondition is that there is a message in the given channel
    // \E msg \in DOMAIN (channel)
    val messageHandlingEffect = BoundedQuantifiedExpression(
      "\\E",
      List(
        QuantifierBound(
          Identifier("msg"),
          UnaryExpression(
            "DOMAIN",
            ParenthesizedExpression(
              channel
            )
          )
        ),
      ),
      ConjunctionList(
        List(
          // msg[1] = ... (location-expression)
          BinaryExpression(
            "=",
            FunctionApplication(Identifier("msg"), List(Literal(1))),
            location
          ),
          // msg[2] = self
          BinaryExpression(
            "=",
            FunctionApplication(Identifier("msg"), List(Literal(2))),
            Identifier("self")
          ),
          // The first effect of the action is that the variable at the location is now set to the payload of the message.
          // E.g. x' = [x EXCEPT ![self] = msg[3] ]
          BinaryExpression(
            "=",
            PrimedExpression(variable),
            ExceptExpression(
              variable,
              List(
                ExceptClause(
                  SelectorPath(List(IndexSelector(List(Identifier("self"))))),
                  FunctionApplication(Identifier("msg"), List(Literal(3)))
                )
              )
            )
          ),
          // The second effect of the action is that the message is removed from the channel.
          // E.g. c' = c (-) SetToBag( {msg} )
          BinaryExpression(
            "=",
            PrimedExpression(channel), // TODO Check if this is correct when the channel is not given by an identifier, but for example as a function f[...]
            BinaryExpression(
              "(-)",
              channel,
              IdentifierApplication(
                Identifier("SetToBag"),
                List(
                  ExplicitSet(
                    List(
                      Identifier("msg")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    List(variableIsNotUndefined, messageHandlingEffect)
  }

  private def generateTruePrecondition(condition: Expression): List[Expression] = {
    List(condition)
  }

  private def generateFalsePrecondition(condition: Expression): List[Expression] = {
    List(
      UnaryExpression(
        "~",
        ParenthesizedExpression(
          condition
        )
      )
    )
  }

  private def generateThreadInstantiationPrecondition(freeVariables: List[Identifier]): List[Expression] = {
    // We require that the free variables from the thread definition are not Undefined at the location.
    // E.g. x[self] # Undefined, y[self] # Undefined
    for identifier <- freeVariables yield {
      BinaryExpression(
        "#",
        FunctionApplication(identifier, List(Identifier("self"))),
        Identifier("Undefined")
      )
    }
  }


}
