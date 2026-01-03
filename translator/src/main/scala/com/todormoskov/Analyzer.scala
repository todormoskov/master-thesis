package com.todormoskov

import com.todormoskov
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
  locationVariables: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Mapping]],
  locationThreads: mutable.LinkedHashMap[Enumerator, mutable.ListBuffer[Enumerator]],
  locThdEnumsToOperations: mutable.LinkedHashMap[(Enumerator, Enumerator), CFG],
  thdDefinitionsToFreeVariables: mutable.LinkedHashMap[Identifier, List[Identifier]],
  thdDefinitionsToOperations: mutable.LinkedHashMap[Identifier, CFG],
  messageAether: mutable.ListBuffer[MultiSet[TravellingMessage]],
  variablesToLocations: mutable.LinkedHashMap[Identifier, mutable.ListBuffer[Enumerator]],
  specWithMultiSends: Boolean,
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
      val locationVariables = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Mapping]]
      val locationThreads = mutable.LinkedHashMap.empty[Enumerator, mutable.ListBuffer[Enumerator]]
      val locThdEnumsToThread = mutable.LinkedHashMap.empty[(Enumerator, Enumerator), Thread]
      val threadDefFreeVars = mutable.LinkedHashMap.empty[Identifier, List[Identifier]]
      val threadDefBodies = mutable.LinkedHashMap.empty[Identifier, Thread]
      val messageAether = mutable.ListBuffer.empty[MultiSet[TravellingMessage]]
      val variablesToLocations = mutable.LinkedHashMap.empty[Identifier, mutable.ListBuffer[Enumerator]]

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
          for (mapping <- body.memory.mappings) addBinding[Enumerator, Mapping](locationVariables, locEnumerator, mapping)
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
            case Declaration(identifier) => addBinding[Enumerator, Mapping](locationVariables, locEnum, Mapping(identifier, Identifier("Undefined")))
            case Assignment(identifier, expression) =>
            case Receive(channel, location, variable) =>
            case Send(messages) =>
          }
          case ConditionalThread(cond, thenThread, elseThread) =>
          case ThreadInstantiation(identifier) =>
        }
      }

      // Start the recursive visit of all EPC spec units.
      // This gathers information about the defined channels, locations (with their variables and their threads), and the defined threads.
      tlaModule.moduleUnits(epcSpecIndex).asInstanceOf[EPCSpec].specUnits.foreach(visitEPCSpecUnit)

      // We compute which EPC variables appear in which locations. This will be used in the definition of the
      // initial state predicate, where we need to specify the initial state of all TLA variables.
      for {
        (locEnum, variableMappingsBuffer) <- locationVariables
        variableMapping <- variableMappingsBuffer
      } {
        addBinding[Identifier, Enumerator](variablesToLocations, variableMapping.identifier, locEnum)
      }

      // We compute the CFGs for each location thread pair with the same control flow analyzer.
      // This ensures that every action, that appears directly in a location, receives a unique ID.
      val controlFlowAnalyser = ControlFlowAnalyzer()
      val locThdEnumsToOperations = locThdEnumsToThread.map((locThdEnums, thread) => (locThdEnums, controlFlowAnalyser.computeCfg(thread)))
      // We also compute the CFGs for the defined threads. Each thread has its own "scope",
      // so we start generating the IDs from 1 for each thread.
      val threadDefinitionsAsOperations = threadDefBodies.map((identifier, thread) => (identifier, ControlFlowAnalyzer().computeCfg(thread)))

      def anySendOfMultipleMessages(region: Region): Boolean = {
        def isSendOfMultipleMessages(operation: Operation): Boolean = operation match {
          case LeafOperation(_, _, command) => command match {
            case Send(ExplicitMultiSet(values)) if values.length > 1 => true
            case Send(QuantifiedMultiSet(_, _)) => true
            case _ => false
          }
          case IfOperation(_, _, _, thenRegion, elseRegion) => (thenRegion ++ elseRegion).exists(isSendOfMultipleMessages)
        }
        region.exists(isSendOfMultipleMessages)
      }
      var specWithMultiSends = locThdEnumsToOperations.exists((_, thdCfg) => anySendOfMultipleMessages(thdCfg._1))
      specWithMultiSends = specWithMultiSends | threadDefinitionsAsOperations.exists((_, thdCfg) => anySendOfMultipleMessages(thdCfg._1))

      // The result of the the analyze()-method is an EpcSpecContext-object with the gathered information, wrapped in an Either container.
      Right(EPCSpecContext(
        visibleTLAOperators,
        channels,
        locationVariables,
        locationThreads,
        locThdEnumsToOperations,
        threadDefFreeVars,
        threadDefinitionsAsOperations,
        messageAether,
        variablesToLocations,
        specWithMultiSends
      ))
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
        // Thread Instantiations know their successors in their current region
        // (where to "return to" when the instantiated thread completes)
        // and also the name of the instantiated thread as a label in the edge.
        List( DiEdge(id, getSuccessorIDInRegion(op, region ++ successorRegion), Some(threadName.name)) )
      case op@LeafOperation(id, _, _) => List( DiEdge(id, getSuccessorIDInRegion(op, region ++ successorRegion), None) )
    }
  }
}
