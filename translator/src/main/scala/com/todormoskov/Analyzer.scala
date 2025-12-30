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
      val epcCtx = EPCSpecContext(
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

object TLAGenerator {
  def generate(epcCtx: EPCSpecContext): List[ModuleUnit] = {
    val tlaBuilder = TLAStringBuilder()
    // We add the constants Undefined and Initialized.
    val constants = ConstantDeclaration(Token(TokenKind.CONSTANTS, "CONSTANTS"),
      List(OperatorDeclaration(Identifier("Undefined"), Nil), OperatorDeclaration(Identifier("Initialized"), Nil)))
    tlaBuilder.appendUnit(constants, newlines = 2)

    // We generate the variable declarations. This includes the declaration of all variables
    // which appear in the memory of a location or are declared with the 'var' keyword.
    // Additionally, we create a variable for the program counters (pc) of the different locations
    // and their threads. Finally, we create variables for each channel, with the same name as the channel.
    val locationVariables = epcCtx.variablesToLocations.keys.toList
    val pc = Identifier("pc")
    val declaredChannels = epcCtx.channels.toList
    val allTLAVariables = locationVariables ++ List(pc) ++ declaredChannels
    val variablesDeclaration = VariableDeclaration(Token(TokenKind.VARIABLES, "VARIABLES"), allTLAVariables)
    tlaBuilder.appendUnit(variablesDeclaration, newlines = 2)

    // We also add the operator vars == << ... >>, which is a list with all variables.
    val varsOperator = OperatorDefinition(NonFixLHS(Identifier("vars"), Nil), Tuple(allTLAVariables))
    tlaBuilder.appendUnit(varsOperator, newlines = 2)

    // Next, we generate the operator ProcSet, which is a set of all process IDs.
    // E.g. ProcSet == {"P"} \union {"Q"}
    val procSetExpression = toUnionOfSets(epcCtx.locationThreads.keys.toList)
    val procSetDefinition = OperatorDefinition(NonFixLHS(Identifier("ProcSet"), Nil), procSetExpression)
    tlaBuilder.appendUnit(procSetDefinition, newlines = 2)

    // Now, we proceed with the generated of the function ThreadSet, which maps each process ID to the set of its threads.
    // E.g. "P" |-> {1} \union {2}; "Q" |-> 1..N
    val locEnumToThreadSetExpression = epcCtx.locationThreads
      .filter((locEnum, _) => locEnum.isInstanceOf[BoundedEnumerator])
      .map((locEnum, threadEnums) => (locEnum.asInstanceOf[BoundedEnumerator], toUnionOfSets(threadEnums.toList)))
    // These (identifier, expression) mappings will be rewritten as TLA+ case arms expression.
    // The resulting TLA+ code will look like this:
    // ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1} \union {2}
    //                                    []   self = "Q" -> 1..N ]
    val threadSetDefinition = OperatorDefinition(NonFixLHS(Identifier("ThreadSet"), Nil),
      BoundedMapping(
        List(QuantifierBound(Identifier("self"), Identifier("ProcSet"))),
        CaseExpression(
          locEnumToThreadSetExpression.map(
            (locEnum, expression) =>
              CaseArm(
                BinaryExpression(
                  locEnum.equalOrIn,
                  Identifier("self"),
                  locEnum.domain
                ),
                expression
              )
          ).toList,
          None
        )
      )
    )
    tlaBuilder.appendUnit(threadSetDefinition, newlines = 2)

    OperationToTLAAction(epcCtx, tlaBuilder).generateInitPredicate()
    OperationToTLAAction(epcCtx, tlaBuilder).translate(locationVariables, allTLAVariables)

    tlaBuilder.getModuleUnits()
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

}

class OperationToTLAAction(epcCtx: EPCSpecContext, tlaBuilder: TLAStringBuilder) {
  /**
   * Method for the generation of the initial predicate.
   */
  def generateInitPredicate(): Unit = {
    // First, we generate the initial state of the pc variable.
    // The pc variable contains functions (or TLA+ array) with type
    // [self \in ProcSet -> [ThreadSet[self] -> {"label1", "label2", ... } ]]
    val pcInitialValue = BoundedMapping(
      List(QuantifierBound(Identifier("self"), Identifier("ProcSet"))),
      CaseExpression(
        // We create a case arm for each location, which maps the IDs of the location to
        // a function of type [ThreadSet[self] -> { ... possible labels } ].
        // Depending on the location enumerator, the condition in the case arm can be of
        // the form: self = 1 or self \in 1..N (and possibly self = "P" or self \in {"P", "Q"}).
        epcCtx.locationThreads.map(
          (locEnum, threadEnums) =>
            CaseArm(
              // This is the case arm condition of the outer function. It has the form (self = 0 -> [...]) or (self |in 1..N -> [...])
              BinaryExpression(
                locEnum.asInstanceOf[BoundedEnumerator].equalOrIn,
                Identifier("self"),
                locEnum.asInstanceOf[BoundedEnumerator].domain
              ),
              // The expression is the new function. So we have a nested bounded mapping.
              // The nested bounded mapping have the following form: [thd \in ThreadSet[self] |-> CASE ... ].
              BoundedMapping(
                List(QuantifierBound(Identifier("thd"), FunctionApplication(Identifier("ThreadSet"), List(Identifier("self"))))),
                CaseExpression(
                  threadEnums.map(thdEnum => {
                    CaseArm(
                      // This is the case arm condition in the nested function. It has the form: (thd = 0 -> "label1") or (thd \in 1..N -> "label2").
                      BinaryExpression(
                        thdEnum.asInstanceOf[BoundedEnumerator].equalOrIn,
                        Identifier("thd"),
                        thdEnum.asInstanceOf[BoundedEnumerator].domain
                      ),
                      Literal(
                        // Here we can use the first edge of the CFG (which is the entry edge and should exist)
                        // and get its "end" operation (the operation, to which edge is pointing) to determine the
                        // entry label for the current (location, thread)-pair.
                        getEdgeEndOperation(
                          // This is the entry edge!
                          epcCtx.locThdEnumsToOperations((locEnum, thdEnum))._2.head,
                          // This is the region of the (location, thread)-pair.
                          epcCtx.locThdEnumsToOperations((locEnum, thdEnum))._1
                        ) match {
                          case Some(operation) => getOperationLabel(locEnum, thdEnum, operation, getOperationSuffix(operation))
                          case None => "Done"
                        }
                      )
                    )
                  }).toList,
                  None
                )
              )
            )
        ).toList,
        None
      )
    )
    val pcPredicate = BinaryExpression("=", Identifier("pc"), pcInitialValue)

    // Secondly, we will create state predicates for the initial state of the variables defined inside of locations.
    // Each location variable will have one corresponding TLA variable, which will contain a function
    // that maps location IDs (self \in ProcSet) to the value of the variable at that location.
    val locationVariablePredicates = for ((variable, locationsBuffer) <- epcCtx.variablesToLocations) yield {
      // First, we create the initial state of the variable, which is mapping [self \in ProcSet |-> CASE ... ]
      val tlaVariableInitialValue = BoundedMapping(
        List(QuantifierBound(Identifier("self"), Identifier("ProcSet"))),
        CaseExpression(
          locationsBuffer.map(
            (locEnum: Enumerator) =>
              CaseArm(
                BinaryExpression(
                  locEnum.asInstanceOf[BoundedEnumerator].equalOrIn,
                  Identifier("self"),
                  locEnum.asInstanceOf[BoundedEnumerator].domain
                ),
                // The initial value of the variable is available in the locationVariables map in the EPC spec context.
                epcCtx.locationVariables(locEnum).find(_.identifier == variable).get.expression
              )
          ).toList,
          // The default case (the value of the variable at all other locations) is Undefined.
          Some(Identifier("Undefined"))
        )
      )
      BinaryExpression("=", variable, tlaVariableInitialValue)
    }

    // Thirdly, we will create state predicates for the initial states of the defined channels. Each channel
    // definition (such as "new c"), is translated into a TLA+ variable (with the same name as the channel)
    // and holds the travelling messages, sent on that channel. Since channels will be implemented as Bags of
    // triples << sender, receiver, payload >>, the top-level definition "new c" will lead to the predicate
    // c = EmptyBag. However, since location networks can also be composed with a message aether (which is a
    // multiset of travelling messages), the initial state of those channels can also be changed in that way.
    // The account for these changes, each message eather in the network, that contains messages for a channel,
    // will be added to the initial EmptyBag for that channel with "(+) SetToBag({...})".
    // Implementation details: We have two possibilities for the definition of multi-sets: QuantifiedMultiSets
    // and ExplicitMultiSets. QuantifiedMultiSets do not contain duplicates and thus can be turned directly
    // into a SetToBag({...}) definition and added directly to the initial channel. ExplicitMultiSets might
    // contain several copies of the same message and in that case a single SetToBag({...}) will "delete" the
    // copies, which we don't want. For this reason, ExplicitMultiSets will be mapped to a SetToBag({...}) for
    // each message, which appears in them.

    val channelPredicates = mutable.ListBuffer.empty[Expression]
    def equalChannels(c1: Identifier, c2: Identifier) = c1 == c2 || c1 == Identifier(c2.name + "_") || Identifier(c1.name + "_") == c2
    for (channel <- epcCtx.channels) {
      val emptyBag = Identifier("EmptyBag")
      var channelInitialValue: Expression = emptyBag
      for (messageBag <- epcCtx.messageAether) {
        messageBag match {
          case ExplicitMultiSet(values) =>
            for (message <- values if equalChannels(message.channel, channel)) {
              // Add the message as a SetToBag({...}) to the channel.
              channelInitialValue = BinaryExpression(
                "(+)",
                channelInitialValue,
                IdentifierApplication(Identifier("SetToBag"),
                  // Here, we create the set with the single travelling message { ... }.
                  // The travelling message is implemented as a triple << sender, receiver, payload >>.
                  List(ExplicitSet(List(Tuple(List(message.source, message.target, message.value)))))
                )
              )
            }
          case QuantifiedMultiSet(BoundedEnumerator(boundVariable, "=", domain), message) if equalChannels(message.channel, channel) =>
            // Add the whole multiset as a SetToBag({...}) to the channel.
            // Since the bounded enumerator is defined with an equal sign "=", then it will be translated to a single message.
            // E.g. {{ (j = 1) c_{0 -> j}:j }}
            // Still, we need to the bounded variable, so we turn the domain into a set. This example will be translated into:
            // SetToBag({ << 0, j, j >> : j \in { 1 } })
            channelInitialValue = BinaryExpression(
              "(+)",
              channelInitialValue,
              IdentifierApplication(
                Identifier("SetToBag"),
                List(
                  // Here, we create the set with all travelling messages { ... }
                  QuantifiedSetComprehension(
                    // The travelling message is implemented as a triple << sender, receiver, payload >>.
                    Tuple(List(message.source, message.target, message.value)),
                    // Since the enumerator has the form (j = 1), we turn it into j \in { 1 }.
                    List(QuantifierBound(boundVariable, ExplicitSet(List(domain))))
                  )
                )
              )
            )
          case QuantifiedMultiSet(BoundedEnumerator(boundVariable, "\\in", domain), message) if equalChannels(message.channel, channel) =>
            // Add the whole multiset as a SetToBag({...}) to the channel.
            // Since the bounded enumerator is defined with "\in", then it will be translated to a multiple messages.
            // E.g. {{ (j \in 1..N) c_{0 -> j}:j }}
            // This time, we can keep the domain of the bounded enumerator as it is. This example will be translated into:
            // SetToBag({ << 0, j, j >> : j \in 1..N })
            channelInitialValue = BinaryExpression(
              "(+)",
              channelInitialValue,
              IdentifierApplication(
                Identifier("SetToBag"),
                List(
                  // Here, we create the set with all travelling messages { ... }
                  QuantifiedSetComprehension(
                    // The travelling message is implemented as a triple << sender, receiver, payload >>.
                    Tuple(List(message.source, message.target, message.value)),
                    // Since the enumerator has the form (j \in 1..N), we keep it as it is.
                    List(QuantifierBound(boundVariable, domain))
                  )
                )
              )
            )
          case QuantifiedMultiSet(BoundedEnumeratorWithFilter(boundVariable, "=", domain, filter), message) if equalChannels(message.channel, channel) =>
            // Add the whole multiset as a SetToBag({...}) to the channel.
            // Since the bounded enumerator is defined with an equal sign "=", then it will be translated to a single message.
            // We also need to take care of the filter in this case. This following example does not make much sense, but still
            // has to be handled correctly (which is, it has to be evaluated to an empty message after the translation):
            // E.g. {{ (j = 1 : j # 1) c_{0 -> j} : j }}
            // The bound variable can still be used in the message definition, so we need to keep it and turn the domain into a set.
            // This example will be translated into:
            // SetToBag({ << 0, j, j >> : j \in { j \in { 1 } : j # 1 } })
            // One problem here is that the j is introduced two times as a binder, but the model checker TLC seems okay with it.
            channelInitialValue = BinaryExpression(
              "(+)",
              channelInitialValue,
              IdentifierApplication(
                Identifier("SetToBag"),
                List(
                  // Here, we create the set with all travelling messages { ... }
                  QuantifiedSetComprehension(
                    // The travelling message is implemented as a triple << sender, receiver, payload >>.
                    Tuple(List(message.source, message.target, message.value)),
                    // Since the enumerator has the form (j = 1 : j # 1), we turn it into j \in { j \in { 1 } : j # 1 }.
                    List(
                      QuantifierBound(
                        // This is the outer binder j \in { ... }
                        boundVariable,
                        BoundedSetComprehension(
                          // This is the inner binder j \in { 1 } : j # 1
                          boundVariable,
                          // We turn the domain into a set, e.g 1 -> { 1 }.
                          ExplicitSet(List(domain)),
                          // We add the filter to this bounded set comprehension.
                          filter
                        )
                      )
                    )
                  )
                )
              )
            )
          case QuantifiedMultiSet(BoundedEnumeratorWithFilter(boundVariable, "\\in", domain, filter), message) if equalChannels(message.channel, channel) =>
            // Add the whole multiset as a SetToBag({...}) to the channel.
            // Since the bounded enumerator is defined with an "\in", then it will be translated to multiple messages.
            // We also need to take care of the filter in this case.
            // E.g. {{ (j \in 1..N : j # self) c_{self -> j} : j }}
            // This example will be translated into:
            // SetToBag({ << self, j, j >> : j \in { j \in 1..N : j # self } })
            // One problem here is that the j is introduced two times as a binder, but the model checker TLC seems okay with it.
            channelInitialValue = BinaryExpression(
              "(+)",
              channelInitialValue,
              IdentifierApplication(
                Identifier("SetToBag"),
                List(
                  // Here, we create the set with all travelling messages { ... }
                  QuantifiedSetComprehension(
                    // The travelling message is implemented as a triple << sender, receiver, payload >>.
                    Tuple(List(message.source, message.target, message.value)),
                    // Since the enumerator has the form (j \in 1..N : j # self), we turn it into j \in { j \in 1..N : j # self }.
                    List(
                      QuantifierBound(
                        // This is the outer binder j \in { ... }
                        boundVariable,
                        BoundedSetComprehension(
                          // This is the inner binder j \in 1..N : j # self
                          boundVariable,
                          // We keep the domain expression as it is, e.g 1..N -> 1..N.
                          domain,
                          // We add the filter to this bounded set comprehension.
                          filter
                        )
                      )
                    )
                  )
                )
              )
            )
          case _ => // do nothing
        }
      }
      channelPredicates addOne BinaryExpression("=", channel, channelInitialValue)
    }

    // O variable is a function that maps processes and its threads to sets of indices (initially empty)
    val OInitialValue = BinaryExpression("=", Identifier("O"),
      BoundedMapping(List(QuantifierBound(Identifier("self"), Identifier("ProcSet"))),
        BoundedMapping(List(QuantifierBound(Identifier("thd"), FunctionApplication(Identifier("ThreadSet"), List(Identifier("self"))))),
          ExplicitSet(Nil)
        )
      )
    )

    // Generate the Init predicate
    val initPredicate = OperatorDefinition(
      NonFixLHS(Identifier("Init"), Nil),
      ConjunctionList(
        List(pcPredicate)
        ++ locationVariablePredicates
        ++ channelPredicates.toList
        ++ (if (epcCtx.specWithMultiSends) then List(OInitialValue) else Nil)
      )
    )
    // Add the Init predicate to the TLA builder.
    tlaBuilder.appendUnit(initPredicate)
    tlaBuilder.appendNewline()
  }

  def translate(locationVariables: List[Identifier], tlaVariables: List[Identifier]): Unit = {
    val generatedLocationDisjunctions = mutable.ListBuffer.empty[OperatorDefinition]
    // We go over every location and start generating the actions for its threads.
    for ((locEnum, locationThreads) <- epcCtx.locationThreads) {
      val generatedThreadDisjunctions = mutable.ListBuffer.empty[OperatorDefinition]
      // Here, we also visit each thread separately
      for (thdEnum <- locationThreads) {
        val (region, edges) = epcCtx.locThdEnumsToOperations((locEnum, thdEnum))
        // We flatten the region, since we no longer need the nested structure.
        val flattenedRegion = region.flatMap(flattenOperation)
        // Here, we will gather the generated actions for the current (location, thread)-pair.
        val generatedThreadActions = mutable.ListBuffer.empty[OperatorDefinition]
        // We go over all operations and translate them into TLA actions (some operations can be translated into multiple TLA actions.)
        // Assignments, send actions, and thread instantiations have one corresponding TLA action.
        // Receive actions (in the case of weak accuracy) and conditional threads are translated into two TLA actions.
        for {
          (operation, idx) <- flattenedRegion.zipWithIndex
          tlaAction <- translateOperation(locEnum, thdEnum, operation, flattenedRegion, edges, locationVariables, tlaVariables)
        } {
          if (idx == 0) {
            // We add a comment before the first TLA action to indicate the beginning of the actions for the current (location, thread)-pair.
            appendInfoComment(locEnum, thdEnum)
            tlaBuilder.appendNewline()
          }
          // We add the translated action to the TLA builder.
          tlaBuilder.appendUnit(tlaAction)
          tlaBuilder.appendNewline()
          // And we store it locally, because we also want to generate the disjunction of all the actions for the (location, thread)-pair.
          generatedThreadActions.addOne(tlaAction)
        }

        // If we were able to generate TLA actions for the operation, we proceed. (Empty send commands are not translated to TLA actions.)
        if (generatedThreadActions.nonEmpty) {
          // Next, we generate the disjunction of all possible thread actions.
          // E.g. p_j(self, thd) == p_j_1A(self, thd) \/ p_j_2R(self, thd) \/ ...
          val threadActionsDisjunctionOperator: OperatorDefinition = generateThreadActionsDisjunction(locEnum, thdEnum, generatedThreadActions)

          // We add the disjunction of the thread actions to the TLA builder.
          tlaBuilder.appendUnit(threadActionsDisjunctionOperator)
          tlaBuilder.appendNewline()
          // And we store it locally, in order to generate a disjunction of all threads for the location
          generatedThreadDisjunctions.addOne(threadActionsDisjunctionOperator)
        }
      }
      // If we were able to generate a disjunction for at least one thread, we proceed.
      // (A single thread consisting only of an empty send action would not create a disjunction of its actions.)
      if (generatedThreadDisjunctions.nonEmpty) {
        // Lastly, we will generate a disjunction for the location with all of its thread disjunctions
        val locationThreadsDisjunction: OperatorDefinition = generateLocationThreadsDisjunction(locEnum, generatedThreadDisjunctions)
        // We add the disjunction for the location with the threads disjunction to the TLA builder.
        tlaBuilder.appendUnit(locationThreadsDisjunction)
        tlaBuilder.appendNewline()
        // And we store it locally, in order to use in the generation of the Next action.
        generatedLocationDisjunctions.addOne(locationThreadsDisjunction)
      }
    }

    // Generate the Terminating operator
    val terminatingDefinition = OperatorDefinition(
      NonFixLHS(Identifier("Terminating"), Nil),
      ConjunctionList(
        List(
          generateTerminationPredicate(),
          UnaryExpression("UNCHANGED", Identifier("vars"))
        )
      )
    )
    // Add the Terminating operator to the TLA builder.
    tlaBuilder.appendUnit(terminatingDefinition)
    tlaBuilder.appendNewline()

    // Generate the Next action
    val nextDefinition = OperatorDefinition(
      NonFixLHS(Identifier("Next"), Nil),
      foldToDisjunction(generatedLocationDisjunctions ++ List(terminatingDefinition))
    )
    // Add the Next operator to the TLA builder.
    tlaBuilder.appendUnit(nextDefinition)
    tlaBuilder.appendNewline()

    // Generate the specification Spec
    val specDefinition = OperatorDefinition(
      NonFixLHS(Identifier("Spec"), Nil),
      BinaryExpression(
        "/\\",
        Identifier("Init"),
        UnaryExpression(
          "[]",
          WeakStuttering(
            Identifier("Next"),
            Identifier("vars")
          )
        )
      )
    )
    // Add the Spec operator to the TLA builder.
    tlaBuilder.appendUnit(specDefinition)
    tlaBuilder.appendNewline()

    // Generate the Termination definition
    val terminationDefinition = OperatorDefinition(
      NonFixLHS(Identifier("Termination"), Nil),
      UnaryExpression("<>", ParenthesizedExpression(generateTerminationPredicate()))
    )
    // Add the Terminating operator to the TLA builder.
    tlaBuilder.appendUnit(terminationDefinition)
    tlaBuilder.appendNewline()
  }

  /**
   * Method for the generation of the "Termination"-predicate.
   * The predicate does not depend on individual locations or threads!
   *
   * \A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done"
   */
  private def generateTerminationPredicate() = {
    BoundedQuantifiedExpression(
      "\\A",
      List(QuantifierBound(Identifier("self"), Identifier("ProcSet"))),
      BoundedQuantifiedExpression(
        "\\A",
        List(
          QuantifierBound(Identifier("thd"), FunctionApplication(Identifier("ThreadSet"), List(Identifier("self"))))
        ),
        BinaryExpression(
          "=",
          FunctionApplication(
            FunctionApplication(
              Identifier("pc"),
              List(
                Identifier("self")
              )
            ),
            List(
              Identifier("thd")
            )
          ),
          Literal("Done")
        )
      )
    )
  }

  private def generateThreadActionsDisjunction(locEnum: Enumerator, thdEnum: Enumerator, generatedTlaActions: ListBuffer[OperatorDefinition]) = {
    val threadActionsDisjunction = foldToDisjunction(generatedTlaActions)
    val threadActionsDisjunctionLabel = locEnum.asInstanceOf[BoundedEnumerator].name.name + "_" + thdEnum.asInstanceOf[BoundedEnumerator].name.name

    val threadActionsDisjunctionOperator = OperatorDefinition(
      NonFixLHS(Identifier(threadActionsDisjunctionLabel), List(Identifier("self"), Identifier("thd"))),
      threadActionsDisjunction
    )
    threadActionsDisjunctionOperator
  }

  private def foldToDisjunction(operatorDefinitions: ListBuffer[OperatorDefinition]) = {
    operatorDefinitions.map(operatorDefinition =>
      IdentifierApplication(
        operatorDefinition.signature.identifier,
        // we know that the arguments are the identifiers "self" and "thd"
        operatorDefinition.signature.arguments.asInstanceOf[List[Identifier]]
      )
    ).toList match {
      case Nil => ???
      case head :: Nil => head
      case head :: next => next.fold(head)((expr1: Expression, expr2: Expression) => BinaryExpression("\\/", expr1, expr2))
    }
  }

  private def generateLocationThreadsDisjunction(locEnum: Enumerator, threadDisjunctions: ListBuffer[OperatorDefinition]) = {
    // Use the identifier of the location as the name of the new operator
    val locationThreadsDisjunctionLabel = locEnum.asInstanceOf[BoundedEnumerator].name.name
    // Create a bounded quantified expression of the form:
    // \E self \in {"P"}: \E thd \in ThreadSet[self]: p_j(self, thd) \/ p_k(self, thd)
    // \E self \in 1..N: \E thd \in ThreadSet[self]: p_j(self, thd) \/ p_k(self, thd)
    val body = BoundedQuantifiedExpression(
      "\\E",
      List(
        QuantifierBound(
          Identifier("self"),
          if (locEnum.asInstanceOf[BoundedEnumerator].equalOrIn == "=")
            then ExplicitSet(List(locEnum.asInstanceOf[BoundedEnumerator].domain))
            else locEnum.asInstanceOf[BoundedEnumerator].domain
        )
      ),
      BoundedQuantifiedExpression(
        "\\E",
        List(
          QuantifierBound(
            Identifier("thd"),
            FunctionApplication(
              Identifier("ThreadSet"),
              List(
                Identifier("self")
              )
            )
          )
        ),
        foldToDisjunction(threadDisjunctions)
      )
    )
    // Return the new operator definition.
    OperatorDefinition(NonFixLHS(Identifier(locationThreadsDisjunctionLabel), Nil), body)
  }

  private def flattenOperation(operation: Operation): Region = operation match {
    case LeafOperation(id, label, command) => List(operation)
    case IfOperation(id, label, condition, thenRegion, elseRegion) =>
      List(operation) ++ thenRegion.flatMap(flattenOperation) ++ elseRegion.flatMap(flattenOperation)
  }

  /**
   * Append a comment to the tlaBuilder to indicate the beginning of the actions of the location and thread.
   * E.g. Location p = "P", thread j /in 1..N
   */
  private def appendInfoComment(locEnum: Enumerator, thdEnum: Enumerator): Unit = {
    val boundedLocEnum = locEnum.asInstanceOf[BoundedEnumerator]
    val boundedThdEnum = thdEnum.asInstanceOf[BoundedEnumerator]
    tlaBuilder.appendString("\\* Location ")
    tlaBuilder.appendExpression(boundedLocEnum.name)
    tlaBuilder.appendString(" ")
    tlaBuilder.appendString(boundedLocEnum.equalOrIn)
    tlaBuilder.appendString(" ")
    tlaBuilder.appendExpression(boundedLocEnum.domain)
    tlaBuilder.appendString(", thread ")
    tlaBuilder.appendExpression(boundedThdEnum.name)
    tlaBuilder.appendString(" ")
    tlaBuilder.appendString(boundedThdEnum.equalOrIn)
    tlaBuilder.appendString(" ")
    tlaBuilder.appendExpression(boundedThdEnum.domain)
  }

  private def translateOperation(locEnum: Enumerator,
                                 thdEnum: Enumerator,
                                 operation: Operation,
                                 flattenedRegion: Region,
                                 edges: List[DiEdge],
                                 locationVariables: List[Identifier],
                                 allTLAVariables: List[Identifier]): List[OperatorDefinition] = {
    val currentLabel = getOperationLabel(locEnum, thdEnum, operation, getOperationSuffix(operation))
    val successorOption = getSuccessor(operation, flattenedRegion, edges)
    val successorLabel = getSuccessorLabel(locEnum, thdEnum, successorOption)
    val pcEnablingCondition = generatePCEnablingCondition(locEnum, thdEnum, currentLabel)
    val pcUpdate = generatePCUpdate(successorLabel)
    operation match {
      case LeafOperation(id, label, command) =>
        command match {
          case Declaration(identifier) =>
            List(
              generateTLAAction(
                locEnum = locEnum,
                thdEnum = thdEnum,
                operation = operation,
                getSuccessorOperation = () => getSuccessor(operation, flattenedRegion, edges),
                getActionEffect = () => generateDeclarationEffect(identifier, locationVariables),
                getUnchangedVars = () => allTLAVariables.filterNot(Set(identifier, Identifier("pc")) contains _)
              )
            )
          case Assignment(identifier, expression) =>
            List(
              generateTLAAction(
                locEnum = locEnum,
                thdEnum = thdEnum,
                operation = operation,
                getSuccessorOperation = () => getSuccessor(operation, flattenedRegion, edges),
                getActionEffect = () => generateAssignmentEffect(identifier, expression, locationVariables),
                getUnchangedVars = () => allTLAVariables.filterNot(Set(identifier, Identifier("pc")) contains _)
              )
            )
          case Receive(channel, location, variable) =>
            List(
              generateTLAAction(
                locEnum = locEnum,
                thdEnum = thdEnum,
                operation = operation,
                getSuccessorOperation = () => getSuccessor(operation, flattenedRegion,edges),
                getActionEffect = () => generateReceiveEffect(channel, location, variable, locationVariables),
                getUnchangedVars = () => allTLAVariables.filterNot(Set(variable, Identifier("pc")) contains _)
              )
            )
          case Send(ExplicitMultiSet(Nil)) => Nil
          case Send(messages) =>
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(Identifier("self"), Identifier("thd"))), ConjunctionList(
                List(pcEnablingCondition) ++ generateSendEffect(messages, locationVariables, successorLabel) ++ List(generateUnchangedExpression(
                  messages match {
                    case ExplicitMultiSet(Nil) => allTLAVariables
                    case ExplicitMultiSet(message :: Nil) => allTLAVariables.filterNot(Set(message.channel, Identifier("pc")) contains _)
                    case ExplicitMultiSet(messages) => allTLAVariables.filterNot(Set(messages.head.channel, Identifier("pc"), Identifier("O")) contains _)
                    case QuantifiedMultiSet(enumerator, message) => allTLAVariables.filterNot(Set(message.channel, Identifier("pc"), Identifier("O")) contains _)
                  }
                ))
              ))
            )
          case ThreadInstantiation(identifier) =>
            List(
              generateTLAAction(
                locEnum = locEnum,
                thdEnum = thdEnum,
                operation = operation,
                getSuccessorOperation = () => getSuccessor(operation, flattenedRegion, edges),
                getActionEffect = () => generateThreadInstantiationEffect(epcCtx.thdDefinitionsToFreeVariables(identifier), locationVariables),
                getUnchangedVars = () => allTLAVariables.filterNot(Set(Identifier("pc")) contains _)
              )
            )

          case ConditionalThread(_, _, _) => Nil // this should not happen, since conditional threads are turned into IfOperations!
        }

      case IfOperation(id, label, condition, thenRegion, elseRegion) =>
        val thenActionLabel = getOperationLabel(locEnum, thdEnum, operation, "T")
        val elseActionLabel = getOperationLabel(locEnum, thdEnum, operation, "F")
        val elseSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "else")
        val thenSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "then")
        val thenSuccessorLabel = getSuccessorLabel(locEnum, thdEnum, thenSuccessor)
        val elseSuccessorLabel = getSuccessorLabel(locEnum, thdEnum, elseSuccessor)
        List(
          OperatorDefinition(NonFixLHS(Identifier(thenActionLabel), List(Identifier("self"), Identifier("thd"))), ConjunctionList(
            List(pcEnablingCondition)
            ++ generateTruePrecondition(condition, locationVariables)
            ++ List(generatePCUpdate(thenSuccessorLabel))
            ++ List(generateUnchangedExpression(allTLAVariables.filter(_ != Identifier("pc"))))
          )),
          OperatorDefinition(NonFixLHS(Identifier(elseActionLabel), List(Identifier("self"), Identifier("thd"))), ConjunctionList(
            List(pcEnablingCondition)
            ++ generateFalsePrecondition(condition, locationVariables)
            ++ List(generatePCUpdate(elseSuccessorLabel))
            ++ List(generateUnchangedExpression(allTLAVariables.filter(_ != Identifier("pc"))))
          ))
        )
    }
  }

  private def generateTLAAction(locEnum: Enumerator,
                        thdEnum: Enumerator,
                        operation: Operation,
                        getSuccessorOperation: () => Option[Operation],
                        getActionEffect: () => List[Expression],
                        getUnchangedVars: () => List[Identifier]): OperatorDefinition = {
    // Generate the labels for the current operation and the successor operation
    val currentLabel: String = getOperationLabel(locEnum, thdEnum, operation, getOperationSuffix(operation))
    val successorLabel: String = getSuccessorOperation() match {
      case Some(successorOperation) => getOperationLabel(locEnum, thdEnum, successorOperation, getOperationSuffix(successorOperation))
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

  private def getOperationLabel(locEnum: Enumerator, thdEnum: Enumerator, operation: Operation, operationSuffix: String): String =
    operation.label match {
      case Some(value) => value.name
      // If there is no label, we create one using the location identifier, thread identifier, operation id and suffix.
      // E.g. p_j_1A, p_j_2R, p_j_3S, ...
      case None => "%s_%s_%d%s" format(
        locEnum.asInstanceOf[BoundedEnumerator].name.name,
        thdEnum.asInstanceOf[BoundedEnumerator].name.name,
        operation.id,
        operationSuffix
      )
    }

  private def getOperationLabel(thdIdentifier: Identifier, operation: Operation, operationSuffix: String): String =
    operation.label match {
      case Some(value) => value.name
      case None => "%s_%d%s" format(thdIdentifier.name, operation.id, operationSuffix)
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

  private def getSuccessorLabel(locEnum: Enumerator, thdEnum: Enumerator, successorOption: Option[Operation]): String =
    successorOption match {
      case Some(successorOperation) => getOperationLabel(locEnum, thdEnum, successorOperation, getOperationSuffix(successorOperation))
      case None => "Done"
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
    // E.g. pc' = [pc EXCEPT ![self][thd] = "<successorLabel>"]
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

  private def getSuccessor(operation: Operation, containingRegion: Region, edges: List[DiEdge]): Option[Operation] =
    edges.find(_.start == operation.id).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

  private def getSuccessorByEdgeLabel(operation: Operation, containingRegion: Region, edges: List[DiEdge], edgeLabel: String): Option[Operation] =
    edges.find(edge => edge.start == operation.id && edge.label.contains(edgeLabel)).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

  private def getEdgeEndOperation(edge: DiEdge, containingRegion: Region): Option[Operation] =
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

  private def generateUnchangedExpression(unchangedVariables: List[Identifier]): Expression = {
    // Generate the unchanged variables conjunct.
    // E.g. UNCHANGED << x, y, ... >>
    UnaryExpression("UNCHANGED", IdentifierTuple(unchangedVariables))
  }

  private def generateDeclarationEffect(identifier: Identifier, locationVariables: List[Identifier]): List[Expression] = {
    // We require that the variable in the var action is Undefined at the location.
    // E.g. x[self] = Undefined
    val variableIsUndefined = BinaryExpression(
      "=",
      // FunctionApplication(Identifier(identifier.name), List(Identifier("self"))),
      EPCtoTLAExpression.rewrite(identifier, locationVariables),
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

  private def generateAssignmentEffect(identifier: Identifier, expression: Expression, locationVariables: List[Identifier]): List[Expression] = {
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
            // Rewrite all variable read operations like this x -> x[self]
            EPCtoTLAExpression.rewrite(expression, locationVariables)
          )
        )
      )
    )
    List(variableIsNotUndefined, variableIsSetToExpressionInNextState)
  }

  private def generateReceiveEffect(channel: Expression, locationExpr: Expression, variable: Identifier, locationVariables: List[Identifier]): List[Expression] = {
    // We require that the variable in the "receive" action is not Undefined at the location.
    // E.g. x[self] # Undefined
    val variableIsNotUndefined = BinaryExpression(
      "#",
      // FunctionApplication(variable, List(Identifier("self"))),
      EPCtoTLAExpression.rewrite(variable, locationVariables),
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
            EPCtoTLAExpression.rewrite(locationExpr, locationVariables)
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

  private def generateSendEffect(messages: MultiSet[OutgoingMessage], locationVariables: List[Identifier], successorLabel: String): List[Expression] = {
    messages match {
      case ExplicitMultiSet(Nil) =>
        // Empty send commands are filtered before we reach this code, so we do nothing!
        Nil
      case ExplicitMultiSet(singleMsg :: Nil) =>
        // Sending a single message means that we send it directly, so we simply need to update the channel!
        val channelUpdate = generateChannelUpdateForSingleSend(singleMsg, locationVariables)
        val pcUpdate = generatePCUpdate(successorLabel)
        List(channelUpdate, pcUpdate)
      case ExplicitMultiSet(multipleMessages) =>
        // TODO We only allow the sending of multiple messages in a single comman on the same channel!
        // Sending multiple messages means that we also need to remember what was sent so far.
        // The implementation of the Send command is based on existential quantification over the set of messages
        // that are still not send. Since we cannot simply send a multiset of all messages at once (since the values
        // of the target and value expressions in the messages can change between sending two messages; this can happen
        // if we have a separate thread in the same location, which updates the variables used in the message),
        // we will use a set of indices to remember, which messages were already send. This means that the O variable
        // will contain exactly those indices.
        //
        // E.g. if we need to send two messages, lets say {{ c@2!"Hello", c@3!"Hello" }},
        // we create the set 1..2 and maintain O[self][thd], which should initially be empty,
        // then \E i \in 1..2 \ O[self][thd]: ... will give us the index of the next message to send.
        // The changes will include a change in the channel based on a case distinction for the different indices:
        // - c' = CASE i = 1 -> c (+) SetToBag({ << self, 2, "Hello" >> })
        //        []   i = 2 -> c (+) SetToBag({ << self, 3, "Hello" >> })
        // an update of the O variable to include the index i, which is being send with the current action.
        // Importantly, once we have sent all message, we need to reset O to the empty set, in order to "clean it up".
        // - O' = [O EXCEPT ![self][thd] = IF @ \cup { i } # 1..2
        //                                    THEN @ \cup { i }
        //                                    ELSE {} ]
        // And finally, we need to update the pc in two ways: it stays the same if there are still messages to be send,
        // or it moves to the next command if all messages were already sent (we recognize this using O')
        // - pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup { i } # 1..2
        //                                    THEN @
        //                                    ELSE "successorLabel"]
        val indexBinder = Identifier("i")
        val allIndices = BinaryExpression("..", Literal(1), Literal(multipleMessages.size))
        val alreadySend = FunctionApplication(FunctionApplication(Identifier("O"), List(Identifier("self"))), List(Identifier("thd")))
        val remainingIndices = BinaryExpression("\\", allIndices, alreadySend)
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(indexBinder, remainingIndices)),
            ConjunctionList(List(
              generateChannelUpdateForMultiSend(indexBinder, multipleMessages, locationVariables),
              generateOUpdateForMultiSend(indexBinder, allIndices),
              generatePCUpdateForMultiSend(indexBinder, allIndices, successorLabel)
            ))
          )
        )
      case QuantifiedMultiSet(enumerator, message) =>
        // Sending a quantified set of messages also means that we also need to remember what was sent so far.
        // Add the whole multiset as a SetToBag({...}) to the channel.
        // Since the bounded enumerator is defined with an equal sign "=", then it will be translated to a single message.
        // E.g. {{ (j = 1) c@j!<<j>> }}
        // Still, we need to the bounded variable, so we turn the domain into a set. This example will be translated into:
        // SetToBag({ << self, j, j >> })
        val (boundVariable, indexSet) = enumerator match {
          case BoundedEnumerator(name, "=", domain) =>
            val rewrittenDomain = EPCtoTLAExpression.rewrite(domain, locationVariables)
            (name, ExplicitSet(List(rewrittenDomain)))
          case BoundedEnumerator(name, "\\in", domain) =>
            val rewrittenDomain = EPCtoTLAExpression.rewrite(domain, locationVariables)
            (name, rewrittenDomain)
          case BoundedEnumeratorWithFilter(name, "=", domain, filter) =>
            val rewrittenDomain = EPCtoTLAExpression.rewrite(domain, locationVariables)
            val rewrittenFilter = EPCtoTLAExpression.rewrite(filter, locationVariables)
            (name, BoundedSetComprehension(name, ExplicitSet(List(rewrittenDomain)), rewrittenFilter))
          case BoundedEnumeratorWithFilter(name, "\\in", domain, filter) =>
            val rewrittenDomain = EPCtoTLAExpression.rewrite(domain, locationVariables)
            val rewrittenFilter = EPCtoTLAExpression.rewrite(filter, locationVariables)
            (name, BoundedSetComprehension(name, rewrittenDomain, rewrittenFilter))
        }
        val alreadySend = FunctionApplication(FunctionApplication(Identifier("O"), List(Identifier("self"))), List(Identifier("thd")))
        val remainingIndices = BinaryExpression("\\", indexSet, alreadySend)
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(boundVariable, remainingIndices)),
            ConjunctionList(List(
              generateChannelUpdateForSingleSend(message, locationVariables),
              generateOUpdateForMultiSend(boundVariable, indexSet),
              generatePCUpdateForMultiSend(boundVariable, indexSet, successorLabel)
            ))
          )
        )
    }
  }

  private def generateChannelUpdateForSingleSend(singleMsg: OutgoingMessage, locationVariables: List[Identifier]) = {
    BinaryExpression(
      "=",
      PrimedExpression(singleMsg.channel),
      BinaryExpression(
        "(+)",
        singleMsg.channel,
        IdentifierApplication(
          Identifier("SetToBag"), List(ExplicitSet(List(
            // Here, we create a triple of the form << self, target, value >>
            Tuple(List(
              Identifier("self"),
              EPCtoTLAExpression.rewrite(singleMsg.target, locationVariables),
              EPCtoTLAExpression.rewrite(singleMsg.value, locationVariables)
            ))
          )))
        )
      )
    )
  }

  private def generateChannelUpdateForMultiSend(indexBinder: Identifier, multipleMessages: List[OutgoingMessage], locationVariables: List[Identifier]) = {
    BinaryExpression(
      "=",
      PrimedExpression(multipleMessages.head.channel),
      CaseExpression(
        for ((message, idx) <- multipleMessages.zipWithIndex) yield {
          CaseArm(
            BinaryExpression("=", indexBinder, Literal(idx + 1)),
            BinaryExpression("(+)", multipleMessages.head.channel,
              IdentifierApplication(Identifier("SetToBag"), List(
                Tuple(List(
                  Identifier("self"),
                  EPCtoTLAExpression.rewrite(message.target, locationVariables),
                  EPCtoTLAExpression.rewrite(message.value, locationVariables)
                ))
              ))
            )
          )
        },
        None
      )
    )
  }

  def generateOUpdateForMultiSend(binder: Identifier, domain: Expression): Expression = {
    val O = Identifier("O")
    BinaryExpression(
      "=",
      PrimedExpression(O),
      ExceptExpression(
        O,
        List(
          // EXCEPT
          ExceptClause(
            // ![self][thd] =
            SelectorPath(List(IndexSelector(List(Identifier("self"))), IndexSelector(List(Identifier("thd"))))),
            // IF @ \cup { i } # <domain>
            IfThenElse(
              BinaryExpression(
                "#",
                BinaryExpression("\\cup", AtSign(), ExplicitSet(List(binder))),
                domain
              ),
              // THEN @ \cup { i }
              BinaryExpression("\\cup", AtSign(), ExplicitSet(List(binder))),
              // ELSE {}
              ExplicitSet(Nil)
            )
          )
        )
      )
    )
  }

  private def generatePCUpdateForMultiSend(binder: Identifier, domain: Expression, successorLabel: String) = {
    val O = Identifier("O")
    val pc = Identifier("pc")
    BinaryExpression(
      "=",
      PrimedExpression(pc),
      ExceptExpression(
        pc,
        List(
          // EXCEPT
          ExceptClause(
            // ![self][thd] =
            SelectorPath(List(IndexSelector(List(Identifier("self"))), IndexSelector(List(Identifier("thd"))))),
            // IF O[self][thd] \cup { i } # <domain> THEN @ ELSE <successorLabel>
            IfThenElse(
              BinaryExpression(
                "#",
                BinaryExpression(
                  "\\cup",
                  FunctionApplication(FunctionApplication(O, List(Identifier("self"))), List(Identifier("thd"))),
                  ExplicitSet(List(binder)),
                ),
                domain
              ),
              // THEN @
              AtSign(),
              // ELSE <successorLabel>
              Literal(successorLabel)
            )
          )
        )
      )
    )
  }

  private def generateTruePrecondition(condition: Expression, locationVariables: List[Identifier]): List[Expression] = {
    List(
      EPCtoTLAExpression.rewrite(condition, locationVariables)
    )
  }

  private def generateFalsePrecondition(condition: Expression, locationVariables: List[Identifier]): List[Expression] = {
    List(
      UnaryExpression(
        "~",
        ParenthesizedExpression(
          EPCtoTLAExpression.rewrite(condition, locationVariables)
        )
      )
    )
  }

  private def generateThreadInstantiationEffect(freeVariables: List[Identifier], locationVariables: List[Identifier]): List[Expression] = {
    // We require that the free variables from the thread definition are not Undefined at the location.
    // E.g. x[self] # Undefined, y[self] # Undefined
    for identifier <- freeVariables yield {
      BinaryExpression(
        "#",
        // FunctionApplication(identifier, List(Identifier("self"))),
        EPCtoTLAExpression.rewrite(identifier, locationVariables),
        Identifier("Undefined")
      )
    }
  }
}

object EPCtoTLAExpression {

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

  def getModuleUnits(): List[ModuleUnit] = generatedModuleUnits.toList

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
      case Token(kind, value) => appendString(value)
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
      case IdentifierTuple(identifiers) =>
        appendExpressions(identifiers, "<< ", ", ", " >>")
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
      case Tuple(expressions) =>
        appendExpressions(expressions, "<< ", ", ", " >>")
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
        if (Set("..") contains operator) {
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
          appendString("/\\ ")
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
        appendString(" EXCEPT !")
        for ((clause, idx) <- clauses.zipWithIndex) {
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
          if (idx != clauses.length - 1) appendString(", ")
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


