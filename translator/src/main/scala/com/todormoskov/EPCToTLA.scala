package com.todormoskov

import com.todormoskov.Lexer.{Token, TokenKind}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class EPCToTLA(epcCtx: EPCSpecContext) {
  private val tlaStringBuilder = new TLAStringBuilder

  // Constant names
  private val CONSTANTS = "CONSTANTS"
  private val VARIABLES = "VARIABLES"
  private val UNDEFINED = "Undefined"
  private val INITIALIZED = "Initialized"
  private val PROC_SET = "ProcSet"
  private val THREAD_SET = "ThreadSet"
  private val SELF = "self"
  private val THD = "thd"
  private val PC = "pc"
  private val RA = "ra"
  private val _O = "O"
  private val FAILED_SET = "F"
  private val TRUSTED_IMMORTAL = "trim"
  private val DONE = "Done"
  private val VARS = "vars"
  private val INIT = "Init"
  private val FAIL = "Fail"
  private val TERMINATING = "Terminating"
  private val NEXT = "Next"
  private val SPEC = "Spec"
  private val TERMINATION = "Termination"
  private val EMPTY_BAG = "EmptyBag"
  private val SET_TO_BAG = "SetToBag"

  // Constant names as identifiers
  private val Undefined = Identifier(UNDEFINED)
  private val Initialized = Identifier(INITIALIZED)
  private val ProcSet = Identifier(PROC_SET)
  private val ThreadSet = Identifier(THREAD_SET)
  private val self = Identifier(SELF)
  private val thd = Identifier(THD)
  private val pc = Identifier(PC)
  private val ra = Identifier(RA)
  private val O = Identifier(_O)
  private val F = Identifier(FAILED_SET)
  private val trim = Identifier(TRUSTED_IMMORTAL)
  private val Done = Literal(DONE)
  private val vars = Identifier(VARS)
  private val Init = Identifier(INIT)
  private val Fail = Identifier(FAIL)
  private val Terminating = Identifier(TERMINATING)
  private val Next = Identifier(NEXT)
  private val Spec = Identifier(SPEC)
  private val Termination = Identifier(TERMINATION)

  private val emptyBag = Identifier(EMPTY_BAG)
  private val emptySet = ExplicitSet(Nil)
  private val emptyTuple = Tuple(Nil)
  private val SetToBag = Identifier(SET_TO_BAG)
  private val selfInProcSet = QuantifierBound(self, ProcSet)
  private val thdInThreadSetSelf = QuantifierBound(thd, FunctionApplication(ThreadSet, List(self)))
  private val pcThreadValue = ThdLvlFnAppl(pc)

  // Helper methods
  private def ThdLvlFnAppl(identifier: Identifier): Expression =
    FunctionApplication(FunctionApplication(identifier, List(self)), List(thd))

  /**
   * Entry method for the translation of the EPC spec to TLA+.
   */
  def translateSpec(): TLAStringBuilder = {
    // We add the constants Undefined and Initialized.
    val constants = ConstantDeclaration(Token(TokenKind.CONSTANTS, CONSTANTS, -1, -1),
      List(OperatorDeclaration(Undefined, Nil), OperatorDeclaration(Initialized, Nil)))
    tlaStringBuilder.appendUnit(constants, newlines = 2)

    // We generate the variable declarations. This includes the declaration of all variables
    // which appear in the memory of a location or are declared with the 'var' keyword.
    // Additionally, we create a variable for the program counters (pc) of the different locations
    // and their threads. Finally, we create variables for each channel, with the same name as the channel.
    val raOption = Option.when(epcCtx.thdDefinitionsToOperations.nonEmpty)(ra)
    val OOption = Option.when(epcCtx.specWithMultiSends)(O)
    val FOption = Option.when(epcCtx.failStopConfigured) (F)
    val trimOption = Option.when(epcCtx.weakAccuracyConfigured) (trim)
    val locationVariables = epcCtx.variablesToLocations.keys.toList
    val declaredChannels = epcCtx.channels.toList
    val allTLAVariables = List(pc) ++ raOption ++ OOption ++ FOption ++ trimOption ++ locationVariables ++ declaredChannels
    val variablesDeclaration = VariableDeclaration(Token(TokenKind.VARIABLES, VARIABLES, -1, -1), allTLAVariables)
    tlaStringBuilder.appendUnit(variablesDeclaration, newlines = 2)

    // We also add the operator vars == << ... >>, which is a list with all variables.
    val varsOperator = OperatorDefinition(NonFixLHS(vars, Nil), Tuple(allTLAVariables))
    tlaStringBuilder.appendUnit(varsOperator, newlines = 2)

    // Next, we generate the operator ProcSet, which is a set of all process IDs.
    // E.g. ProcSet == {"P"} \union {"Q"}
    val procSetExpression = toUnionOfSets(epcCtx.locationThreads.keys.toList)
    val procSetDefinition = OperatorDefinition(NonFixLHS(ProcSet, Nil), procSetExpression)
    tlaStringBuilder.appendUnit(procSetDefinition, newlines = 2)

    // Now, we proceed with the generated of the function ThreadSet, which maps each process ID to the set of its threads.
    // E.g. "P" |-> {1} \union {2}; "Q" |-> 1..N
    val locEnumToThreadSetExpression = epcCtx.locationThreads
      .filter((locEnum, _) => locEnum.isInstanceOf[BoundedEnumerator])
      .map((locEnum, threadEnums) => (locEnum.asInstanceOf[BoundedEnumerator], toUnionOfSets(threadEnums.toList)))
    // These (identifier, expression) mappings will be rewritten as TLA+ case arms expression.
    // The resulting TLA+ code will look like this:
    // ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1} \union {2}
    //                                    []   self = "Q" -> 1..N ]
    val threadSetDefinition = OperatorDefinition(NonFixLHS(ThreadSet, Nil),
      BoundedMapping(List(selfInProcSet),
        CaseExpression(locEnumToThreadSetExpression.map((locEnum, expression) =>
          CaseArm(BinaryExpression(locEnum.equalOrIn, self, locEnum.domain), expression) ).toList, None)))
    tlaStringBuilder.appendUnit(threadSetDefinition, newlines = 2)

    generateInitStatePredicate()
    val locationOperators = translateLocations(locationVariables, allTLAVariables)
    val threadDefinitionOperators = translateThreadDefinitions(locationVariables, allTLAVariables)
    generateRemainingSpecOperators(allTLAVariables, locationOperators, threadDefinitionOperators)

    tlaStringBuilder
  }

  /**
   * Maps each enumerator to a set and then folds them to a union sets.
   */
  private def toUnionOfSets(enumerators: List[Enumerator]) =
    enumerators.map(enumeratorDomainToSet) match {
      case Nil => emptySet
      case x :: Nil => x
      case x :: xs => xs.fold(x)((expr1, expr2) => BinaryExpression("\\union", expr1, expr2))
    }

  /**
   * Maps an enumerator to its domain as a set.
   */
  private def enumeratorDomainToSet(enumerator: Enumerator) =
    enumerator match {
      case BoundedEnumerator(name, "=", domain) => ExplicitSet(List(domain)) // here we create a set with one element
      case BoundedEnumerator(name, "\\in", domain) => domain // the domain in this case is already a set
      case _ => emptySet // currently we ignore bounded enumerators with filters
    }

  /**
   * Generates the initial state predicate.
   */
  private def generateInitStatePredicate(): Unit = {
    // Firstly, we generate the initial state of the pc variable.
    val pcInitialValue = generatePCInitStatePredicate()

    // ra variable is a function that maps processes and its threads to lists of return addresses (initially empty)
    val raInitialValue = Option.when(epcCtx.thdDefinitionsToOperations.nonEmpty)
      (BinaryExpression("=", ra, BoundedMapping(List(selfInProcSet), BoundedMapping(List(thdInThreadSetSelf), emptyTuple))))

    // O variable is a function that maps processes and its threads to sets of indices (initially empty).
    val OInitialValue = Option.when(epcCtx.specWithMultiSends)
      (BinaryExpression("=", O, BoundedMapping(List(selfInProcSet), BoundedMapping(List(thdInThreadSetSelf), emptySet))))

    // F is a variable that contains failed processes and is used only when the FailStop modifier is configured.
    val FInitialValue = Option.when(epcCtx.failStopConfigured)(BinaryExpression("=", F, emptySet))

    // trim is variable that identifies the "Trusted Immortal" and is used when then WeakAccuracy modifier is configured.
    val trimInitialValue = Option.when(epcCtx.weakAccuracyConfigured)(BinaryExpression("\\in", trim, ProcSet))

    // Secondly, we will create state predicates for the initial state of the variables defined inside of locations.
    val locationVariablePredicates = generateLocationVariablesInitStatePredicates()

    // Thirdly, we will create state predicates for the initial states of the defined channels.
    val channelPredicates = generateDefinedChannelsInitStatePredicate()

    // Generate the Init predicate
    val initPredicate = OperatorDefinition(NonFixLHS(Init, Nil), ConjunctionList(
        List(pcInitialValue) ++ raInitialValue ++ OInitialValue ++ FInitialValue ++ trimInitialValue ++ locationVariablePredicates ++ channelPredicates))
    // Add the Init predicate to the TLA builder.
    tlaStringBuilder.appendUnit(initPredicate)
    tlaStringBuilder.appendNewline()
  }

  /**
   * Generates the initial state of the pc variable.
   * pc = [self \in ProcSet -> [ThreadSet[self] -> {"label1", "label2", ... } ] ]
   */
  private def generatePCInitStatePredicate(): Expression = {
    BinaryExpression("=", pc, BoundedMapping(
        List(selfInProcSet),
        CaseExpression(
          // We create a case arm for each location, which maps the IDs of the location to
          // a function of type [ThreadSet[self] -> { ... possible labels } ].
          // Depending on the location enumerator, the condition in the case arm can be of
          // the form: self = 1 or self \in 1..N (and possibly self = "P" or self \in {"P", "Q"}).
          epcCtx.locationThreads.map(
            (locEnum, threadEnums) =>
              CaseArm(
                // This is the case arm condition of the outer function. It has the form (self = 0 -> [...]) or (self |in 1..N -> [...])
                BinaryExpression(locEnum.asInstanceOf[BoundedEnumerator].equalOrIn, self, locEnum.asInstanceOf[BoundedEnumerator].domain),
                // The expression is the new function. So we have a nested bounded mapping.
                // The nested bounded mapping have the following form: [thd \in ThreadSet[self] |-> CASE ... ].
                BoundedMapping(
                  List(QuantifierBound(thd, FunctionApplication(ThreadSet, List(self)))),
                  CaseExpression(
                    threadEnums.map(thdEnum => {
                      CaseArm(
                        // This is the case arm condition in the nested function. It has the form: (thd = 0 -> "label1") or (thd \in 1..N -> "label2").
                        BinaryExpression(thdEnum.asInstanceOf[BoundedEnumerator].equalOrIn, thd, thdEnum.asInstanceOf[BoundedEnumerator].domain),
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
                            case None => DONE
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
    )
  }

  /**
   * Generates state predicates for the initial state of the variables defined inside of locations.
   * Each location variable will have one corresponding TLA variable, which will contain a function
   * that maps location IDs (self \in ProcSet) to the value of the variable at that location.
   */
  private def generateLocationVariablesInitStatePredicates() = {
    for ((variable, locationsBuffer) <- epcCtx.variablesToLocations) yield {
      // First, we create the initial state of the variable, which is mapping [self \in ProcSet |-> CASE ... ]
      val tlaVariableInitialValue = BoundedMapping(
        List(selfInProcSet),
        CaseExpression(
          locationsBuffer.map(
            (locEnum: Enumerator) =>
              CaseArm(BinaryExpression(locEnum.asInstanceOf[BoundedEnumerator].equalOrIn, self, locEnum.asInstanceOf[BoundedEnumerator].domain),
                // The initial value of the variable is available in the locationVariables map in the EPC spec context.
                epcCtx.locationVariables(locEnum).find(_.identifier == variable).get.expression
              )
          ).toList,
          // The default case (the value of the variable at all other locations) is Undefined.
          Some(Undefined)
        )
      )
      BinaryExpression("=", variable, tlaVariableInitialValue)
    }
  }

  /**
   * Generates state predicates for the initial states of the defined channels. Each channel definition
   * (such as "new c"), is translated into a TLA+ variable (with the same name as the channel)
   * and holds the travelling messages, sent on that channel. Since channels will be implemented as Bags of
   * triples << sender, receiver, payload >>, the top-level definition "new c" will lead to the predicate
   * c = EmptyBag. However, since location networks can also be composed with a message aether (which is a
   * multiset of travelling messages), the initial state of those channels can also be changed in that way.
   * To account for these changes, each message eather in the network, that contains messages for a channel,
   * will be added to the initial EmptyBag for that channel with "(+) SetToBag({...})".
   * Implementation details: We have two possibilities for the definition of multi-sets: QuantifiedMultiSets
   * and ExplicitMultiSets. QuantifiedMultiSets do not contain duplicates and thus can be turned directly
   * into a SetToBag({...}) definition and added directly to the initial channel. ExplicitMultiSets might
   * contain several copies of the same message and in that case a single SetToBag({...}) will "delete" the
   * copies, which we don't want. For this reason, ExplicitMultiSets will be mapped to a SetToBag({...}) for
   * each message, which appears in them.
   */
  private def generateDefinedChannelsInitStatePredicate(): List[Expression] = {
    val channelPredicates = mutable.ListBuffer.empty[Expression]
    for (channel <- epcCtx.channels) {
      var channelInitialValue: Expression = emptyBag
      for (messageBag <- epcCtx.messageAether) {
        messageBag match {
          case ExplicitMultiSet(values) =>
            for (message <- values if equalChannels(message.channel, channel)) {
              // Add each message as a SetToBag({...}) to the channel.
              channelInitialValue = extendBagWithSet(channelInitialValue, ExplicitSet(List(Tuple(List(message.source, message.target, message.value)))))
            }
          case QuantifiedMultiSet(enumerator, message) if equalChannels(message.channel, channel) =>
            for (messagesSet <- travellingMessagesToTLASet(enumerator, message)) {
              // Add the whole multiset as a SetToBag({...}) to the channel.
              channelInitialValue = extendBagWithSet(channelInitialValue, messagesSet)
            }
          case _ => // do nothing
        }
      }
      channelPredicates addOne BinaryExpression("=", channel, channelInitialValue)
    }
    channelPredicates.toList
  }

  /**
   * Checks if two channels are equal or if one them has an _ as a suffix, but otherwise equal.
   */
  private def equalChannels(c1: Identifier, c2: Identifier) = c1 == c2 || c1 == Identifier(c2.name + "_") || Identifier(c1.name + "_") == c2

  /**
   * Extends the given bag with the given set using the TLA+ bag union operator (+).
   * E.g. extendBagWithSet(emptyBag, ...) = emptyBag (+) ...
   */
  private def extendBagWithSet(bag: Expression, set: Set): BinaryExpression = BinaryExpression("(+)", bag, IdentifierApplication(SetToBag, List(set)))

  /**
   * Creates a TLA+ set for the quantified multiset of travelling messages.
   */
  private def travellingMessagesToTLASet(enumerator: Enumerator, message: TravellingMessage): Either[String, QuantifiedSetComprehension] =
    // First we turn the enumerator to a quantifier bound. E.g ((j = 1) -> j \in {1}) or ((j \in 1..N) -> j \in 1..N).
    for (quantifierBound <- enumeratorToQuantifierBound(enumerator))
      // The travelling message is implemented as a triple << sender, receiver, payload >>.
      yield QuantifiedSetComprehension( Tuple(List(message.source, message.target, message.value)), List(quantifierBound))

  /**
   * Translates an EPC enumerator to a TLA+ quantifier bound.
   * E.g ((j = 1) -> j \in {1}) or ((j \in 1..N) -> j \in 1..N).
   */
  private def enumeratorToQuantifierBound(enumerator: Enumerator): Either[String, QuantifierBound] = {
    enumerator match {
      case BoundedEnumerator(name, "=", domain) =>
        // Since the enumerator has the form (j = 1), we turn it into j \in { 1 }.
        Right(QuantifierBound(name, ExplicitSet(List(domain))))
      case BoundedEnumerator(name, "\\in", domain) =>
        // Since the enumerator has the form (j \in 1..N), we keep it as it is.
        Right(QuantifierBound(name, domain))
      case BoundedEnumeratorWithFilter(name, "=", domain, filter) =>
        // Since the enumerator has the form (j = 1 : j # 1), we turn it into j \in { j \in { 1 } : j # 1 }.
        Right(
          // This is the outer binder j \in { ... }
          QuantifierBound(name,
            // This is the inner binder j \in { 1 } : j # 1
            // We turn the domain into a set, e.g 1 -> { 1 }.
            // We add the filter to this bounded set comprehension.
            BoundedSetComprehension(name, ExplicitSet(List(domain)), filter)
          )
        )
      case BoundedEnumeratorWithFilter(name, "\\in", domain, filter) =>
        // Since the enumerator has the form (j \in 1..N : j # self), we turn it into j \in { j \in 1..N : j # self }.
        Right(
          // This is the outer binder j \in { ... }
          QuantifierBound(name,
            // This is the inner binder j \in 1..N : j # self
            // We keep the domain expression as it is, e.g 1..N -> 1..N.
            // We add the filter to this bounded set comprehension.
            BoundedSetComprehension(name, domain, filter)
          )
        )
      case _ => Left("Enumerator not known!")
    }
  }

  /**
   * Translates all locations into TLA+ actions.
   */
  private def translateLocations(locationVariables: List[Identifier], tlaVariables: List[Identifier]): List[OperatorDefinition] = {
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
          (operation, operationIdx) <- flattenedRegion.zipWithIndex
          (tlaAction, tlaActionIdx) <- translateOperation(Some(locEnum), Some(thdEnum), None, operation, flattenedRegion, edges, locationVariables, tlaVariables).zipWithIndex
        } {
          if (operationIdx == 0 && tlaActionIdx == 0) {
            // We add a comment before the first TLA action to indicate the beginning of the actions for the current (location, thread)-pair.
            appendInfoComment(locEnum, thdEnum)
            tlaStringBuilder.appendNewline()
          }
          // We add the translated action to the TLA builder.
          tlaStringBuilder.appendUnit(tlaAction)
          tlaStringBuilder.appendNewline()
          // And we store it locally, because we also want to generate the disjunction of all the actions for the (location, thread)-pair.
          generatedThreadActions.addOne(tlaAction)
        }

        // If we were able to generate TLA actions for the operation, we proceed. (Empty send commands are not translated to TLA actions.)
        if (generatedThreadActions.nonEmpty) {
          // Next, we generate the disjunction of all possible thread actions.
          // E.g. p_j(self, thd) == p_j_1A(self, thd) \/ p_j_2R(self, thd) \/ ...
          val threadActionsDisjunctionOperator: OperatorDefinition = generateThreadActionsDisjunction(locEnum, thdEnum, generatedThreadActions.toList)

          // We add the disjunction of the thread actions to the TLA builder.
          tlaStringBuilder.appendUnit(threadActionsDisjunctionOperator)
          tlaStringBuilder.appendNewline()
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
        tlaStringBuilder.appendUnit(locationThreadsDisjunction)
        tlaStringBuilder.appendNewline()
        // And we store it locally, in order to use in the generation of the Next action.
        generatedLocationDisjunctions.addOne(locationThreadsDisjunction)
      }
    }
    generatedLocationDisjunctions.toList
  }

  /**
   * Translates all defined thread into TLA+ actions.
   */
  private def translateThreadDefinitions(locationVariables: List[Identifier], tlaVariables: List[Identifier]): List[OperatorDefinition] = {
    val generatedDefinedThreadDisjunctions = mutable.ListBuffer.empty[OperatorDefinition]
    // We go over every thread definition and start generating the TLA actions for its body.
    for ((thdIdentifier, threadCFG) <- epcCtx.thdDefinitionsToOperations) {
      val (region, edges) = threadCFG
      // We flatten the region, since we no longer need the nested structure.
      val flattenedRegion = region.flatMap(flattenOperation)
      // Here, we will gather the generated actions for the current thread.
      val generatedThreadActions = mutable.ListBuffer.empty[OperatorDefinition]
      // We go over all operations and translate them into TLA actions (some operations can be translated into multiple TLA actions.)
      // Assignments, send actions, and thread instantiations have one corresponding TLA action.
      // Receive actions (in the case of weak accuracy) and conditional threads are translated into two TLA actions.
      for {
        (operation, operationIdx) <- flattenedRegion.zipWithIndex
        (tlaAction, tlaActionIdx) <- translateOperation(None, None, Some(thdIdentifier), operation, flattenedRegion, edges, locationVariables, tlaVariables).zipWithIndex
      } {
        if (operationIdx == 0 && tlaActionIdx == 0) {
          // We add a comment before the first TLA action to indicate the beginning of the actions for the thread definition.
          appendInfoComment(thdIdentifier, epcCtx.thdDefinitionsToFreeVariables(thdIdentifier))
          tlaStringBuilder.appendNewline()
        }
        // We add the translated action to the TLA builder.
        tlaStringBuilder.appendUnit(tlaAction)
        tlaStringBuilder.appendNewline()
        // And we store it locally, because we also want to generate the disjunction of all the actions for the (location, thread)-pair.
        generatedThreadActions.addOne(tlaAction)
      }

      // If we were able to generate TLA actions for the operation, we proceed. (Empty send commands are not translated to TLA actions.)
      if (generatedThreadActions.nonEmpty) {
        // Next, we generate the disjunction of all possible thread actions.
        // E.g. assignXAndStop(self, thd) == assignXAndStop_1A(self, thd) \/ assignXAndStop_2A(self, thd)
        val threadActionsDisjunctionOperator: OperatorDefinition = generateThreadActionsDisjunction(thdIdentifier, generatedThreadActions.toList)

        // We add the disjunction of the thread actions to the TLA builder.
        tlaStringBuilder.appendUnit(threadActionsDisjunctionOperator)
        tlaStringBuilder.appendNewline()
        // And we store it locally, in order to generate a disjunction of all threads for the location
        generatedDefinedThreadDisjunctions.addOne(threadActionsDisjunctionOperator)
      }
    }
    generatedDefinedThreadDisjunctions.toList
  }

  /**
   * Generates the remaining spec operators, such as (Fail), Terminating, Next, Spec, Termination.
   */
  private def generateRemainingSpecOperators(tlaVariables: List[Identifier], locationDisjunctions: List[OperatorDefinition], definedThreadDisjunctions: List[OperatorDefinition]): Unit = {
    // Optionally generate the Fail operator
    val failOption = Option.when (epcCtx.failStopConfigured) (OperatorDefinition(
      NonFixLHS(Fail, Nil),
      BoundedQuantifiedExpression("\\E", List(selfInProcSet), ConjunctionList(Nil
        ++ Option.when(epcCtx.weakAccuracyConfigured)(BinaryExpression("#", self, trim))
        ++ Some(BinaryExpression("=", PrimedExpression(F), BinaryExpression("\\cup", F, ExplicitSet(List(self)))))
        ++ Some(UnchangedExpr(getUnchangedVars(tlaVariables, List(F))))
      ))
    ))
    // Add the Fail operator to the TLA builder.
    failOption.foreach(failDefinition =>
      tlaStringBuilder.appendUnit(failDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the Terminating operator
    val terminatingOption = Option.when (epcCtx.terminatingConfigured) (OperatorDefinition(
      NonFixLHS(Terminating, Nil), ConjunctionList(List(generateTerminationPredicate(), UnaryExpression("UNCHANGED", vars)))
    ))
    terminatingOption.foreach(terminatingDefinition =>
      // Add the Terminating operator to the TLA builder.
      tlaStringBuilder.appendUnit(terminatingDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the Next action
    val nextDefinition = OperatorDefinition(NonFixLHS(Next, Nil),
      foldToDisjunction(locationDisjunctions ++ definedThreadDisjunctions ++ failOption ++ terminatingOption)
    )
    // Add the Next operator to the TLA builder.
    tlaStringBuilder.appendUnit(nextDefinition)
    tlaStringBuilder.appendNewline()

    // Generate the specification Spec
    val specDefinition = OperatorDefinition(
      NonFixLHS(Spec, Nil),
      if (!epcCtx.nextWFConfigured)
        then BinaryExpression("/\\", Init, UnaryExpression("[]", WeakStuttering(Next, vars)))
        else BinaryExpression("/\\",
          BinaryExpression("/\\", Init, UnaryExpression("[]", WeakStuttering(Next, vars))),
          WeakFairness(vars, Next)
        )
    )
    // Add the Spec operator to the TLA builder.
    tlaStringBuilder.appendUnit(specDefinition)
    tlaStringBuilder.appendNewline()

    // Generate the Termination definition
    val terminationOption = Option.when (epcCtx.terminationConfigured) (OperatorDefinition(
      NonFixLHS(Termination, Nil),
      UnaryExpression("<>", ParenthesizedExpression(generateTerminationPredicate()))
    ))
    terminationOption.foreach(terminationDefinition =>
      // Add the Terminating operator to the TLA builder.
      tlaStringBuilder.appendUnit(terminationDefinition)
      tlaStringBuilder.appendNewline()
    )
  }

  /**
   * Method for the generation of the "Termination"-predicate.
   * The predicate does not depend on individual locations or threads!
   *
   * \A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done"
   */
  private def generateTerminationPredicate() =
    BoundedQuantifiedExpression("\\A", List(selfInProcSet), BoundedQuantifiedExpression("\\A", List(thdInThreadSetSelf), BinaryExpression("=", pcThreadValue, Done)))

  /**
   * Create a disjunction of the possible actions of a thread within a location.
   * E.g. p_j(self, thd) == p_j_1A(self, thd) \/ p_j_2R(self, thd) \/ ...
   */
  private def generateThreadActionsDisjunction(locEnum: Enumerator, thdEnum: Enumerator, generatedTlaActions: List[OperatorDefinition]) =
    OperatorDefinition(
      NonFixLHS(Identifier(locEnum.asInstanceOf[BoundedEnumerator].name.name + "_" + thdEnum.asInstanceOf[BoundedEnumerator].name.name), List(self, thd)),
      foldToDisjunction(generatedTlaActions)
    )

  /**
   * Create a disjunction of the possible actions of a defined thread.
   * assignXAndStop == \E self \in ProcSet : \E thd \in ThreadSet[self] : assignXAndStop_1A(self, thd) \/ assignXAndStop_2A(self, thd)
   */
  private def generateThreadActionsDisjunction(thdIdentifier: Identifier, generatedTlaActions: List[OperatorDefinition]) =
    OperatorDefinition(NonFixLHS(Identifier(thdIdentifier.name), Nil),
      BoundedQuantifiedExpression("\\E", List(threadsActionDisjunctionSelfInDomain),
        BoundedQuantifiedExpression("\\E", List(thdInThreadSetSelf), foldToDisjunction(generatedTlaActions))))

  private def threadsActionDisjunctionSelfInDomain = if epcCtx.failStopConfigured then QuantifierBound(self, BinaryExpression("\\", ProcSet, F)) else selfInProcSet

  /**
   * Maps each operator definition (p_j_1A(self, thd) == ...) to an identifier application ( p_j_1A(self, thd) )
   * and then folds them to a disjunction of identifier applications.
   */
  private def foldToDisjunction(operatorDefinitions: List[OperatorDefinition]) = {
    operatorDefinitions.map(operatorDefinition =>
      IdentifierApplication(
        operatorDefinition.signature.identifier,
        // we know that the arguments are the identifiers "self" and "thd"
        operatorDefinition.signature.arguments.asInstanceOf[List[Identifier]]
      )
    ) match {
      case Nil => ???
      case head :: Nil => head
      case head :: next => next.fold(head)((expr1: Expression, expr2: Expression) => BinaryExpression("\\/", expr1, expr2))
    }
  }

  /**
   * Create a disjunction of the possible thread disjunctions (of thread actions) of a location.
   */
  private def generateLocationThreadsDisjunction(locEnum: Enumerator, threadDisjunctions: ListBuffer[OperatorDefinition]) = {
    // Use the identifier of the location as the name of the new operator
    val locationThreadsDisjunctionLabel = locEnum.asInstanceOf[BoundedEnumerator].name.name
    // Create a bounded quantified expression of the form:
    // \E self \in {"P"}: \E thd \in ThreadSet[self]: p_j(self, thd) \/ p_k(self, thd)
    // \E self \in 1..N: \E thd \in ThreadSet[self]: p_j(self, thd) \/ p_k(self, thd)
    val body = BoundedQuantifiedExpression("\\E", List(locationThreadsDisjunctionSelfInDomain(locEnum)),
      BoundedQuantifiedExpression("\\E", List(thdInThreadSetSelf), foldToDisjunction(threadDisjunctions.toList)))
    // Return the new operator definition.
    OperatorDefinition(NonFixLHS(Identifier(locationThreadsDisjunctionLabel), Nil), body)
  }

  private def locationThreadsDisjunctionSelfInDomain(locEnum: Enumerator) =
    val domain = enumeratorDomainToSet(locEnum)
    if epcCtx.failStopConfigured
      then QuantifierBound(self, BinaryExpression("\\", domain, F))
      else QuantifierBound(self, domain)

  /**
   * Flattens an operation recursively.
   */
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
    tlaStringBuilder.appendString("\\* Location ")
    tlaStringBuilder.appendExpression(boundedLocEnum.name)
    tlaStringBuilder.appendString(" ")
    tlaStringBuilder.appendString(boundedLocEnum.equalOrIn)
    tlaStringBuilder.appendString(" ")
    tlaStringBuilder.appendExpression(boundedLocEnum.domain)
    tlaStringBuilder.appendString(", thread ")
    tlaStringBuilder.appendExpression(boundedThdEnum.name)
    tlaStringBuilder.appendString(" ")
    tlaStringBuilder.appendString(boundedThdEnum.equalOrIn)
    tlaStringBuilder.appendString(" ")
    tlaStringBuilder.appendExpression(boundedThdEnum.domain)
  }

  /**
   * Append a comment to the tlaBuilder to indicate the beginning of the actions of a thread definition.
   * E.g. def assignXAndStop(x)
   */
  private def appendInfoComment(thdIdentifier: Identifier, freeVariables: List[Identifier]): Unit = {
    tlaStringBuilder.appendString("\\* def ")
    tlaStringBuilder.appendExpression(thdIdentifier)
    tlaStringBuilder.appendExpressions(freeVariables, "(", ", ", ")")
  }

  private def translateOperation(locEnumOption: Option[Enumerator],
                                 thdEnumOption: Option[Enumerator],
                                 thdIdentifierOption: Option[Identifier],
                                 operation: Operation,
                                 flattenedRegion: Region,
                                 edges: List[DiEdge],
                                 locationVariables: List[Identifier],
                                 allTLAVariables: List[Identifier]): List[OperatorDefinition] = {
    val currentLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, getOperationSuffix(operation))
    val successorOption = getSuccessor(operation, flattenedRegion, edges)
    val successorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, successorOption)
    val pcEnablingCondition = PCEnablingCondition(currentLabel)
    val isThreadExit = thdIdentifierOption.isDefined && successorLabel == DONE
    val pcUpdateUnlessOnThreadExit = Option.unless(isThreadExit) (PCUpdate(successorLabel))
    val pcUpdateOnThreadExit = Option.when(isThreadExit) (PCUpdateOnThreadExit())
    val raOnThreadExit = Option.when (isThreadExit) (ra)
    val raUpdateOnThreadExit = Option.when (isThreadExit) (RAUpdateOnThreadExit())
    operation match {
      case LeafOperation(id, label, command) =>
        command match {
          case Declaration(identifier) =>
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateDeclarationEffect(identifier, locationVariables)
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(identifier, pc, raOnThreadExit))))
              ))
            )
          case Assignment(identifier, expression) =>
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateAssignmentEffect(identifier, expression, locationVariables)
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(identifier, pc, raOnThreadExit))))
              ))
            )
          case Receive(channel, location, variable) =>
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateReceiveEffect(channel, location, variable, locationVariables)
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(variable, channel, pc, raOnThreadExit))))
              ))
            )
            ++ Option.when(epcCtx.weakAccuracyConfigured) ({
              val suspectActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, "Susp")
              OperatorDefinition(NonFixLHS(Identifier(suspectActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateSuspectEffect(location, locationVariables)
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, raOnThreadExit))))
              ))
            })
          case Send(ExplicitMultiSet(Nil)) =>
            // Empty send commands are not translated into TLA actions.
            Nil
          case Send(messages) =>
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateSendEffect(messages, locationVariables, thdIdentifierOption, successorLabel)
                  ++ Some(UnchangedExpr(
                  messages match {
                    case ExplicitMultiSet(Nil) => allTLAVariables
                    case ExplicitMultiSet(message :: Nil) =>
                      getUnchangedVars(allTLAVariables, List(message.channel, pc, raOnThreadExit))
                    case ExplicitMultiSet(messages) =>
                      getUnchangedVars(allTLAVariables, List(messages.head.channel, pc, raOnThreadExit, O))
                    case QuantifiedMultiSet(enumerator, message) =>
                      getUnchangedVars(allTLAVariables, List(message.channel, pc, raOnThreadExit, O))
                  }
                ))
              ))
            )
          case ThreadInstantiation(identifier) =>
            // With thread instantiations, the successor label can only be determined correctly using the thread identifier.
            val definedThreadEntryOperation = getDefinedThreadEntryOperation(epcCtx.thdDefinitionsToOperations(identifier))
            val threadEntryOperationLabel = getSuccessorLabel(identifier, definedThreadEntryOperation)
            val freeVariables = epcCtx.thdDefinitionsToFreeVariables(identifier)
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateThreadInstantiationEffect(freeVariables, locationVariables, successorLabel)
                  ++ Some(PCUpdate(threadEntryOperationLabel))
                  // If there is a successor in the same region, we push that successor on the "return addresses" stack.
                  ++ Option.when(successorLabel != DONE) (ThdLvlExceptUpdate(ra, BinaryExpression("\\o", Tuple(List(Literal(successorLabel))), ThdLvlFnAppl(ra))))
                  ++ Option.when(successorLabel != DONE) (UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, ra))))
                  // If there is no successor in the same region, we don't do anything with the "return addresses" stack.
                  ++ Option.when(successorLabel == DONE) (UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc))))
              ))
            )
          case ConditionalThread(_, _, _) =>
            // This is only to remove the compiler warning, conditional threads are turned into IfOperations in the analysis phase.
            Nil
        }

      case IfOperation(id, label, condition, thenRegion, elseRegion) =>
        val thenActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, "T")
        val elseActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, "F")
        val elseSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "else")
        val thenSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "then")
        val elseSuccessorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, elseSuccessor)
        val thenSuccessorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, thenSuccessor)
        val thenPCUpdateUnlessOnThreadExit = Option.unless(isThreadExit)(PCUpdate(thenSuccessorLabel))
        val elsePCUpdateUnlessOnThreadExit = Option.unless(isThreadExit)(PCUpdate(elseSuccessorLabel))
        List(
          OperatorDefinition(NonFixLHS(Identifier(thenActionLabel), List(self, thd)), ConjunctionList(
            List(pcEnablingCondition)
              ++ generateTruePrecondition(condition, locationVariables)
              ++ thenPCUpdateUnlessOnThreadExit
              ++ pcUpdateOnThreadExit
              ++ raUpdateOnThreadExit
              ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, raOnThreadExit))))
          )),
          OperatorDefinition(NonFixLHS(Identifier(elseActionLabel), List(self, thd)), ConjunctionList(
            List(pcEnablingCondition)
              ++ generateFalsePrecondition(condition, locationVariables)
              ++ elsePCUpdateUnlessOnThreadExit
              ++ pcUpdateOnThreadExit
              ++ raUpdateOnThreadExit
              ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, raOnThreadExit))))
          ))
        )
    }
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

  private def getOperationLabel(locEnumOption: Option[Enumerator],
                                thdEnumOption: Option[Enumerator],
                                threadIdentifierOption: Option[Identifier],
                                operation: Operation,
                                operationSuffix: String): String = {
    if (locEnumOption.isDefined && thdEnumOption.isDefined && threadIdentifierOption.isEmpty) {
      return getOperationLabel(locEnumOption.get, thdEnumOption.get, operation, operationSuffix)
    }
    if (locEnumOption.isEmpty && thdEnumOption.isEmpty && threadIdentifierOption.isDefined) {
      return getOperationLabel(threadIdentifierOption.get, operation, operationSuffix)
    }
    ""
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

  private def getSuccessorLabel(locEnumOption: Option[Enumerator],
                                thdEnumOption: Option[Enumerator],
                                threadIdentifierOption: Option[Identifier],
                                successorOption: Option[Operation]): String = {
    if (locEnumOption.isDefined && thdEnumOption.isDefined && threadIdentifierOption.isEmpty) {
      return getSuccessorLabel(locEnumOption.get, thdEnumOption.get, successorOption)
    }
    if (locEnumOption.isEmpty && thdEnumOption.isEmpty && threadIdentifierOption.isDefined) {
      return getSuccessorLabel(threadIdentifierOption.get, successorOption)
    }
    ""
  }

  private def getSuccessorLabel(locEnum: Enumerator, thdEnum: Enumerator, successorOption: Option[Operation]): String =
    successorOption match {
      case Some(successorOperation) => getOperationLabel(locEnum, thdEnum, successorOperation, getOperationSuffix(successorOperation))
      case None => DONE
    }

  private def getSuccessorLabel(thdIdentifier: Identifier, successorOption: Option[Operation]): String =
    successorOption match {
      case Some(successorOperation) => getOperationLabel(thdIdentifier, successorOperation, getOperationSuffix(successorOperation))
      case None => DONE
    }

  /**
   * Generate the enabling condition for the program counter
   * E.g. pc[self][thd] = "..."
   */
  private def PCEnablingCondition(label: String): Expression = BinaryExpression("=", ThdLvlFnAppl(Identifier("pc")), Literal(label))

  /**
   * Generate the update for the program counter.
   * E.g. pc' = [pc EXCEPT ![self][thd] = "successorLabel"]
   */
  private def PCUpdate(successorLabel: String): Expression = ThdLvlExceptUpdate(pc, Literal(successorLabel))

  /**
   * Generate the update for the program counter for an exiting operation of a defined thread.
   * E.g. pc' = [pc EXCEPT ![self][thd] = IF ra[self][thd] # << >> THEN Head(ra[self][thd]) ELSE "Done"]
   */
  private def PCUpdateOnThreadExit(): Expression = ThdLvlExceptUpdate(pc,
    IfThenElse(
      BinaryExpression("#", ThdLvlFnAppl(ra), Tuple(Nil)),
      IdentifierApplication(Identifier("Head"), List(ThdLvlFnAppl(ra))),
      Done
    ))

  /**
   * Conditionally generates an update that decreases the "return addresses" stack for a (location, thread)-pair.
   * E.g. ra' = [ra EXCEPT ![self][thd] = IF ra[self][thd] # <<>>
   *                                          THEN Tail(ra[self][thd])
   *                                          ELSE ra[self][thd] ]
   */
  private def RAUpdateOnThreadExit(): Expression =
    ThdLvlExceptUpdate(Identifier("ra"),
      IfThenElse(
        BinaryExpression("#", ThdLvlFnAppl(Identifier("ra")), Tuple(Nil)),
        IdentifierApplication(Identifier("Tail"), List(ThdLvlFnAppl(Identifier("ra")))),
        ThdLvlFnAppl(Identifier("ra"))
      )
    )

  private def ThdLvlExceptUpdate(fnName: Identifier, newThdValue: Expression): Expression =
    BinaryExpression("=",
      PrimedExpression(fnName),
      ExceptExpression(fnName,
        List(ExceptClause(SelectorPath(List(IndexSelector(List(self)),IndexSelector(List(thd)))), newThdValue))
      )
    )

  private def getSuccessor(operation: Operation, containingRegion: Region, edges: List[DiEdge]): Option[Operation] =
    edges.find(_.start == operation.id).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

  private def getSuccessorByEdgeLabel(operation: Operation, containingRegion: Region, edges: List[DiEdge], edgeLabel: String): Option[Operation] =
    edges.find(edge => edge.start == operation.id && edge.label.contains(edgeLabel)).flatMap(edge => getEdgeEndOperation(edge, containingRegion))

  private def getEdgeEndOperation(edge: DiEdge, containingRegion: Region): Option[Operation] =
    edge match {
      case DiEdge(start, -1, None) => None // there is no successor operation in this case => (label "Done")
      case DiEdge(start, end, _) => containingRegion.find(_.id == end) // We simply look for the successor operation in the flattened region.
    }

  /**
   * This is function can be used to determine the entry operation of a defined thread.
   * This is useful when we have a thread instantiation and the program counter should
   * move from the current action to the first action of the instantiated thread.
   * In order to find the successor operation, we access the CFG of the thread, and we
   * call the getEdgeEndOperation()-method on the first edge from its edge list (which
   * is exactly the entry edge).
   * Important: The operations of the thread are not flattened, but this is not a problem,
   * because there is always an entry edge which points the correct entry operation!
   */
  private def getDefinedThreadEntryOperation(definedThreadCFG: CFG): Option[Operation] =
    val threadOperations = definedThreadCFG._1
    val threadEdges = definedThreadCFG._2
    threadEdges.headOption.flatMap(entryEdge => getEdgeEndOperation(entryEdge, threadOperations))

  private def UnchangedExpr(unchangedVariables: List[Identifier]): Expression = {
    // Generate the unchanged variables conjunct.
    // E.g. UNCHANGED << x, y, ... >>
    UnaryExpression("UNCHANGED", IdentifierTuple(unchangedVariables))
  }

  private def getUnchangedVars(variables: List[Identifier], changedVars: List[Expression | Option[Expression]]) = {
    val changedVarsSet = changedVars.filter(_ != None).map {
      case Some(variable) => variable
      case variable => variable
    }.toSet
    variables.filterNot(variable => changedVarsSet.contains(variable))
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
      PrimedExpression(identifier),
      ExceptExpression(
        identifier,
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
      PrimedExpression(identifier),
      ExceptExpression(
        identifier,
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
    val sourceExpr = EPCtoTLAExpression.rewrite(locationExpr, locationVariables)
    val variableExpr = EPCtoTLAExpression.rewrite(variable, locationVariables)
    // We require that the location expression can be evaluation to a valid location (is element of ProcSet).
    val sourceExprIsInProcSet = BinaryExpression("\\in", sourceExpr, Identifier("ProcSet"))
    // We require that the variable in the "receive" action is not Undefined at the location. E.g. x[self] # Undefined.
    val variableIsNotUndefined = BinaryExpression("#", variableExpr, Identifier("Undefined"))
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
    List(sourceExprIsInProcSet, variableIsNotUndefined, messageHandlingEffect)
  }

  private def generateSuspectEffect(location: Expression, locationVariables: List[Identifier]): List[Expression] = {
    val sourceExpr = EPCtoTLAExpression.rewrite(location, locationVariables)
    // We require that the source location in the "receive" action is a process.
    // E.g. <location-expression> \in ProcSet
    val sourceIsInProcSet = BinaryExpression("\\in", sourceExpr, Identifier("ProcSet"))
    // And that it is not the trusted immortal
    val sourceIsNotTrustedImmortal = BinaryExpression("#", sourceExpr, Identifier(TRUSTED_IMMORTAL))
    List(sourceIsInProcSet, sourceIsNotTrustedImmortal)
  }

  private def generateSendEffect(messages: MultiSet[OutgoingMessage],
                                 locationVariables: List[Identifier],
                                 thdIdentifierOption: Option[Identifier],
                                 successorLabel: String): List[Expression] =
  {
    val isThreadExit = thdIdentifierOption.isDefined && successorLabel == "Done"
    val pcUpdateUnlessOnThreadExit = Option.unless(isThreadExit)(PCUpdate(successorLabel))
    val pcUpdateOnThreadExit = Option.when(isThreadExit)(PCUpdateOnThreadExit())
    val raUpdateOnThreadExit = Option.when(isThreadExit)(RAUpdateOnThreadExit())
    messages match {
      case ExplicitMultiSet(Nil) =>
        // Empty send commands are filtered before we reach this code, so we do nothing!
        Nil
      case ExplicitMultiSet(singleMsg :: Nil) =>
        // Sending a single message means that we send it directly, so we simply need to update the channel!
        val channelUpdate = generateChannelUpdateForSingleSend(singleMsg, locationVariables)
        val pcUpdate = PCUpdate(successorLabel)
        List(channelUpdate)
          ++ pcUpdateUnlessOnThreadExit
          ++ pcUpdateOnThreadExit
          ++ raUpdateOnThreadExit
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
        val raUpdateOnThreadExitForMultiSend = Option.when(isThreadExit)(generateRAUpdateForMultiSend(indexBinder, allIndices))
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(indexBinder, remainingIndices)), ConjunctionList(
            List(
              generateChannelUpdateForMultiSend(indexBinder, multipleMessages, locationVariables),
              generatePCUpdateForMultiSend(indexBinder, allIndices, thdIdentifierOption, successorLabel))
              ++ raUpdateOnThreadExitForMultiSend
              ++ Some(generateOUpdateForMultiSend(indexBinder, allIndices))
          ))
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
        val raUpdateOnThreadExitForMultiSend = Option.when(isThreadExit)(generateRAUpdateForMultiSend(boundVariable, indexSet))
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(boundVariable, remainingIndices)), ConjunctionList(
            List(
              generateChannelUpdateForSingleSend(message, locationVariables),
              generatePCUpdateForMultiSend(boundVariable, indexSet, thdIdentifierOption, successorLabel),
            )
              ++ raUpdateOnThreadExitForMultiSend
              ++ Some(generateOUpdateForMultiSend(boundVariable, indexSet))
          ))
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
                ExplicitSet(List(
                  Tuple(List(
                    Identifier("self"),
                    EPCtoTLAExpression.rewrite(message.target, locationVariables),
                    EPCtoTLAExpression.rewrite(message.value, locationVariables)
                  ))
                ))
              ))
            )
          )
        },
        None
      )
    )
  }

  private def generatePCUpdateForMultiSend(binder: Identifier,
                                           domain: Expression,
                                           thdIdentifierOption: Option[Identifier],
                                           successorLabel: String) =
  {
    val isThreadExit = thdIdentifierOption.isDefined && successorLabel == "Done"
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
            // IF O[self][thd] \cup { i } # <domain> THEN pc[self][thd] ELSE <successorLabel>
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
              // THEN pc[self][thd]
              ThdLvlFnAppl(pc),
              // ELSE <successorLabel>
              if (isThreadExit) {
                IfThenElse(
                  BinaryExpression("#", ThdLvlFnAppl(Identifier("ra")), Tuple(Nil)),
                  IdentifierApplication(Identifier("Head"), List(ThdLvlFnAppl(Identifier("ra")))),
                  Literal("Done")
                )
              } else {
                Literal(successorLabel)
              }
            )
          )
        )
      )
    )
  }

  /**
   * ra' = [ra EXCEPT ![self][thd] = IF O[self][thd] \cup { i } # { ...domain }
   *                                    THEN ra[self][thd]
   *                                    ELSE IF ra[self][thd] # << >>
   *                                        THEN Tail(ra[self][thd])
   *                                        ELSE ra[self][thd] ]
   */
  private def generateRAUpdateForMultiSend(binder: Identifier, domain: Expression) =
    ThdLvlExceptUpdate(Identifier("ra"),
      // IF O[self][thd] \cup { i } # <domain> THEN @ ELSE Tail(ra[self][thd])
      IfThenElse(
        BinaryExpression("#", BinaryExpression("\\cup", ThdLvlFnAppl(Identifier("O")), ExplicitSet(List(binder))), domain ),
        ThdLvlFnAppl(Identifier("ra")),
        IfThenElse(
          BinaryExpression("#", ThdLvlFnAppl(Identifier("ra")), Tuple(Nil)),
          IdentifierApplication(Identifier("Tail"), List(ThdLvlFnAppl(Identifier("ra")))),
          ThdLvlFnAppl(Identifier("ra"))
        )
      )
    )

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
            // IF O[self][thd] \cup { i } # <domain>
            IfThenElse(
              BinaryExpression(
                "#",
                BinaryExpression("\\cup", ThdLvlFnAppl(O), ExplicitSet(List(binder))),
                domain
              ),
              // THEN O[self][thd] \cup { i }
              BinaryExpression("\\cup", ThdLvlFnAppl(O), ExplicitSet(List(binder))),
              // ELSE {}
              ExplicitSet(Nil)
            )
          )
        )
      )
    )
  }

  private def generateTruePrecondition(condition: Expression, locationVariables: List[Identifier]): List[Expression] =
    List( EPCtoTLAExpression.rewrite(condition, locationVariables) )

  private def generateFalsePrecondition(condition: Expression, locationVariables: List[Identifier]): List[Expression] =
    List( UnaryExpression("~", ParenthesizedExpression( EPCtoTLAExpression.rewrite(condition, locationVariables) )) )

  private def generateThreadInstantiationEffect(freeVariables: List[Identifier], locationVariables: List[Identifier], successorLabel: String): List[Expression] = {
    // We require that the free variables from the thread definition are not Undefined at the location.
    // E.g. x[self] # Undefined, y[self] # Undefined
    for identifier <- freeVariables yield { BinaryExpression("#", EPCtoTLAExpression.rewrite(identifier, locationVariables), Identifier("Undefined")) }
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
