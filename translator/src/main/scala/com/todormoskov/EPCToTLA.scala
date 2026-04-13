package com.todormoskov

import com.todormoskov.Lexer.{Token, TokenKind}

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

class EPCToTLA(epcCtx: EPCSpecContext) {
  private val tlaStringBuilder = TLAStringBuilder()
  // The knownIdentifiers set is used to remember all identifiers, which we have already seen
  // (or we have created on our own), in order to create new, unique identifiers.
  private val knownIdentifiers = mutable.LinkedHashSet.empty[Identifier]

  // Constant names
  private val CONSTANTS = "CONSTANTS"
  private val VARIABLES = "VARIABLES"
  private val UNDEFINED = "Undefined"
  private val INITIALIZED = "Initialized"
  private val PROC_SET = "ProcSet"
  private val THREAD_SET = "ThreadSet"
  private val CHAN_SET = "ChanSet"
  private val SUSP_CHAN_SET = "SuspChanSet"
  private val SELF = "self"
  private val THD = "thd"
  private val CHAN = "_chan"
  private val SUSPCHAN = "_suspchan"
  private val SUSPECT_OK = "suspectOK"
  private val SUSPECTED = "suspected"
  private val SUSPECTOR = "suspector"
  private val CURRENT_TI = "currentTI"
  private val CURRENT_F = "currentF"
  private val TRUSTED_IMMORTAL_SET = "TI"
  private val NEW_TRUSTED_IMMORTAL_SET = "ti"
  private val FAILED_SET = "F"
  private val NEW_FAILED_SET = "f"
  private val _AE = "AE"
  private val _O = "O"
  private val _D = "D"
  private val PC = "pc"
  private val RA = "ra"
  private val DONE = "Done"
  private val VARS = "vars"
  private val INIT = "Init"
  private val ENV = "Env"
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
  private val ChanSet = Identifier(CHAN_SET)
  private val SuspChanSet = Identifier(SUSP_CHAN_SET)
  private val self = Identifier(SELF)
  private val thd = Identifier(THD)
  private val chan = Identifier(CHAN)
  private val suspchan = Identifier(SUSPCHAN)
  private val suspectOK = Identifier(SUSPECT_OK)
  private val suspected = Identifier(SUSPECTED)
  private val suspector = Identifier(SUSPECTOR)
  private val currentTI = Identifier(CURRENT_TI)
  private val currentF = Identifier(CURRENT_F)
  private val TI = Identifier(TRUSTED_IMMORTAL_SET)
  private val ti = Identifier(NEW_TRUSTED_IMMORTAL_SET)
  private val F = Identifier(FAILED_SET)
  private val f = Identifier(NEW_FAILED_SET)
  private val AE = Identifier(_AE)
  private val O = Identifier(_O)
  private val D = Identifier(_D)
  private val pc = Identifier(PC)
  private val ra = Identifier(RA)
  private val Done = Literal(DONE)
  private val vars = Identifier(VARS)
  private val Init = Identifier(INIT)
  private val Env = Identifier(ENV)
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
  private val chanInChanSet = QuantifierBound(chan, ChanSet)
  private val suspchanInSuspChanSet = QuantifierBound(suspchan, SuspChanSet)
  private val pcThreadValue = ThdLvlFnAppl(pc)

  // Helper methods
  private def ThdLvlFnAppl(identifier: Identifier): Expression =
    FunctionApplication(FunctionApplication(identifier, List(self)), List(thd))

  /**
   * Entry method for the translation of the EPC spec to TLA+.
   */
  def translateSpec(): TLAStringBuilder = {
    // We check if there are defined channels, if yes, we need to generate the channel names as constants!
    val declaredChannels = epcCtx.channels.toList
    val declaredSuspiciousChannels = epcCtx.suspiciousChannels.toList

    // We add the constants Undefined and Initialized
    // Besides these values, all channels are also translated as constants
    val constants = ConstantDeclaration(Token(TokenKind.CONSTANTS, CONSTANTS, -1, -1),
      List(OperatorDeclaration(Undefined, Nil), OperatorDeclaration(Initialized, Nil))
      ++ declaredChannels.map(channel => OperatorDeclaration(channel, Nil))
    )
    tlaStringBuilder.appendUnit(constants, newlines = 2)

    // We generate the variable declarations. This includes the declaration of all variables
    // which appear in the memory of a location or are declared with the 'var' keyword.
    // Additionally, we create a variable for the program counters (pc) of the different locations
    // and their threads. Finally, we create variables for each channel, with the same name as the channel.
    val TIOption = Option.when(epcCtx.unreliableFailureDetection) (TI)
    val FOption = Option.when(epcCtx.failStopConfigured) (F)
    val AEOption = Option.when(declaredChannels.nonEmpty)(AE)
    val OOption = Option.when(epcCtx.specWithMultiSends)(O)
    val DOption = Option.when(declaredSuspiciousChannels.nonEmpty)(D)
    val raOption = Option.when(epcCtx.thdDefinitionsToOperations.nonEmpty)(ra)
    val locationVariables = epcCtx.variablesToLocations.keys.toList
    val locationVariablesWithImplicitVars = locationVariables ++ DOption
    val allTLAVariables = List() ++ TIOption ++ FOption ++ AEOption ++ OOption ++ DOption ++ List(pc) ++ raOption ++ locationVariables
    val variablesDeclaration = VariableDeclaration(Token(TokenKind.VARIABLES, VARIABLES, -1, -1), allTLAVariables)
    tlaStringBuilder.appendUnit(variablesDeclaration, newlines = 2)
    // Add all TLA variables to the set with known identifiers.
    allTLAVariables.foreach(knownIdentifiers.addOne)

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

    // Add the ChanSet operator when there are defined channels
    // ChanSet == {c1, c2, ...}
    val chanSetOption = Option.when(declaredChannels.nonEmpty)(
      OperatorDefinition(NonFixLHS(ChanSet, Nil), ExplicitSet(declaredChannels))
    )
    chanSetOption.foreach(chanSetDefinition =>
      tlaStringBuilder.appendUnit(chanSetDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Add the SuspChanSet operator when there are defined suspicious channels
    // SuspChanSet == {c1, c2, ...}
    val suspChanSetOption = Option.when(declaredSuspiciousChannels.nonEmpty)(
      OperatorDefinition(NonFixLHS(SuspChanSet, Nil), ExplicitSet(declaredSuspiciousChannels))
    )
    suspChanSetOption.foreach(suspChanSetDefinition =>
      tlaStringBuilder.appendUnit(suspChanSetDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the suspectOK predicate when "WeakAccuracy" is configured
    val suspectOKOnWeakAccuracy = Option.when(epcCtx.weakAccuracyConfigured)(
      OperatorDefinition(
        NonFixLHS(suspectOK, List(ti, f, suspected, suspector)),
        BinaryExpression("/\\", BinaryExpression("#", ti, emptySet), BinaryExpression("\\notin", suspected, ti))
      )
    )
    suspectOKOnWeakAccuracy.foreach(suspectOKDefinition =>
      tlaStringBuilder.appendUnit(suspectOKDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the suspectOK predicate when "EventualWeakAccuracy" is configured
    val suspectOKOnEventualWeakAccuracy = Option.when(epcCtx.eventualWeakAccuracyConfigured)(
      OperatorDefinition(NonFixLHS(suspectOK, List(ti, f, suspected, suspector)), BinaryExpression("\\notin", suspected, ti))
    )
    suspectOKOnEventualWeakAccuracy.foreach(suspectOKDefinition =>
      tlaStringBuilder.appendUnit(suspectOKDefinition)
      tlaStringBuilder.appendNewline()
    )

    generateInitStatePredicate()
    val locationOperators = translateLocations(locationVariablesWithImplicitVars, allTLAVariables)
    val threadDefinitionOperators = translateThreadDefinitions(locationVariablesWithImplicitVars, allTLAVariables)
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
    // TI is variable that contains the "Trusted Immortal" locations and is used when then WeakAccuracy modifier is configured.
    val TIInitialValue = Option.when(epcCtx.unreliableFailureDetection)(BinaryExpression("=", TI, emptySet))

    // F is a variable that contains failed processes and is used only when the FailStop modifier is configured.
    val FInitialValue = Option.when(epcCtx.failStopConfigured)(BinaryExpression("=", F, emptySet))

    // AE is a variable that represents the message aether and is used when there are declared channels
    val aeInitialValue = Option.when(epcCtx.channels.nonEmpty)(generateMessageAetherInitStatePredicate())

    // O variable is a function that maps processes and its threads to sets of indices (initially empty).
    val OInitialValue = Option.when(epcCtx.specWithMultiSends)
      (BinaryExpression("=", O, BoundedMapping(List(selfInProcSet), BoundedMapping(List(thdInThreadSetSelf), emptySet))))

    // D variable is a function that maps each process to a funtion that maps
    // suspicious channels to a function that map all locations to a boolean value
    // This means that it has the type: ProcSet -> (SuspChanSet -> (ProcSet -> Boolean))
    // It has the following initial value: [self \in ProcSet |-> [_suspchan \in SuspChanSet |-> [p \in ProcSet |-> TRUE]]]
    val DInitialValue = Option.when(epcCtx.suspiciousChannels.nonEmpty)
      (BinaryExpression("=", D, BoundedMapping(List(selfInProcSet), BoundedMapping(List(suspchanInSuspChanSet),
        BoundedMapping(List(QuantifierBound(Identifier("_p"), ProcSet)), Literal(true))
      ))))

    // pc is a variable holds the program counters of all (location, thread)-pairs
    val pcInitialValue = generatePCInitStatePredicate()

    // ra variable is a function that maps processes and its threads to lists of return addresses (initially empty)
    val raInitialValue = Option.when(epcCtx.thdDefinitionsToOperations.nonEmpty)
      (BinaryExpression("=", ra, BoundedMapping(List(selfInProcSet), BoundedMapping(List(thdInThreadSetSelf), emptyTuple))))

    // Lastly, we will create state predicates for the initial state of the variables defined inside of locations.
    val locationVariablePredicates = generateLocationVariablesInitStatePredicates()

    // Generate the Init predicate
    val initPredicate = OperatorDefinition(NonFixLHS(Init, Nil), ConjunctionList(Nil
          ++ TIInitialValue ++ FInitialValue ++ aeInitialValue ++ OInitialValue ++ DInitialValue
          ++ Option(pcInitialValue) ++ raInitialValue ++ locationVariablePredicates))
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
                            case Some(operation) => getOperationLabel(Some(locEnum), Some(thdEnum), None, operation, None)
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
   * Generates state predicates for the initial states of the defined channels. Each channel declaration
   * (e.g. "chan c1", "suspchan c2"), is translated as a TLA+ constant (with the same name as the channel).
   * The message aether is represented as a TLA+ variable AE that maps each channel to a bag/multiset
   * of the travelling messages, sent on that channel. Each travelling message in these bags is triple of
   * the form << sender, receiver, payload >>. A simple top-level definition "chan c" will lead to the initial
   * mapping "c = EmptyBag". However, since location networks can also be composed with a message aether (which
   * is a multiset of travelling messages), the initial state of those channels can also be changed in that way.
   * To account for these changes, each message eather in the network, that contains messages for a channel,
   * will be added to the initial EmptyBag for that channel with "(+) SetToBag({...})".
   * Implementation details: We have two possibilities for the definition of multi-sets: QuantifiedMultiSets
   * and ExplicitMultiSets. QuantifiedMultiSets do not contain duplicates and thus can be turned directly
   * into a SetToBag({...}) definition and added directly to the initial channel. ExplicitMultiSets might
   * contain several copies of the same message and in that case a single SetToBag({...}) will "delete" the
   * copies, which we don't want. For this reason, ExplicitMultiSets will be mapped to a SetToBag({...}) for
   * each message, which appears in them.
   */
  private def generateMessageAetherInitStatePredicate(): Expression = {
    // We calculate the initial bags for each channel
    val initialChannelBags = mutable.ListBuffer.empty[(Identifier, Expression)]
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
      initialChannelBags addOne (channel, channelInitialValue)
    }

    // We create the predicate with the initial state of the AE variable
    // This will have the form AE = [_chan \in ChanSet |-> CASE _chan = c1 -> ...
    //                                                     []   _chan = c2 -> ...]
    val aeInitialValue = BoundedMapping(List(chanInChanSet), CaseExpression(
        // We create all case arms "_chan = c1 -> ...",
        initialChannelBags.map((channel, initialBag) => CaseArm(BinaryExpression("=", chan, channel), initialBag)).toList,
        // We map all channels, so there is no need for a default case
        None
      )
    )
    BinaryExpression("=", AE, aeInitialValue)
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
        // Conditional threads are translated into two TLA actions.
        // Receive actions are translated into three TLA actions, when there are suspicious channels in the specification.
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
      // Conditional threads are translated into two TLA actions.
      // Receive actions are translated into three TLA actions, when there are suspicious channels in the specification.
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
    val lowerCaseTI = getUniqueIdentifier(ti)
    val lowerCaseF = getUniqueIdentifier(f)
    // "Env" operator when only fail-stop is configured
    val envOnFailStop = Option.when (epcCtx.failStopConfigured && !epcCtx.unreliableFailureDetection) (OperatorDefinition(
      NonFixLHS(Env, Nil),
      // \E f \in SUBSET (ProcSet \ F):
      //    /\ failOK(F, f)
      //    /\ F' = F \cup f
      //    /\ UNCHANGED ...
      BoundedQuantifiedExpression("\\E", List(QuantifierBound(lowerCaseF,
        UnaryExpression("SUBSET", ParenthesizedExpression(BinaryExpression("\\", ProcSet, F)))
      )), ConjunctionList(Nil
        ++ Option.when(epcCtx.visibleTLAOperators.contains("failOK"))(IdentifierApplication(Identifier("failOK"), List(F, lowerCaseF)))
        ++ Some(BinaryExpression("=", PrimedExpression(F), BinaryExpression("\\cup", F, lowerCaseF)))
        ++ Some(UnchangedExpr(getUnchangedVars(tlaVariables, List(F))))
      ))
    ))
    // Add the Fail operator to the TLA builder.
    envOnFailStop.foreach(failDefinition =>
      tlaStringBuilder.appendUnit(failDefinition)
      tlaStringBuilder.appendNewline()
    )

    // "Env" operator when fail-stop and (eventual) weak accuracy are configured
    val envOption = Option.when(epcCtx.failStopConfigured && epcCtx.unreliableFailureDetection)(OperatorDefinition(
      NonFixLHS(Env, Nil),
      // \E ti, f \in SUBSET (ProcSet \ (TI \cup F)):
      //    /\ ti \cap f # {}
      //    /\ failOK(F, f)
      //    /\ TI' = TI \cup ti
      //    /\ F' = F \cup f
      //    /\ UNCHANGED ...
      BoundedQuantifiedExpression("\\E", List(QuantifierBound(List(lowerCaseTI, lowerCaseF),
        UnaryExpression("SUBSET", ParenthesizedExpression(
          BinaryExpression("\\", ProcSet, ParenthesizedExpression(BinaryExpression("\\cup", TI, F)))
        ))
      )), ConjunctionList(Nil
        ++ Some(BinaryExpression("=", BinaryExpression("\\cap", lowerCaseTI, lowerCaseF), emptySet))
        ++ Option.when(epcCtx.visibleTLAOperators.contains("failOK"))(IdentifierApplication(Identifier("failOK"), List(F, lowerCaseF)))
        ++ Some(BinaryExpression("=", PrimedExpression(TI), BinaryExpression("\\cup", TI, lowerCaseTI)))
        ++ Some(BinaryExpression("=", PrimedExpression(F), BinaryExpression("\\cup", F, lowerCaseF)))
        ++ Some(UnchangedExpr(getUnchangedVars(tlaVariables, List(TI, F))))
      ))
    ))
    // Add the Fail operator to the TLA builder.
    envOption.foreach(failDefinition =>
      tlaStringBuilder.appendUnit(failDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the Terminating operator
    val terminatingOption = Option.when (epcCtx.terminatingConfigured) (OperatorDefinition(
      NonFixLHS(Terminating, Nil), ConjunctionList(List(generateTerminationPredicate(), UnaryExpression("UNCHANGED", vars)))
    ))
    // Add the Terminating operator to the TLA builder.
    terminatingOption.foreach(terminatingDefinition =>
      tlaStringBuilder.appendUnit(terminatingDefinition)
      tlaStringBuilder.appendNewline()
    )

    // Generate the Next action
    val nextActions = locationDisjunctions ++ definedThreadDisjunctions ++ envOnFailStop ++ envOption ++ terminatingOption
    val nextOperatorOption = Option.when(nextActions.nonEmpty)(OperatorDefinition(NonFixLHS(Next, Nil), foldToDisjunction(nextActions)))
    // Add the Next operator to the TLA builder.
    nextOperatorOption.foreach(nextOperator =>
      tlaStringBuilder.appendUnit(nextOperator)
      tlaStringBuilder.appendNewline()
    )

    // Generate the specification Spec
    val specOperatorOption = nextOperatorOption.map(_ =>
      OperatorDefinition(
        NonFixLHS(Spec, Nil),
        if !epcCtx.nextWFConfigured
        then BinaryExpression("/\\", Init, UnaryExpression("[]", WeakStuttering(Next, vars)))
        else BinaryExpression("/\\",
          BinaryExpression("/\\", Init, UnaryExpression("[]", WeakStuttering(Next, vars))),
          WeakFairness(vars, Next)
        )
      )
    )
    // Add the Spec operator to the TLA builder.
    specOperatorOption.foreach(specOperator =>
      tlaStringBuilder.appendUnit(specOperator)
      tlaStringBuilder.appendNewline()
    )

    // Generate the Termination definition
    val terminationOption = Option.when (epcCtx.terminationConfigured) (OperatorDefinition(
      NonFixLHS(Termination, Nil),
      UnaryExpression("<>", ParenthesizedExpression(generateTerminationPredicate()))
    ))
    // Add the Terminating operator to the TLA builder.
    terminationOption.foreach(terminationDefinition =>
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
    BoundedQuantifiedExpression("\\A", List(selfInProcSetMaybeWithoutF), BoundedQuantifiedExpression("\\A", List(thdInThreadSetSelf), BinaryExpression("=", pcThreadValue, Done)))

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
      BoundedQuantifiedExpression("\\E", List(selfInProcSetMaybeWithoutF),
        BoundedQuantifiedExpression("\\E", List(thdInThreadSetSelf), foldToDisjunction(generatedTlaActions))))

  private def selfInProcSetMaybeWithoutF = if epcCtx.failStopConfigured then QuantifierBound(self, BinaryExpression("\\", ProcSet, F)) else selfInProcSet

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
      case Nil => ??? // as long as the function is called with a non-empty list of operator definitions, it should be okay
      case head :: Nil => head
      case list: List[IdentifierApplication] => DisjunctionList(list)
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
    case ChoiceOperation(id, label, choices) => List(operation)
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
                                 allTLAVariables: List[Identifier],
                                 secondaryActionID: Option[Int] = None): List[OperatorDefinition] = {
    val currentLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, None)
    val pcEnablingCondition = PCEnablingCondition(currentLabel)
    operation match {
      case LeafOperation(id, label, command) =>
        // Calculate the successor of the current operation in the current region.
        val successorOption = getSuccessor(operation, flattenedRegion, edges)
        val successorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, successorOption)
        val isThreadExit = thdIdentifierOption.isDefined && successorLabel == DONE
        val pcUpdateUnlessOnThreadExit = Option.unless(isThreadExit) (PCUpdate(successorLabel))
        val pcUpdateOnThreadExit = Option.when(isThreadExit) (PCUpdateOnThreadExit())
        val raUpdateOnThreadExit = Option.when (isThreadExit) (RAUpdateOnThreadExit())
        val raOnThreadExit = Option.when (isThreadExit) (ra)
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
            // Receive actions can be part of a choice of multiple alternatives.
            // For each alternative, we are creating TLA+ actions as if the other alternatives did not exist.
            // However, these actions (TLA+ operators) need to have unique names ==> We need secondary action IDs.
            // If such an ID is provided (currently only in the case of choices of multiple receives),
            // we append the secondary ID to the generated label, before the rule suffix.
            // E.g. rcv: c@1?(y) + c@2?(y) will be generated into two TLA+ actions: rcv_1Rcv and rcv_2Rcv (if c is a "normal" channel)
            val secondarySuffix = secondaryActionID.map("_" + _.toString).getOrElse("")
            val rcvActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some(secondarySuffix + "Rcv"))
            List(
              OperatorDefinition(NonFixLHS(Identifier(rcvActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateReceiveEffect(channel, location, variable, locationVariables, "Rcv")
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(AE, pc, raOnThreadExit, variable))))
              ))
            )
            ++ Option.when(epcCtx.suspiciousChannels.nonEmpty) ({
              val rcvSActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some(secondarySuffix + "RcvS"))
              OperatorDefinition(NonFixLHS(Identifier(rcvSActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateReceiveEffect(channel, location, variable, locationVariables, "RcvS")
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(AE, D, pc, raOnThreadExit, variable))))
              ))
            })
            ++ Option.when(epcCtx.suspiciousChannels.nonEmpty) ({
              val suspectActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some(secondarySuffix + "Susp"))
              OperatorDefinition(NonFixLHS(Identifier(suspectActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateSuspectEffect(channel, location, locationVariables)
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(D, pc, raOnThreadExit))))
              ))
            })
          case IndexedSumChoice(enumerator, Receive(channel, location, variable)) =>
            val rcvActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some("Rcv"))
            List(
              OperatorDefinition(NonFixLHS(Identifier(rcvActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateReceiveEffect(channel, location, variable, locationVariables, "Rcv", Some(enumerator))
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(AE, pc, raOnThreadExit, variable))))
              ))
            )
            ++ Option.when(epcCtx.suspiciousChannels.nonEmpty) ({
              val rcvSActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some("RcvS"))
              OperatorDefinition(NonFixLHS(Identifier(rcvSActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateReceiveEffect(channel, location, variable, locationVariables, "RcvS", Some(enumerator))
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(AE, D, pc, raOnThreadExit, variable))))
              ))
            })
            ++ Option.when(epcCtx.suspiciousChannels.nonEmpty) ({
              val suspectActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some("Susp"))
              OperatorDefinition(NonFixLHS(Identifier(suspectActionLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateSuspectEffect(channel, location, locationVariables, Some(enumerator))
                  ++ pcUpdateUnlessOnThreadExit
                  ++ pcUpdateOnThreadExit
                  ++ raUpdateOnThreadExit
                  ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(D, pc, raOnThreadExit))))
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
                      getUnchangedVars(allTLAVariables, List(AE, pc, raOnThreadExit))
                    case ExplicitMultiSet(messages) =>
                      getUnchangedVars(allTLAVariables, List(AE, O, pc, raOnThreadExit))
                    case QuantifiedMultiSet(enumerator, message) =>
                      getUnchangedVars(allTLAVariables, List(AE, O, pc, raOnThreadExit))
                  }
                ))
              ))
            )
          case ThreadInstantiation(identifier) =>
            // With thread instantiations, the successor label can only be determined correctly using the thread identifier.
            val definedThreadEntryOperation = getDefinedThreadEntryOperation(epcCtx.thdDefinitionsToOperations(identifier))
            val threadEntryOperationLabel = getSuccessorLabel(None, None, Some(identifier), definedThreadEntryOperation)
            val freeVariables = epcCtx.thdDefinitionsToFreeVariables(identifier)
            List(
              OperatorDefinition(NonFixLHS(Identifier(currentLabel), List(self, thd)), ConjunctionList(
                List(pcEnablingCondition)
                  ++ generateThreadInstantiationPreconditions(freeVariables, locationVariables, successorLabel)
                  ++ Some(PCUpdate(threadEntryOperationLabel))
                  // If there is a successor in the same region, we push that successor on the "return addresses" stack.
                  ++ Option.when(successorLabel != DONE) (ThdLvlExceptUpdate(ra, BinaryExpression("\\o", Tuple(List(Literal(successorLabel))), ThdLvlFnAppl(ra))))
                  ++ Option.when(successorLabel != DONE) (UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, ra))))
                  // If there is no successor in the same region, we don't do anything with the "return addresses" stack.
                  ++ Option.when(successorLabel == DONE) (UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc))))
              ))
            )
          // This is only to remove the compiler warning, conditional threads are turned into IfOperations in the analysis phase!
          case ConditionalThread(_, _, _) => Nil
          // This is only to remove the compiler warning, choices are turned into ChoiceOperations in the analysis phase!
          case Choice(_) => Nil
        }

      case IfOperation(id, label, condition, thenRegion, elseRegion) =>
        // Get the "then" and "else" action labels.
        val thenActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some("T"))
        val elseActionLabel = getOperationLabel(locEnumOption, thdEnumOption, thdIdentifierOption, operation, Some("F"))
        // Calculate the "then" and "else" successors and their labels.
        val thenSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "then")
        val elseSuccessor = getSuccessorByEdgeLabel(operation, flattenedRegion, edges, "else")
        val thenSuccessorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, thenSuccessor)
        val elseSuccessorLabel = getSuccessorLabel(locEnumOption, thdEnumOption, thdIdentifierOption, elseSuccessor)
        // Check if the "then" or "else" block is directly exiting a defined thread and create
        // the "pc" and "ra" updates for both cases (exiting or not exiting a defined thread).
        val thenIsThreadExit = thdIdentifierOption.isDefined && thenSuccessorLabel == DONE
        val elseIsThreadExit = thdIdentifierOption.isDefined && elseSuccessorLabel == DONE
        val thenPCUpdateUnlessOnThreadExit = Option.unless(thenIsThreadExit)(PCUpdate(thenSuccessorLabel))
        val elsePCUpdateUnlessOnThreadExit = Option.unless(elseIsThreadExit)(PCUpdate(elseSuccessorLabel))
        val thenPCUpdateOnThreadExit = Option.when(thenIsThreadExit)(PCUpdateOnThreadExit())
        val elsePCUpdateOnThreadExit = Option.when(elseIsThreadExit)(PCUpdateOnThreadExit())
        val thenRAUpdateOnThreadExit = Option.when(thenIsThreadExit)(RAUpdateOnThreadExit())
        val elseRAUpdateOnThreadExit = Option.when(elseIsThreadExit)(RAUpdateOnThreadExit())
        val thenRAOnThreadExit = Option.when(thenIsThreadExit)(ra)
        val elseRAOnThreadExit = Option.when(elseIsThreadExit)(ra)
        List(
          OperatorDefinition(NonFixLHS(Identifier(thenActionLabel), List(self, thd)), ConjunctionList(
            List(pcEnablingCondition)
              ++ generateTruePrecondition(condition, locationVariables)
              ++ thenPCUpdateUnlessOnThreadExit
              ++ thenPCUpdateOnThreadExit
              ++ thenRAUpdateOnThreadExit
              ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, thenRAOnThreadExit))))
          )),
          OperatorDefinition(NonFixLHS(Identifier(elseActionLabel), List(self, thd)), ConjunctionList(
            List(pcEnablingCondition)
              ++ generateFalsePrecondition(condition, locationVariables)
              ++ elsePCUpdateUnlessOnThreadExit
              ++ elsePCUpdateOnThreadExit
              ++ elseRAUpdateOnThreadExit
              ++ Some(UnchangedExpr(getUnchangedVars(allTLAVariables, List(pc, elseRAOnThreadExit))))
          ))
        )

      case ChoiceOperation(id, label, choices) =>
        // Here we call the translateOperation() method recursively for all receive alternatives.
        // Additionally, we pass the index of the "receive alterative" among all alternatives
        // as a "secondary operation ID", so that the corresponding TLA+ operator can be generated with a unique name.
        choices.zipWithIndex.flatMap((choiceOperation, choiceIndex) => translateOperation(locEnumOption, thdEnumOption, thdIdentifierOption,
          choiceOperation, flattenedRegion, edges, locationVariables, allTLAVariables, Some(choiceIndex + 1)))
    }
  }

  private def getOperationSuffix(operation: Operation): String =
    operation match {
      case LeafOperation(id, label, command) =>
        command match {
          case Declaration(_) => "D"
          case Assignment(_, _) => "A"
          case Receive(_, _, _) => "R"
          case Choice(_) => "R" // this should not happen, since choices are turned into ChoiceOperations!
          case IndexedSumChoice(_, _) => "R"
          case Send(_) => "S"
          case ThreadInstantiation(_) => "I"
          case ConditionalThread(_, _, _) => "C" // this should not happen, since conditional threads are turned into IfOperations!
        }
      case IfOperation(id, label, condition, thenRegion, elseRegion) => "C" // "C" for conditional
      case ChoiceOperation(id, label, choices) => "R" // "R" for receive action
    }

  private def getOperationLabel(locEnumOption: Option[Enumerator],
                                thdEnumOption: Option[Enumerator],
                                threadIdentifierOption: Option[Identifier],
                                operation: Operation,
                                nonStandardSuffix: Option[String]): String = {
    if (locEnumOption.isDefined && thdEnumOption.isDefined && threadIdentifierOption.isEmpty) {
      return getOperationLabel(locEnumOption.get, thdEnumOption.get, operation, nonStandardSuffix)
    }
    if (locEnumOption.isEmpty && thdEnumOption.isEmpty && threadIdentifierOption.isDefined) {
      return getOperationLabel(threadIdentifierOption.get, operation, nonStandardSuffix)
    }
    ""
  }

  private def getOperationLabel(locEnum: Enumerator, thdEnum: Enumerator, operation: Operation, nonStandardSuffix: Option[String]): String =
    operation.label match {
      case Some(value) => value.name + nonStandardSuffix.getOrElse("")
      // If there is no label, we create one using the location identifier, thread identifier, operation id and suffix.
      // E.g. p_j_1A, p_j_2R, p_j_3S, ...
      case None => "%s_%s_%d%s" format(
        locEnum.asInstanceOf[BoundedEnumerator].name.name,
        thdEnum.asInstanceOf[BoundedEnumerator].name.name,
        operation.id,
        nonStandardSuffix.getOrElse(getOperationSuffix(operation))
      )
    }

  private def getOperationLabel(thdIdentifier: Identifier, operation: Operation, nonStandardSuffix: Option[String]): String =
    operation.label match {
      case Some(value) => value.name + nonStandardSuffix.getOrElse("")
      case None => "%s_%d%s" format(thdIdentifier.name, operation.id, nonStandardSuffix.getOrElse(getOperationSuffix(operation)))
    }

  private def getSuccessorLabel(locEnumOption: Option[Enumerator],
                                thdEnumOption: Option[Enumerator],
                                threadIdentifierOption: Option[Identifier],
                                successorOption: Option[Operation]): String =
    successorOption match {
      case Some(successorOperation) => getOperationLabel(
        locEnumOption,
        thdEnumOption,
        threadIdentifierOption,
        successorOperation,
        None
      )
      case None => DONE
    }

  /**
   * Generate the enabling condition for the program counter
   * E.g. pc[self][thd] = "..."
   */
  private def PCEnablingCondition(label: String): Expression = BinaryExpression("=", ThdLvlFnAppl(pc), Literal(label))

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
    ThdLvlExceptUpdate(ra,
      IfThenElse(
        BinaryExpression("#", ThdLvlFnAppl(ra), Tuple(Nil)),
        IdentifierApplication(Identifier("Tail"), List(ThdLvlFnAppl(ra))),
        ThdLvlFnAppl(ra)
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
    val variableIsUndefined = BinaryExpression("=", rewriteAccessToLocationVariables(identifier, locationVariables), Undefined)
    // The effect of the action is that the variable is now Initialized at the location.
    // E.g. x' = [x EXCEPT ![self] = Initialized]
    val variableIsInitializedInNextState = BinaryExpression("=",
      PrimedExpression(identifier),
      ExceptExpression(identifier, List(ExceptClause(SelectorPath(List(IndexSelector(List(self)))), Initialized)))
    )
    List(variableIsUndefined, variableIsInitializedInNextState)
  }

  private def generateAssignmentEffect(identifier: Identifier, expression: Expression, locationVariables: List[Identifier]): List[Expression] = {
    // We require that the variable in the assign action is not Undefined at the location.
    // E.g. x[self] # Undefined
    val variableIsNotUndefined = BinaryExpression("#", rewriteAccessToLocationVariables(identifier, locationVariables), Undefined)
    // The effect of the action is that the variable at the location is now set to the rhs of the assign action.
    // E.g. x' = [x EXCEPT ![self] = expression...]
    val variableIsSetToExpressionInNextState = BinaryExpression("=",
      PrimedExpression(identifier),
      // Rewrite all variable read operations like this x -> x[self]
      ExceptExpression(identifier, List(ExceptClause(SelectorPath(List(IndexSelector(List(self)))), rewriteAccessToLocationVariables(expression, locationVariables))))
    )
    List(variableIsNotUndefined, variableIsSetToExpressionInNextState)
  }

  private def generateReceiveEffect(channel: Expression,
                                    location: Expression,
                                    variable: Identifier,
                                    locationVariables: List[Identifier],
                                    rcvOrRcvS: "Rcv" | "RcvS",
                                    enumeratorOption: Option[Enumerator] = None): List[Expression] = {
    val channelExpr = rewriteAccessToLocationVariables(channel, locationVariables)
    val sourceExpr = rewriteAccessToLocationVariables(location, locationVariables)
    val variableExpr = rewriteAccessToLocationVariables(variable, locationVariables)
    // The channel should be part of "ChanSet \ SuspChanSet" or "SuspChanSet"
    val channelSet = rcvOrRcvS match {
      case "Rcv" if epcCtx.suspiciousChannels.isEmpty => ChanSet
      case "Rcv" => BinaryExpression("\\", ChanSet, SuspChanSet)
      case "RcvS" => SuspChanSet
    }
    // We require that the channel is a "normal", non-suspicious channel, i.e. c \in Chan \ SuspChanSet
    val channelIsInChanSet = BinaryExpression("\\in", channelExpr, channelSet)
    // We require that the location expression can be evaluation to a valid location (is element of ProcSet).
    val sourceExprIsInProcSet = BinaryExpression("\\in", sourceExpr, ProcSet)
    // We require that the variable in the "receive" action is not Undefined at the location. E.g. x[self] # Undefined.
    val variableIsNotUndefined = BinaryExpression("#", variableExpr, Undefined)
    // Another precondition is that there is a message in the given channel
    // \E msg \in DOMAIN AE[c]
    val messageBinder = getUniqueIdentifier(Identifier("msg"))
    // Nested preconditions under the existential quantifier
    // Precondition that requires that the message source location is the one specified in the message source expression
    // msg[1] = ... (location-expression)
    val sourcePrecondition = BinaryExpression("=", FunctionApplication(messageBinder, List(Literal(1))), sourceExpr)
    // Precondition that requires that the message target location in the current location (self)
    // msg[2] = self
    val targetPrecondition = BinaryExpression("=", FunctionApplication(messageBinder, List(Literal(2))), self)
    // For TLA+ actions corresponding to rule RcvS we also need to update the D component of the executing location
    val failureDetectorUpdate = Option.when(rcvOrRcvS == "RcvS")(
      // D' = [D EXCEPT ![self][channelExpr][sourceExpr] = TRUE]
      BinaryExpression("=", PrimedExpression(D),
        ExceptExpression(D, List(ExceptClause(SelectorPath(List(
          IndexSelector(List(self)), IndexSelector(List(channelExpr)), IndexSelector(List(sourceExpr)))),
          Literal(true))))
      )
    )
    // Update of the variable at the location to the payload of the message.
    // E.g. x' = [x EXCEPT ![self] = msg[3] ]
    val variableUpdate = BinaryExpression("=", PrimedExpression(variable), ExceptExpression(variable, List(
      ExceptClause(SelectorPath(List(IndexSelector(List(self)))), FunctionApplication(messageBinder, List(Literal(3))))
    )))
    // Update of the message aether so that the message is removed from the channel.
    // E.g. AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag( {msg} ) ]
    val messageAetherUpdate = BinaryExpression("=", PrimedExpression(AE),
      ExceptExpression(AE, List(ExceptClause(SelectorPath(List(IndexSelector(List(channelExpr)))),
          // AE[c] (-) SetToBag( {msg} )
          BinaryExpression("(-)", FunctionApplication(AE, List(channelExpr)), IdentifierApplication(SetToBag, List(ExplicitSet(List(messageBinder)))))
      )))
    )
    val messageHandlingEffect = BoundedQuantifiedExpression("\\E", List(
      QuantifierBound(messageBinder, UnaryExpression("DOMAIN", FunctionApplication(AE, List(channelExpr))))),
      ConjunctionList(
        List(sourcePrecondition, targetPrecondition)
        ++ failureDetectorUpdate
        ++ List(variableUpdate, messageAetherUpdate))
    )
    // We add all conjuncts together
    val receiveConjuncts = List(channelIsInChanSet, sourceExprIsInProcSet, variableIsNotUndefined, messageHandlingEffect)
    // If an enumerator is provided (this the case when the "receive" action is given as an indexed sum),
    // we map the enumerator to a binder and an index set, and we wrap all generated equations in it.
    // E.g. (j \ in 1..N) c@j?(x) --> /\ \E j \in 1..N : /\ c \in ChanSet /\ ...
    enumeratorOption.map(enumerator => {
      val (indexSetBinder, indexSet) = enumeratorToBoundVariableAndIndexSetWithRewrite(enumerator, locationVariables)
      List(BoundedQuantifiedExpression("\\E", List(QuantifierBound(indexSetBinder, indexSet)), ConjunctionList(receiveConjuncts)))
    }).getOrElse(receiveConjuncts)
  }

  private def generateSuspectEffect(channel: Expression,
                                    location: Expression,
                                    locationVariables: List[Identifier],
                                    enumeratorOption: Option[Enumerator] = None): List[Expression] = {
    val channelExpr = rewriteAccessToLocationVariables(channel, locationVariables)
    val sourceExpr = rewriteAccessToLocationVariables(location, locationVariables)
    // We require that the channel expression can be evaluated to a suspicious channel
    // In symbols: c \in SuspChanSet
    val channelIsInSuspChanSet = BinaryExpression("\\in", channelExpr, SuspChanSet)
    // We require that the source location in the "receive" action is a process.
    // E.g. <location-expression> \in ProcSet
    val sourceIsInProcSet = BinaryExpression("\\in", sourceExpr, ProcSet)
    // We require that the suspectOK-predicate is satisfied
    val suspectOKOptionOnWeakAccuracy = Option.when(epcCtx.unreliableFailureDetection)(
      IdentifierApplication(suspectOK, List(TI, F, sourceExpr, self))
    )
    // We update the D component of the executing location
    // D' = [D EXCEPT ![self][channelExpr][sourceExpr] = FALSE]
    val DComponentUpdate = BinaryExpression("=", PrimedExpression(D),
      ExceptExpression(D, List(ExceptClause(SelectorPath(List(
        IndexSelector(List(self)), IndexSelector(List(channelExpr)), IndexSelector(List(sourceExpr)))),
        Literal(false))))
    )
    val suspectConjuncts = List(channelIsInSuspChanSet, sourceIsInProcSet) ++ suspectOKOptionOnWeakAccuracy ++ List(DComponentUpdate)
    // If an enumerator is provided (this the case when the "receive" action is given as an indexed sum),
    // we map the enumerator to a binder and an index set, and we wrap all generated equations in it.
    // E.g. (j \ in 1..N) c@j?(x) --> /\ \E j \in 1..N : /\ c \in SuspChanSet /\ ...
    enumeratorOption.map(enumerator => {
      val (indexSetBinder, indexSet) = enumeratorToBoundVariableAndIndexSetWithRewrite(enumerator, locationVariables)
      List(BoundedQuantifiedExpression("\\E", List(QuantifierBound(indexSetBinder, indexSet)), ConjunctionList(suspectConjuncts)))
    }).getOrElse(suspectConjuncts)
  }

  private def generateSendEffect(messages: MultiSet[OutgoingMessage],
                                 locationVariables: List[Identifier],
                                 thdIdentifierOption: Option[Identifier],
                                 successorLabel: String): List[Expression] =
  {
    val isThreadExit = thdIdentifierOption.isDefined && successorLabel == DONE
    val pcUpdateUnlessOnThreadExit = Option.unless(isThreadExit)(PCUpdate(successorLabel))
    val pcUpdateOnThreadExit = Option.when(isThreadExit)(PCUpdateOnThreadExit())
    val raUpdateOnThreadExit = Option.when(isThreadExit)(RAUpdateOnThreadExit())
    messages match {
      case ExplicitMultiSet(Nil) =>
        // Empty send commands are filtered before we reach this code, so we do nothing!
        Nil
      case ExplicitMultiSet(singleMsg :: Nil) =>
        // We rewrite the EPC expressions to their TLA+ representation
        val channelExpr = rewriteAccessToLocationVariables(singleMsg.channel, locationVariables)
        val targetExpr = rewriteAccessToLocationVariables(singleMsg.target, locationVariables)
        // We prepare the send preconditions:
        // 1) the channel should be an element of ChanSet
        // 2) the target location should be an element of ProcSet
        val channelPrecondition = BinaryExpression("\\in", channelExpr, ChanSet)
        val targetPrecondition = BinaryExpression("\\in", targetExpr, ProcSet)
        // Sending a single message means that we send it directly, so we simply need to update the channel!
        val channelUpdate = generateChannelUpdateForSingleSend(singleMsg, locationVariables)
        val pcUpdate = PCUpdate(successorLabel)
        List(channelPrecondition, targetPrecondition, channelUpdate)
          ++ pcUpdateUnlessOnThreadExit
          ++ pcUpdateOnThreadExit
          ++ raUpdateOnThreadExit
      case ExplicitMultiSet(multipleMessages) =>
        // Sending multiple messages means that we also need to remember what was sent so far.
        // The implementation of the Send command is based on existential quantification over the set of messages
        // that are still not send. Since we cannot simply send a multiset of all messages at once (because the values
        // of the target and value expressions in the messages can change between sending two messages; this can happen
        // if we have a separate thread in the same location, which updates the variables used in the message),
        // we will use a set of indices to remember, which messages were already send. This means that the O variable
        // will contain exactly those indices.
        //
        // E.g. if we need to send two messages, lets say {{ c@2<<"Hello">>, c@3<<"Hello">> }},
        // we create the set 1..2 and maintain O[self][thd], which should initially be empty,
        // then \E i \in 1..2 \ O[self][thd]: ... will give us the index of the next message to send.
        // The changes will include a change in the channel based on a case distinction for the different indices:
        // - AE' = CASE i = 1 -> [AE EXCEPT ![c] = AE[c] (+) SetToBag({ << self, 2, "Hello" >> })]
        //         []   i = 2 -> [AE EXCEPT ![c] = AE[c] (+) SetToBag({ << self, 2, "Hello" >> })]
        // an update of the O variable to include the index i, which is being sent with the current action.
        // Importantly, once we have sent all message, we need to reset O to the empty set, in order to "clean it up".
        // - O' = [O EXCEPT ![self][thd] = IF @ \cup { i } # 1..2
        //                                    THEN @ \cup { i }
        //                                    ELSE {} ]
        // And finally, we need to update the pc in two ways: it stays the same if there are still messages to be sent,
        // or it moves to the next command if all messages were already sent (we recognize this using O')
        // - pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup { i } # 1..2
        //                                    THEN @
        //                                    ELSE "successorLabel"]
        val indexBinder = getUniqueIdentifier(Identifier("i"))
        val allIndices = BinaryExpression("..", Literal(1), Literal(multipleMessages.size))
        val alreadySend = FunctionApplication(FunctionApplication(O, List(self)), List(thd))
        val remainingIndices = BinaryExpression("\\", allIndices, alreadySend)
        val raUpdateOnThreadExitForMultiSend = Option.when(isThreadExit)(generateRAUpdateForMultiSend(indexBinder, allIndices))
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(indexBinder, remainingIndices)), ConjunctionList(
            List(
              generatePreconditionForMultiSend(indexBinder, multipleMessages, locationVariables),
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
        // E.g. {{ (j = 1) c@j<<j>> }}
        // Still, we need to the bounded variable, so we turn the domain into a set. This example will be translated into:
        // SetToBag({ << self, j, j >> })
        val (boundVariable, indexSet) = enumeratorToBoundVariableAndIndexSetWithRewrite(enumerator, locationVariables)
        val alreadySend = FunctionApplication(FunctionApplication(O, List(self)), List(thd))
        val remainingIndices = BinaryExpression("\\", indexSet, alreadySend)
        val raUpdateOnThreadExitForMultiSend = Option.when(isThreadExit)(generateRAUpdateForMultiSend(boundVariable, indexSet))
        // We rewrite the EPC expressions to their TLA+ representation
        val channelExpr = rewriteAccessToLocationVariables(message.channel, locationVariables)
        val targetExpr = rewriteAccessToLocationVariables(message.target, locationVariables)
        // We prepare the send preconditions:
        // 1) the channel should be an element of ChanSet
        // 2) the target location should be an element of ProcSet
        val channelPrecondition = BinaryExpression("\\in", channelExpr, ChanSet)
        val targetPrecondition = BinaryExpression("\\in", targetExpr, ProcSet)
        List(
          BoundedQuantifiedExpression("\\E", List(QuantifierBound(boundVariable, remainingIndices)), ConjunctionList(
            List(
              channelPrecondition,
              targetPrecondition,
              generateChannelUpdateForSingleSend(message, locationVariables),
              generatePCUpdateForMultiSend(boundVariable, indexSet, thdIdentifierOption, successorLabel))
            ++ raUpdateOnThreadExitForMultiSend
            ++ Some(generateOUpdateForMultiSend(boundVariable, indexSet))
          ))
        )
    }
  }

  /**
   * Translates bounded enumerators into pairs of a bound variable and a set.
   * During the translation all accesses to location variables in the domain
   * of the enumerator are rewritten so that they are applied to self (e.g. x --> x[self]).
   * @param enumerator The input enumerator.
   * @param locationVariables The identifiers that are location variables and should be rewritten.
   * @return
   */
  private def enumeratorToBoundVariableAndIndexSetWithRewrite(enumerator: Enumerator, locationVariables: List[Identifier]): (Identifier, Expression) = {
    enumerator match {
      // Translate the bounded enumerators into pairs of a binder and a set.
      // {{ (j = 1) c@j!<<j>> }}       --> (j, {1})   --> \E j \in {1}
      // {{ (j \in 1..N) c@j!<<j>> }}  --> (j, 1..N)  --> \E j \in 1..N
      case BoundedEnumerator(name, "=", domain) =>
        val rewrittenDomain = rewriteAccessToLocationVariables(domain, locationVariables)
        (name, ExplicitSet(List(rewrittenDomain)))
      case BoundedEnumerator(name, "\\in", domain) =>
        val rewrittenDomain = rewriteAccessToLocationVariables(domain, locationVariables)
        (name, rewrittenDomain)
      // Turning bounded enumerators with filters into index sets introduces binders with the same name:
      // {{ (j = 1) c@j!<<j>> : j # 1 }}           --> \E j \in { j \in {1}: j # 1 }: ...
      // {{ (j \in 1..N) c@j!<<j>> : j # self }}   --> \E j \in { j \in 1..N: j # self }: ...
      // To avoid this, we do a renaming of the inner binder with a unique name, constructed from the bound variable.
      case BoundedEnumeratorWithFilter(name, "=", domain, filter) =>
        val rewrittenDomain = rewriteAccessToLocationVariables(domain, locationVariables)
        val rewrittenFilter = rewriteAccessToLocationVariables(filter, locationVariables)
        val setComprehension = BoundedSetComprehension(name, ExplicitSet(List(rewrittenDomain)), rewrittenFilter)
        // We "throw away" the identifier with the same name as given in the EPC code (e.g. simply j).
        if (!knownIdentifiers.contains(name)) getUniqueIdentifier(name)
        // We create a new identifier for the inner set comprehension (e.g. j1).
        val innerBinder = getUniqueIdentifier(name)
        val renamedSetComprehension = renameBV(name, innerBinder, setComprehension, depth = 1)
        (name, renamedSetComprehension)
      case BoundedEnumeratorWithFilter(name, "\\in", domain, filter) =>
        val rewrittenDomain = rewriteAccessToLocationVariables(domain, locationVariables)
        val rewrittenFilter = rewriteAccessToLocationVariables(filter, locationVariables)
        val setComprehension = BoundedSetComprehension(name, rewrittenDomain, rewrittenFilter)
        // We "throw away" the identifier with the same name as given in the EPC code (e.g. simply j).
        if (!knownIdentifiers.contains(name)) getUniqueIdentifier(name)
        // We create a new identifier for the inner set comprehension (e.g. j1).
        val innerBinder = getUniqueIdentifier(name)
        val renamedSetComprehension = renameBV(name, innerBinder, setComprehension, depth = 1)
        (name, renamedSetComprehension)
    }
  }

  private def generateChannelUpdateForSingleSend(singleMsg: OutgoingMessage, locationVariables: List[Identifier]) = {
    // We rewrite the EPC expressions to their TLA+ representation
    val channelExpr = rewriteAccessToLocationVariables(singleMsg.channel, locationVariables)
    val targetExpr = rewriteAccessToLocationVariables(singleMsg.target, locationVariables)
    val valueExpr = rewriteAccessToLocationVariables(singleMsg.value, locationVariables)
    // Create the conjunct with the update of the message aether variable AE:
    // AE' = [AE EXCEPT ![c] = AE[c] (+) SetToBag({<<self,2,x[self]>>})]
    BinaryExpression("=", PrimedExpression(AE),
      ExceptExpression(AE, List(
        ExceptClause(SelectorPath(List(IndexSelector(List(channelExpr)))),
          // AE[c] (+) SetToBag({ << self, 2, x[self] >> })
          BinaryExpression("(+)", FunctionApplication(AE, List(channelExpr)),
            IdentifierApplication(SetToBag, List(ExplicitSet(List(
              // Here, we create a triple of the form << self, target, value >>
              Tuple(List(self, targetExpr, valueExpr)))))
            )
          )
        )
      ))
    )
  }

  private def generatePreconditionForMultiSend(indexBinder: Identifier, multipleMessages: List[OutgoingMessage], locationVariables: List[Identifier]) = {
    // Create the conjunct with the preconditions for the multi-send action:
    // E.g. {{ c@2<<"Hello">>, c@2<<""Hello">> }}
    // CASE i = 1 -> /\ c \in ChanSet
    //               /\ 2 \in ProcSet
    // []   i = 2 -> /\ c \in ChanSet
    //               /\ 2 \in ProcSet
    CaseExpression(
      for ((message, idx) <- multipleMessages.zipWithIndex) yield {
        // We rewrite the EPC expressions to their TLA+ representation
        val channelExpr = rewriteAccessToLocationVariables(message.channel, locationVariables)
        val targetExpr = rewriteAccessToLocationVariables(message.target, locationVariables)
        val valueExpr = rewriteAccessToLocationVariables(message.value, locationVariables)
        // Create the case arm with the update
        CaseArm(
          // CASE i = 1 ->
          BinaryExpression("=", indexBinder, Literal(idx + 1)),
          // ConjunctionList of the form:
          // /\ c \in ChanSet
          // /\ 2 \in ProcSet
          ConjunctionList(List(
            BinaryExpression("\\in", channelExpr, ChanSet),
            BinaryExpression("\\in", targetExpr, ProcSet)
          ))
        )
      },
      None
    )
  }

  private def generateChannelUpdateForMultiSend(indexBinder: Identifier, multipleMessages: List[OutgoingMessage], locationVariables: List[Identifier]) = {
    // Create the conjunct with the update of the message aether variable AE:
    // - AE' = CASE i = 1 -> [AE EXCEPT ![c] = AE[c] (+) SetToBag({ << self, 2, "Hello" >> })]
    //         []   i = 2 -> [AE EXCEPT ![c] = AE[c] (+) SetToBag({ << self, 2, "Hello" >> })]
    BinaryExpression("=", PrimedExpression(AE),
      CaseExpression(
        for ((message, idx) <- multipleMessages.zipWithIndex) yield {
          // We rewrite the EPC expressions to their TLA+ representation
          val channelExpr = rewriteAccessToLocationVariables(message.channel, locationVariables)
          val targetExpr = rewriteAccessToLocationVariables(message.target, locationVariables)
          val valueExpr = rewriteAccessToLocationVariables(message.value, locationVariables)
          // Create the case arm with the update
          CaseArm(
            // CASE i = 1 ->
            BinaryExpression("=", indexBinder, Literal(idx + 1)),
            // [AE EXCEPT ![c] = AE[c] (+) SetToBag({ << self, 2, "Hello" >> })]
            ExceptExpression(AE, List(
              ExceptClause(SelectorPath(List(IndexSelector(List(channelExpr)))),
                // AE[c] (+) SetToBag({ << self, 2, x[self] >> })
                BinaryExpression("(+)", FunctionApplication(AE, List(channelExpr)),
                  IdentifierApplication(SetToBag, List(ExplicitSet(List(
                    // Here, we create a triple of the form << self, target, value >>
                    Tuple(List(self, targetExpr, valueExpr)))))
                  )
                )
              )
            ))
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
    val isThreadExit = thdIdentifierOption.isDefined && successorLabel == DONE
    BinaryExpression(
      "=",
      PrimedExpression(pc),
      ExceptExpression(
        pc,
        List(
          // EXCEPT
          ExceptClause(
            // ![self][thd] =
            SelectorPath(List(IndexSelector(List(self)), IndexSelector(List(thd)))),
            // IF O[self][thd] \cup { i } # <domain> THEN pc[self][thd] ELSE <successorLabel>
            IfThenElse(
              BinaryExpression(
                "#",
                BinaryExpression(
                  "\\cup",
                  FunctionApplication(FunctionApplication(O, List(self)), List(thd)),
                  ExplicitSet(List(binder)),
                ),
                domain
              ),
              // THEN pc[self][thd]
              ThdLvlFnAppl(pc),
              // ELSE <successorLabel>
              if (isThreadExit) {
                IfThenElse(
                  BinaryExpression("#", ThdLvlFnAppl(ra), Tuple(Nil)),
                  IdentifierApplication(Identifier("Head"), List(ThdLvlFnAppl(ra))),
                  Done
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
        BinaryExpression("#", BinaryExpression("\\cup", ThdLvlFnAppl(O), ExplicitSet(List(binder))), domain),
        ThdLvlFnAppl(ra),
        IfThenElse(
          BinaryExpression("#", ThdLvlFnAppl(ra), Tuple(Nil)),
          IdentifierApplication(Identifier("Tail"), List(ThdLvlFnAppl(ra))),
          ThdLvlFnAppl(ra)
        )
      )
    )

  private def generateOUpdateForMultiSend(binder: Identifier, domain: Expression): Expression = {
    BinaryExpression(
      "=",
      PrimedExpression(O),
      ExceptExpression(
        O,
        List(
          // EXCEPT
          ExceptClause(
            // ![self][thd] =
            SelectorPath(List(IndexSelector(List(self)), IndexSelector(List(thd)))),
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
    List(rewriteAccessToLocationVariables(condition, locationVariables))

  private def generateFalsePrecondition(condition: Expression, locationVariables: List[Identifier]): List[Expression] =
    List( UnaryExpression("~", ParenthesizedExpression( rewriteAccessToLocationVariables(condition, locationVariables) )) )

  private def generateThreadInstantiationPreconditions(freeVariables: List[Identifier], locationVariables: List[Identifier], successorLabel: String): List[Expression] = {
    // We require that the free variables from the thread definition are not Undefined at the location.
    // E.g. x[self] # Undefined, y[self] # Undefined
    for identifier <- freeVariables yield { BinaryExpression("#", rewriteAccessToLocationVariables(identifier, locationVariables), Identifier("Undefined")) }
  }

  /**
   * General rewrite, which allows to rewrite different AST nodes of a TLA+ expression with an input "rewrite function".
   * The method iterates recursively over an AST and rewrites each node using the function rewriteFn.
   */
  private def rewrite(expression: Expression, rewriteFn: Expression => Expression): Expression = {
    expression match {
      case literal: Literal => rewriteFn(literal)
      case identifier: Identifier => rewriteFn(identifier)
      case IdentifierTuple(identifiers) => Tuple(identifiers.map(rewriteFn))
      case qb@QuantifierBound(variable, domain) => qb.copy(domain = rewriteFn(domain))
      case m@Mapping(identifier, expression) => m.copy(expression = rewriteFn(expression))
      case Tuple(expressions) => Tuple(expressions.map(rewriteFn))
      case expr@UnaryExpression(_, expression) => expr.copy(expression = rewriteFn(expression))
      case expr@BinaryExpression(_, lhs, rhs) => expr.copy(lhs = rewriteFn(lhs), rhs = rewriteFn(rhs))
      case application@IdentifierApplication(_, operands) => application.copy(operands = operands.map(rewriteFn))
      case application@GeneralIdentifierApplication(_, operands) => application.copy(operands = operands.map(rewriteFn))
      case FunctionApplication(expression, operands) => FunctionApplication(rewriteFn(expression), operands.map(rewriteFn))
      case PrimedExpression(expression) => PrimedExpression(rewriteFn(expression))
      case ParenthesizedExpression(expression) => ParenthesizedExpression(rewriteFn(expression))
      case IfThenElse(cond, thenExpr, elseExpr) => IfThenElse(rewriteFn(cond), rewriteFn(thenExpr), rewriteFn(elseExpr))
      case CaseExpression(caseArms, defaultExpression) =>
        CaseExpression(caseArms.map(caseArm => CaseArm(rewriteFn(caseArm.lhs), rewriteFn(caseArm.rhs))), defaultExpression.map(rewriteFn))
      case ChooseExpression(identifier, domain, predicate) =>
        ChooseExpression(identifier, rewriteFn(domain), rewriteFn(predicate))
      case expr@BoundedQuantifiedExpression(quantifiers, binders, body) =>
        expr.copy(
          binders = binders.map((quantifierBound: QuantifierBound) => QuantifierBound(quantifierBound.variable, rewriteFn(quantifierBound.domain))),
          body = rewriteFn(body)
        )
      case expr@QuantifiedExpression(quantifier, binders, body) => expr.copy(body = rewriteFn(body))
      case ConjunctionList(listItems) => ConjunctionList(listItems.map(rewriteFn))
      case DisjunctionList(listItems) => DisjunctionList(listItems.map(rewriteFn))
      case ExplicitSet(values) => ExplicitSet(values.map(rewriteFn))
      case bsc@BoundedSetComprehension(variable, domain, predicate) =>
        bsc.copy(domain = rewriteFn(domain), predicate = rewriteFn(predicate))
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) =>
        QuantifiedSetComprehension(
          rewriteFn(mappingExpression),
          quantifierBounds.map((qb: QuantifierBound) => QuantifierBound(qb.variable, rewriteFn(qb.domain)))
        )
      case ExplicitFunction(mappings) => ExplicitFunction(mappings.map(mapping => mapping.copy(expression = rewriteFn(mapping.expression))))
      case BoundedMapping(quantifierBounds, expression) =>
        BoundedMapping(
          quantifierBounds.map((qb: QuantifierBound) => QuantifierBound(qb.variable, rewriteFn(qb.domain))),
          rewriteFn(expression)
        )
      case setOfAllFunctions@SetOfAllFunctions(domain, codomain) => SetOfAllFunctions(rewriteFn(domain), rewriteFn(codomain))
      case setOfAllRecords@SetOfAllRecords(mappings) => SetOfAllRecords(mappings.map(mapping => mapping.copy(expression = rewriteFn(mapping.expression))))
      // EXCEPT Expressions are rewritten as a whole.
      // This means that each rewrite function should rewrite these EXCEPT expressions,
      // in order to prevent endless recursion, leading to stack overflows!
      case exceptExpr: ExceptExpression => rewriteFn(exceptExpr)
      // LET-IN Expressions are also rewritten as a whole!
      // ==> rewriteFn should rewrite these expressions and not simply call rewrite on LET-IN expressions.
      case letInExpr: LetInExpression => rewriteFn(letInExpr)
      // The following expressions are simply returned unchanged
      case genIdentifier: GeneralIdentifier => genIdentifier
      case ws: WeakStuttering => ws
      case ss: StrongStuttering => ss
      case wf: WeakFairness => wf
      case sf: StrongFairness => sf
      case at: AtSign => at
    }
  }

  /**
   * Method for rewriting the occurrences of location variables in expressions when translating from EPC to TLA+.
   * A EPC expression "x + 1" for a location variable "x" is translated into "x[self] + 1" in the corresponding TLA+ expression.
   */
  private def rewriteAccessToLocationVariables(expression: Expression, locationVariables: List[Identifier]): Expression = {
    // Create a closure that will do the actual rewriting
    def memoizedRewrite(expression: Expression): Expression = {
      expression match {
        case literal: Literal => literal
        case identifier: Identifier if locationVariables contains identifier => FunctionApplication(identifier, List(self))
        case identifier: Identifier => identifier
        case exceptExpr@ExceptExpression(exceptTarget, clauses) =>
          // We map each clause to a new clause with the following changes:
          // (1) If the EXCEPT target is a location variable (e.g. x), it is changed to x[self]
          // (2) The dot selectors are left unchanged.
          // (3) The index selectors are also rewritten.
          val updatedTarget = if locationVariables contains exceptTarget then FunctionApplication(exceptTarget, List(self)) else exceptTarget
          ExceptExpression(updatedTarget, clauses.map(clause => ExceptClause(
            SelectorPath(clause.selector.parts.map {
              case dotSelector: DotSelector => dotSelector
              case indexSelector: IndexSelector => IndexSelector(indexSelector.indices.map(expr => rewrite(expr, memoizedRewrite)))
            }),
            rewrite(clause.value, memoizedRewrite)
          )))
        case letInExpr@LetInExpression(definitions, expression) =>
          // We rewrite the access to location variables in the bodies of the definitions and the main expression.
          // E.g. for a location variable x, we change "x" to "x[self]" in all expressions.
          // If a definition "redefines" a location variable, we simply skip it and leave
          // TLA+ to show an error in the generated translation.
          val updatedDefinitions: List[OperatorDefinition | FunctionDefinition | ModuleDefinition] = definitions.map {
            case operatorDef@OperatorDefinition(_, body) =>
              operatorDef.copy(body = rewrite(body, memoizedRewrite))
            case functionDef@FunctionDefinition(_, _, body) =>
              functionDef.copy(body = rewrite(body, memoizedRewrite))
            case moduleDef@ModuleDefinition(_, _) =>
              // We leave this case unchanges since it makes no sense for EPC specifications.
              moduleDef
          }
          val updatedExpression = rewrite(expression, memoizedRewrite)
          LetInExpression(updatedDefinitions, updatedExpression)
        case _ => rewrite(expression, memoizedRewrite)
      }
    }
    // Start the rewriting function
    memoizedRewrite(expression)
  }

  /**
   * Renames the "from" identifier to the "to" identifier in the target identifier, list of identifiers or identifier tuple.
   */
  private def renameBV(from: Identifier, to: Identifier, target: Identifier | List[Identifier] | IdentifierTuple): Identifier | List[Identifier] | IdentifierTuple = {
    target match {
      case identifier: Identifier if identifier == from => to
      case identifier: Identifier => identifier
      case identifiers: List[Identifier] => identifiers.map(renameBV(from, to, _).asInstanceOf[Identifier])
      case tuple: IdentifierTuple => IdentifierTuple(tuple.identifiers.map(renameBV(from, to, _).asInstanceOf[Identifier]))
    }
  }

  /**
   * Renames the "from" identifier to the "to" identifier in target quantifier bound, if the depth is positive.
   */
  private def renameBV(from: Identifier, to: Identifier, qb: QuantifierBound, depth: Int): QuantifierBound = {
    // If we encounter the "from" identifier as the binder in the quantifier bound, we rename it, and we keep the domain as it is.
    if (bv(qb).contains(from) && depth > 0) {
      QuantifierBound(renameBV(from, to, qb.variable), qb.domain)
    }
    // If we encounter the "from" identifier as the binder, but depth == 0 (meaning we should not rename further)
    // We keep the binder the same, but rename all free occurrences in the domain.
    else if (bv(qb).contains(from) && depth == 0) {
      QuantifierBound(qb.variable, renameBV(from, to, qb.domain, depth))
    }
    // If we encounter another binder, we leave it as it is and substitute recursively in the domain.
    else {
      QuantifierBound(qb.variable, renameBV(from, to, qb.domain, depth))
    }
  }

  /**
   * Renames the "from" identifier to the "to" identifier in the list with quantifier bounds, if the depth is positive.
   */
  private def renameBV(from: Identifier, to: Identifier, qbs: List[QuantifierBound], depth: Int): List[QuantifierBound] =
    qbs.map(renameBV(from, to, _, depth: Int))

  /**
   * Renames the "from" identifier to the "to" identifier in target expression, if the depth is positive.
   */
  private def renameBV(from: Identifier, to: Identifier, target: Expression, depth: Int): Expression = {
    target match {
      case literal: Literal => literal
      case identifier: Identifier if identifier == from => to
      case identifier: Identifier => identifier
      case IdentifierTuple(identifiers) => Tuple(identifiers.map(renameBV(from, to, _, depth)))
      case quantifierBound: QuantifierBound => renameBV(from, to, quantifierBound, depth)
      case Mapping(identifier, expression) => Mapping(identifier, renameBV(from, to, expression, depth))
      case Tuple(expressions) => Tuple(expressions.map(renameBV(from, to, _, depth)))
      case expr@UnaryExpression(_, expression) => expr.copy(expression = renameBV(from, to, expression, depth))
      case expr@BinaryExpression(_, lhs, rhs) => expr.copy(lhs = renameBV(from, to, lhs, depth), rhs = renameBV(from, to, rhs, depth))
      case application@IdentifierApplication(_, operands) => application.copy(operands = operands.map(renameBV(from, to, _, depth)))
      case application@GeneralIdentifierApplication(_, operands) => application.copy(operands = operands.map(renameBV(from, to, _, depth)))
      case FunctionApplication(expression, operands) => FunctionApplication(renameBV(from, to, expression, depth), operands.map(renameBV(from, to, _, depth)))
      case PrimedExpression(expression) => PrimedExpression(renameBV(from, to, expression, depth))
      case ParenthesizedExpression(expression) => ParenthesizedExpression(renameBV(from, to, expression, depth))
      case IfThenElse(cond, thenExpr, elseExpr) => IfThenElse(renameBV(from, to, cond, depth), renameBV(from, to, thenExpr, depth), renameBV(from, to, elseExpr, depth))
      case CaseExpression(caseArms, defaultExpression) =>
        CaseExpression(caseArms.map(caseArm => CaseArm(renameBV(from, to, caseArm.lhs, depth), renameBV(from, to, caseArm.rhs, depth))),
          defaultExpression.map(renameBV(from, to, _, depth)))
      case ChooseExpression(identifier, domain, predicate) if bv(identifier).contains(from) && depth > 0 =>
        ChooseExpression(
          renameBV(from, to, identifier).asInstanceOf[Identifier | IdentifierTuple],
          domain,
          renameBV(from, to, predicate, depth - 1)
        )
      case ChooseExpression(identifier, domain, predicate) if bv(identifier).contains(from) && depth == 0 =>
        ChooseExpression(identifier, domain, predicate)
      case ChooseExpression(identifier, domain, predicate) =>
        ChooseExpression(
          identifier,
          renameBV(from, to, domain, depth),
          renameBV(from, to, predicate, depth)
        )
      case BoundedQuantifiedExpression(quantifier, binders, body) if bv(binders).contains(from) && depth > 0 =>
        BoundedQuantifiedExpression(
          quantifier,
          renameBV(from, to, binders, depth),
          renameBV(from, to, body, depth - 1)
        )
      case BoundedQuantifiedExpression(quantifier, binders, body) if bv(binders).contains(from) && depth == 0 =>
        BoundedQuantifiedExpression(quantifier, binders, body)
      case BoundedQuantifiedExpression(quantifier, binders, body) =>
        BoundedQuantifiedExpression(
          quantifier,
          renameBV(from, to, binders, depth),
          renameBV(from, to, body, depth)
        )
      case QuantifiedExpression(quantifier, binders, body) if bv(binders).contains(from) && depth > 0 =>
        QuantifiedExpression(
          quantifier,
          renameBV(from, to, binders).asInstanceOf[List[Identifier]],
          renameBV(from, to, body, depth - 1)
        )
      case QuantifiedExpression(quantifier, binders, body) if bv(binders).contains(from) && depth == 0 =>
        QuantifiedExpression(quantifier, binders, body)
      case QuantifiedExpression(quantifier, binders, body) =>
        QuantifiedExpression(
          quantifier,
          renameBV(from, to, binders).asInstanceOf[List[Identifier]],
          renameBV(from, to, body, depth)
        )
      case ConjunctionList(listItems) => ConjunctionList(listItems.map(renameBV(from, to, _, depth)))
      case DisjunctionList(listItems) => DisjunctionList(listItems.map(renameBV(from, to, _, depth)))
      case ExplicitSet(values) => ExplicitSet(values.map(renameBV(from, to, _, depth)))
      case BoundedSetComprehension(variable, domain, predicate) if bv(variable).contains(from) && depth > 0 =>
        BoundedSetComprehension(
          renameBV(from, to, variable).asInstanceOf[Identifier | IdentifierTuple],
          domain,
          renameBV(from, to, predicate, depth - 1)
        )
      case BoundedSetComprehension(variable, domain, predicate) if bv(variable).contains(from) && depth == 0 =>
        BoundedSetComprehension(variable, domain, predicate)
      case BoundedSetComprehension(variable, domain, predicate) =>
        BoundedSetComprehension(
          variable,
          renameBV(from, to, domain, depth),
          renameBV(from, to, predicate, depth)
        )
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) if bv(quantifierBounds).contains(from) && depth > 0 =>
        QuantifiedSetComprehension(
          renameBV(from, to, mappingExpression, depth - 1),
          renameBV(from, to, quantifierBounds, depth)
        )
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) if bv(quantifierBounds).contains(from) && depth == 0 =>
        QuantifiedSetComprehension(mappingExpression, quantifierBounds)
      case QuantifiedSetComprehension(mappingExpression, quantifierBounds) =>
        QuantifiedSetComprehension(
          renameBV(from, to, mappingExpression, depth),
          renameBV(from, to, quantifierBounds, depth)
        )
      case ExplicitFunction(mappings) => ExplicitFunction(mappings.map(renameBV(from, to, _, depth).asInstanceOf[Mapping]))
      case BoundedMapping(quantifierBounds, expression) if bv(quantifierBounds).contains(from) && depth > 0 =>
        BoundedMapping(
          renameBV(from, to, quantifierBounds, depth),
          renameBV(from, to, expression, depth - 1)
        )
      case BoundedMapping(quantifierBounds, expression) if bv(quantifierBounds).contains(from) && depth == 0 =>
        BoundedMapping(quantifierBounds, expression)
      case BoundedMapping(quantifierBounds, expression) =>
        BoundedMapping(
          renameBV(from, to, quantifierBounds, depth),
          renameBV(from, to, expression, depth)
        )
      case SetOfAllFunctions(domain, codomain) => SetOfAllFunctions(renameBV(from, to, domain, depth), renameBV(from, to, codomain, depth))
      case SetOfAllRecords(mappings) => SetOfAllRecords(mappings.map(renameBV(from, to, _, depth).asInstanceOf[Mapping]))
      case ExceptExpression(exceptTarget, clauses) =>
        ExceptExpression(
          renameBV(from, to, exceptTarget, depth),
          clauses.map(clause => ExceptClause(
            SelectorPath(clause.selector.parts.map {
              case dotSelector: DotSelector => dotSelector
              case indexSelector: IndexSelector => IndexSelector(indexSelector.indices.map(expr => renameBV(from, to, expr, depth)))
            }),
            renameBV(from, to, clause.value, depth)
        )))
      case LetInExpression(definitions, expression) =>
        // All occurrences of the "form" identifier are renamed to the "to" identifier in the body of definition
        // if the "from" identifier does not occur as a parameter in the definition.
        val updatedDefinitions: List[OperatorDefinition | FunctionDefinition | ModuleDefinition] = definitions.map {
          case operatorDef@OperatorDefinition(NonFixLHS(identifier, arguments), body) if arguments.exists {
              case identifier: Identifier if identifier == from => true
              case OperatorDeclaration(operator, _) if operator == from => true
              case _ => false
            } => operatorDef
          case operatorDef@OperatorDefinition(NonFixLHS(identifier, arguments), body) =>
            operatorDef.copy(body = renameBV(from, to, body, depth))
          case functionDef@FunctionDefinition(identifier, arguments, body) if bv(arguments) contains from =>
            functionDef
          case functionDef@FunctionDefinition(identifier, arguments, body) =>
            functionDef.copy(body = renameBV(from, to, body, depth))
          case moduleDef: ModuleDefinition => moduleDef
        }
        // All occurrences of the "from" identifier in the expression are renamed to the "to" identifier
        val renamedExpr = renameBV(from, to, expression, depth)
        LetInExpression(updatedDefinitions, renamedExpr)
      // The following expressions are simply returned unchanged
      case genIdentifier: GeneralIdentifier => genIdentifier
      case ws: WeakStuttering => ws
      case ss: StrongStuttering => ss
      case wf: WeakFairness => wf
      case sf: StrongFairness => sf
      case at: AtSign => at
    }
  }

  /**
   * Interprets the input variable (or variables) as bound variables and returns them as a set.
   */
  private def bv(variable: Identifier | List[Identifier] | IdentifierTuple): immutable.Set[Identifier] = {
    variable match {
      case identifier: Identifier => Set(identifier)
      case identifiers: List[Identifier] => identifiers.toSet
      case tuple: IdentifierTuple => tuple.identifiers.toSet
    }
  }

  /**
   * Returns a set with the bound variables of a quantifier bound.
   */
  private def bv(qb: QuantifierBound): immutable.Set[Identifier] = bv(qb.variable)

  /**
   * Returns a set with the bound variables of a list of quantifier bounds.
   */
  private def bv(qbs: List[QuantifierBound]): immutable.Set[Identifier] = qbs.toSet.flatMap(bv)

  /**
   * Small utility method for the creation of unique identifiers.
   * For an input identifier, we check iteratively if we have already seen it
   * or a version of it with an index after the name. (E.g. x, x1, x2, ...)
   * Once we find a new unique identifier, we add it to the known identifiers set
   * and we return it.
   */
  private def getUniqueIdentifier(identifier: Identifier): Identifier = {
    var idx = 0
    var tempIdentifier = identifier
    // We check if we have already seen this identifier.
    // If yes, we increment the counter and check for the next one.
    while (knownIdentifiers contains tempIdentifier) {
      idx += 1
      tempIdentifier = Identifier(s"${identifier.name}${idx.toString}")
    }
    // Once we found a new identifier, we add it to the known identifiers set.
    knownIdentifiers addOne tempIdentifier
    // And we return the identifier to the caller.
    tempIdentifier
  }
}