------------------------------ MODULE EPC2TLA ------------------------------
EXTENDS Integers, Sequences, Bags

CONSTANTS Variables, Locations, Channels

CONSTANTS Undefined, Initialized

VARIABLES loc_network, msg_aether, thr_defs

VARIABLES trim, failed_locs

vars == << loc_network, msg_aether, thr_defs, trim, failed_locs >>

\* EPC_mem == [Locations -> [Variables -> Values]]
\* loc_network == [Location |-> ]
\* VARIABLES network
\* TypeOK == network \in [locations |-> ] 

(***************************************************************************)
(* Records representing an outgoing message.  These are the messages that  *)
(* are being sent by the send actions.  Here te channel, target location   *)
(* and value fields are supposed to be expressions, which can be evaluated *)
(* to actual channels, locations and values.                               *)
(***************************************************************************)
out_msg(channel, target, value) ==
    [type |-> "message", 
     channel |-> channel,
     target |-> target,
     value |-> value]

(***************************************************************************)
(* Records representing a message.  These are the messages in transit.     *)
(***************************************************************************)
msg(channel, source, target, value) ==
    [type |-> "message", 
     channel |-> channel,
     source |-> source,
     target |-> target,
     value |-> value]

(***************************************************************************)
(* Record representing the variable declation action.                      *)
(***************************************************************************)
var(variable) == 
    [type |-> "action",
     subtype |-> "var", 
     var_name |-> variable]

(***************************************************************************)
(* Record representing the assignment of value to a variable.              *)
(***************************************************************************)
assign(lhs, rhs) == 
    [type |-> "action", 
     subtype |-> "assign", 
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing that a process wants to receive a value from a      *)
(* channel and write that value into a variable.                           *)
(***************************************************************************)
rcv(channel, location, variable) == 
    [type |-> "action", 
     subtype |-> "receive",
     channel |-> channel,
     location |-> location,
     variable |-> variable]

(***************************************************************************)
(* Record representing the sending of a bag of messages.                   *)
(***************************************************************************)
snd(msgs) == 
    [type |-> "action", 
     subtype |-> "send",
     msgs |-> msgs]

(***************************************************************************)
(* This is the empty process.  Once reached, it cannot perform any other   *)
(* step.                                                                   *)
(***************************************************************************)
Nil ==
    [type |-> "guard",
     subtype |-> "empty_process"]

(***************************************************************************)
(* The is the prefix guard.  Once the action is performed, the thread is   *)
(* freed and can be executed.                                              *)
(***************************************************************************)
prefix(action, thread) ==
    [type |-> "guard",
     subtype |-> "prefix",
     action |-> action,
     thread |-> thread ]

(***************************************************************************)
(* This is a thread, which is a guard.  When such a thread is available in *)
(* the current state of a location, the location should simply perform a   *)
(* step from the guard, if possible.                                       *)
(***************************************************************************)
thr_guard(guard) == 
    [type |-> "thread", 
     subtype |-> "guard",
     guard |-> guard]

(***************************************************************************)
(* Records that represent a "defining equation" for a thread.  These       *)
(* records contain the thread name, list with free variables (occuring in  *)
(* the thread) and the body of the thread.  Such definitions can be        *)
(* provided and later referenced from the body of other guards or threads, *)
(* part of a term of the calculus.                                         *)
(***************************************************************************)
thr_ident_def(name, free_vars, thr_body) ==
    [type |-> "definition",
     subtype |-> "thread",
     name |-> name,
     free_vars |-> free_vars,
     thr_body |-> thr_body]

(***************************************************************************)
(* This is a thread identifier, which points to one of the threads from    *)
(* the thread definitions (the variable thr_defs, which is also part of    *)
(* the state).  We can write the so called "defining equations", which     *)
(* assosicate a thread identifier (which itself is a record with fields    *)
(* "type", "subtype", "name", "free_vars") with a thread ("thr_body",      *)
(* which is given as the AST of records), and allows the programmer to     *)
(* reference a defined thread from other guards or threads.                *)
(*                                                                         *)
(* When such a thread is available in the current state, we can substitue  *)
(* the identifier with the thread body, as defined in the defining         *)
(* equation.                                                               *)
(***************************************************************************)
thr_ident(thr_identifier) ==
    [type |-> "thread", 
     subtype |-> "identifier",
     thr_identifier |-> thr_identifier]

(***************************************************************************)
(* This is a thread, which is a conditional thread.  When such a thread is *)
(* available in the current state, we we can transition to the             *)
(* then_thread, if the condition can be evaluated to true, or we can       *)
(* transition to the else_thread if the condition can evaluated to false.  *)
(***************************************************************************)
thr_cond(condition, then_thread, else_thread) ==
    [type |-> "thread", 
     subtype |-> "conditional",
     condition |-> condition,
     then_thread |-> then_thread,
     else_thread |-> else_thread]

(***************************************************************************)
(* This is a thread, which is a composition.  When such a thread in the    *)
(* current state of a location, the location can perform a transition from *)
(* either the left-hand side thread, or the right-hand side thread.        *)
(***************************************************************************)
thr_comp(lhs, rhs) == 
    [type |-> "thread", 
     subtype |-> "composition",
     lhs |-> lhs, 
     rhs |-> rhs]
     
thr_compN(index_set, body) == 
    [type |-> "thread",
     subtype |-> "n-ary-composition",
     index_set |-> index_set,
     body |-> body]

(***************************************************************************)
(* This is a location.  It is named, it has a memory and access to a       *)
(* thread.  The transitions it can perform are exactly the transitions,    *)
(* which the thread can perform.                                           *)
(***************************************************************************)
\* loc_state is a set of lists of prefixes
loc(id, mem, threads_bag) ==
    [type |-> "location",
     subtype |-> "single",
     id |-> id,
     mem |-> mem,
     threads_bag |-> threads_bag]

(***************************************************************************)
(* Records for the storing of literals.  We need these, because if we use  *)
(* the values themselves, TLC will optimize them (as ints or strings) and  *)
(* will throws an error if we try to compare these value to records.       *)
(***************************************************************************)
lit(value) ==
    [type |-> "literal",
     value |-> value]

num(value) ==
    [type |-> "literal",
     subtype |-> "number",
     value |-> value]

str(value) ==
    [type |-> "literal",
     subtype |-> "string",
     value |-> value]

\* These are not used anymore!
numvar(variable) ==
    [type |-> "identifier",
     subtype |-> "number",
     variable |-> variable]

strvar(variable) ==
    [type |-> "identifier",
     subtype |-> "string",
     variable |-> variable]

(***************************************************************************)
(* Records for the storing of identifiers.  We need these, because if we   *)
(* use the values themselves, TLC will optimize them (as ints or strings)  *)
(* and will throws an error if we try to compare these value to records.   *)
(***************************************************************************)
ident(name) ==
    [type |-> "identifier",
     name |-> name]

\* https://groups.google.com/g/tlaplus/c/YYI2yRyctwI
\* RECURSIVE Expressions
\* Expressions == [type: { "expression" }, subtype: { "addition" }, lhs: Expressions, rhs: Expressions]

(***************************************************************************)
(* Record representing the equality of the two expressions (possibly       *)
(* recursive, but with final depth).                                       *)
(***************************************************************************)
equal(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "equality",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing the "less than" relation of the two expressions     *)
(* (possibly recursive, but with final depth).                             *)
(***************************************************************************)
lt(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "lt",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing the "greater than" relation of the two expressions  *)
(* (possibly recursive, but with final depth).                             *)
(***************************************************************************)
gt(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "gt",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing the "less than or equal" relation of the two        *)
(* expressions (possibly recursive, but with final depth).                 *)
(***************************************************************************)
leq(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "leq",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing the "greater than or equal" relation of the two     *)
(* expressions (possibly recursive, but with final depth).                 *)
(***************************************************************************)
geq(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "geq",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* Record representing the addition of the expressions (possibly           *)
(* recursive, but with final depth).                                       *)
(***************************************************************************)
add(lhs, rhs) ==
    [type |-> "expression",
     subtype |-> "addition",
     lhs |-> lhs,
     rhs |-> rhs]

(***************************************************************************)
(* The values, which we fetch are "literal" records, not just the value.   *)
(* This means that when we fetch them, we are converting an "identifier"   *)
(* record to a "literal" record.                                           *)
(***************************************************************************)
(***************************************************************************)
(*                                                                         *)
(* fetch(M, identifier) ==                                                 *)
(*     CASE identifier.subtype = "number" -> num(M[identifier.variable])   *)
(*       [] identifier.subtype = "string" -> str(M[identifier.variable])   *)
(*       [] OTHER -> Undefined                                             *)
(***************************************************************************)

(***************************************************************************)
(* The values in the memory are "literal" records, not just the value.     *)
(***************************************************************************)
fetch(M, variable) == M[variable]

(***************************************************************************)
(* The eval operator allows us to evaluate expressions recursively and     *)
(* return their value as "literal" records.                                *)
(*                                                                         *)
(* At the bottom of the recursion are the "literal" and "identifier"       *)
(* records.                                                                *)
(* - "literal" records are returned as records                             *)
(* - "identifier" records are mapped to "literal" records with their       *)
(*   current value                                                         *)
(*                                                                         *)
(* Expressions evaluate the left-hand side and the right-hand side         *)
(* recursively and then perform their operation on the results, which are  *)
(* "literal" records. This means that we need to get the values, wrapped   *)
(* in these "literal" records, calculate the new value and then wrap it    *)
(* again as a "literal"-record                                             *)
(***************************************************************************)
RECURSIVE eval(_, _)
eval(M, expr) ==
    CASE expr \in Variables -> fetch(M, expr)
      [] expr.type = "literal" -> expr
      [] expr.type = "expression" -> 
            LET 
                \* Here we do the recursive evaluations, the results are "literal" records
                lhs_literal == eval(M, expr.lhs)
                rhs_literal == eval(M, expr.rhs)
                \* Hier we do the actual evaluation of the unwrapped values
                result_value == 
                    CASE expr.subtype = "equality"  -> lhs_literal.value = rhs_literal.value
                      [] expr.subtype = "lt"        -> lhs_literal.value < rhs_literal.value
                      [] expr.subtype = "gt"        -> lhs_literal.value > rhs_literal.value
                      [] expr.subtype = "leq"       -> lhs_literal.value <= rhs_literal.value
                      [] expr.subtype = "geq"       -> lhs_literal.value >= rhs_literal.value
                      [] expr.subtype = "addition"  -> lhs_literal.value + rhs_literal.value
                      [] OTHER -> Undefined
            \* Here we return the evaluated and wrapped "literal" record
            IN lit(result_value)
      [] OTHER -> Undefined

(***************************************************************************)
(* Declaration step for the representation of threads as lists.  This      *)
(* action encodes the DECL rule from the operational semantics of the      *)
(* calculus.  It checks if the variable is currently undefined in memory   *)
(* as part of its enabling condition.  If this is the case, it updates the *)
(* memory with the variable set to initialized.                            *)
(***************************************************************************)
Decl_ListVersion == 
    /\  \E location \in loc_network : 
            \E action_seq \in location.state :
                 /\ Head(action_seq).type = "action"
                 /\ Head(action_seq).subtype = "var"
                 /\ location.mem[Head(action_seq).var_name] = Undefined
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The declared variable is added to the memory and then set to initialized
                                !.mem = [location.mem EXCEPT ![Head(action_seq).var_name] = Initialized],
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "declare" step
                                !.state = (location.state \ {action_seq}) \union {Tail(action_seq)} ]}

(***************************************************************************)
(* Assign action for the representation of threads as list.  The action    *)
(* encodes the ASSIGN rule from the operational semantics of the calculus. *)
(* It checks if the variable is not equal to undefined (meaning that it    *)
(* has at some point been initialized) as part of its enabling condition.  *)
(* If this is the case, it evaluates the right-hand side of the assign     *)
(* action and updates the memory with so that it contains the evaluated    *)
(* value.  Important: it does not write the value, but the value, wrapped  *)
(* in a "literal" record!                                                  *)
(***************************************************************************)
Assign_ListVersion == 
    /\  \E location \in loc_network : 
            \E action_seq \in location.state :
                 /\ Head(action_seq).type = "action"
                 /\ Head(action_seq).subtype = "assign"
                 /\ Head(action_seq).lhs \in Variables
                 /\ location.mem[Head(action_seq).lhs] # Undefined
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The declared variable is updated in the memory
                                !.mem = [location.mem EXCEPT ![Head(action_seq).lhs] = eval(location.mem, Head(action_seq).rhs)],
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "assign" step
                                !.state = (location.state \ {action_seq}) \union {Tail(action_seq)} ]}

(***************************************************************************)
(* Operator for the flattening of threads.  This operator is central to    *)
(* the way the rules of the operational semantics of the calculus are      *)
(* enabled.  The idea is that we always flatten the top level compositions *)
(* to a bag of threads, that are not compositions.  These threads are      *)
(* guards, conditional threads, instatiations of other threads.            *)
(*                                                                         *)
(* That way we can simply require the existence of a thread in the bag of  *)
(* threads of each location, and we know that we can just look into the    *)
(* thread type and directly decide what we want to execute.  We do not     *)
(* need to look recursively into the subtree of the thread composition and *)
(* check if some thread is enabled there, so the enabling conditions of    *)
(* our actions are easier to write and understand.                         *)
(***************************************************************************)
RECURSIVE flatten_thread_compositions(_)
flatten_thread_compositions(thread) ==
    IF thread.type = "thread" /\ thread.subtype = "composition"
        THEN flatten_thread_compositions(thread.lhs) (+) flatten_thread_compositions(thread.rhs)
        ELSE SetToBag({thread})

(***************************************************************************)
(* Declaration step for the representation of threads as trees.  This      *)
(* action encodes the DECL rule from the operational semantics of the      *)
(* calculus.  It checks if the variable is currently undefined in memory   *)
(* as part of its enabling condition.  If this is the case, it updates the *)
(* memory with the variable set to initialized.                            *)
(***************************************************************************)
Decl == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                 \* Check if the thread is a guard
                 /\ enabled_thread.type = "thread"
                 /\ enabled_thread.subtype = "guard"
                 \* Check if the guard is a prefix 
                 /\ enabled_thread.guard.type = "guard"
                 /\ enabled_thread.guard.subtype = "prefix"
                 \* Check if the prefix is a var
                 /\ enabled_thread.guard.action.type = "action"
                 /\ enabled_thread.guard.action.subtype = "var"
                 \* Check if the variable is undefined in the memory
                 /\ location.mem[enabled_thread.guard.action.var_name] = Undefined
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The declared variable is added to the memory and then set to initialized
                                !.mem = [location.mem EXCEPT ![enabled_thread.guard.action.var_name] = Initialized],
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "declare" step
                                !.threads_bag = 
                                    \* To implement this, we remove the bag with enabled_thread
                                    (location.threads_bag (-) SetToBag({ enabled_thread }))
                                    \* And we add the guarded thread flattened 
                                    (+) flatten_thread_compositions(enabled_thread.guard.thread)
                        ]}
                 /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Assign action for the representation of threads as trees.  The action   *)
(* encodes the ASSIGN rule from the operational semantics of the calculus. *)
(* It checks if the variable is not equal to undefined (meaning that it    *)
(* has at some point been initialized) as part of its enabling condition.  *)
(* If this is the case, it evaluates the right-hand side of the assign     *)
(* action and updates the memory with so that it contains the evaluated    *)
(* value.  Important: it does not write the value, but the value, wrapped  *)
(* in a "literal" record!                                                  *)
(***************************************************************************)
Assign == 
    /\  \E location \in loc_network :
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                 \* Check if the thread is a guard
                 /\ enabled_thread.type = "thread"
                 /\ enabled_thread.subtype = "guard"
                 \* Check if the guard is a prefix 
                 /\ enabled_thread.guard.type = "guard"
                 /\ enabled_thread.guard.subtype = "prefix"
                 \* Check if the prefix is an assign action
                 /\ enabled_thread.guard.action.type = "action"
                 /\ enabled_thread.guard.action.subtype = "assign"
                 \* Check if the left-hand side is a variable
                 /\ enabled_thread.guard.action.lhs \in Variables
                 \* Check if the variable is already initialized in the memory
                 /\ location.mem[enabled_thread.guard.action.lhs] # Undefined
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The variable is updated in the memory
                                !.mem = [location.mem EXCEPT ![enabled_thread.guard.action.lhs] = eval(location.mem, enabled_thread.guard.action.rhs)],
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "assign" step
                                !.threads_bag = 
                                    \* To implement this, we remove the bag with enabled_thread
                                    (location.threads_bag (-) SetToBag({ enabled_thread }))
                                    \* And we add the guarded thread flattened 
                                    (+) flatten_thread_compositions(enabled_thread.guard.thread)
                        ]}
                 /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Receive action for the representation of threads as trees.  The action  *)
(* encodes the RCV rule from the operational semantics of the calculus.    *)
(* Important: the action allows programmers to specify the channel and the *)
(* location, which they want to receive from, as expressions.  This means  *)
(* that we know the channel and location, from which we want to receive,   *)
(* after we evaluate their expressions.                                    *)
(*                                                                         *)
(* This action then checks if there is a message in the bag with messages  *)
(* in transit, which is sent on the channel, on which the receive action   *)
(* wants to receive from, and from the location, from which the receive    *)
(* action wants to receive.                                                *)
(*                                                                         *)
(* If the enabling condition is satisfied, we update the variable,         *)
(* specified in the receive action, with the value, wrapped in the         *)
(* message.  Additionally, we remove the message from the bag with         *)
(* messages in transit.                                                    *)
(***************************************************************************)
Rcv == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                \E matching_msg \in DOMAIN msg_aether :
                    \* Check if the thread is a guard
                    /\ enabled_thread.type = "thread"
                    /\ enabled_thread.subtype = "guard"
                    \* Check if the guard is a prefix 
                    /\ enabled_thread.guard.type = "guard"
                    /\ enabled_thread.guard.subtype = "prefix"
                    \* Check if the prefix is a receive action
                    /\ enabled_thread.guard.action.type = "action"
                    /\ enabled_thread.guard.action.subtype = "receive"
                    /\ eval(location.mem, enabled_thread.guard.action.channel) \in Channels
                    /\ eval(location.mem, enabled_thread.guard.action.channel) = matching_msg.channel
                    /\ eval(location.mem, enabled_thread.guard.action.location) \in Locations
                    /\ eval(location.mem, enabled_thread.guard.action.location) = matching_msg.source
                    \* Check if the variable in the receive action is a variable
                    /\ enabled_thread.guard.action.variable \in Variables
                    \* Check if the variable is already initialized in the memory
                    /\ location.mem[enabled_thread.guard.action.variable] # Undefined
                    /\ loc_network' = 
                            \* We remove the current version of the location from the global state
                            (loc_network \ { location }) \union 
                            \* And add it again with the new state
                            { [location EXCEPT 
                                    \* The variable is updated in the memory
                                    !.mem = [location.mem EXCEPT ![enabled_thread.guard.action.variable] = matching_msg.value],
                                    \* The state of the process, i.e. the threads running it in stay the same
                                    \* except for the thread, which currently executes the "rcv" step
                                    !.threads_bag = 
                                        \* To implement this, we remove the bag with enabled_thread
                                        (location.threads_bag (-) SetToBag({ enabled_thread }))
                                        \* And we add the guarded thread flattened 
                                        (+) flatten_thread_compositions(enabled_thread.guard.thread)
                            ]}
                    /\ msg_aether' = msg_aether (-) SetToBag({ matching_msg })
                    /\ UNCHANGED << thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Suspicion request action for the representation of threads as trees.    *)
(* The action encodes the L-SUSP rule from the operational semantics of    *)
(* the calculus.                                                           *)
(*                                                                         *)
(* Important: the rule works allows a thread to ignore the reception of a  *)
(* message by launching of a suspicion for the intended source location of *)
(* the sender.                                                             *)
(*                                                                         *)
(* This rule checks only checks if the location expression can be          *)
(* evaluated to a location (which should normally be the case) and then    *)
(* simply ignores the receive action.                                      *)
(***************************************************************************)
Susp == 
    /\  \E location \in loc_network :
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\  \E enabled_thread \in DOMAIN location.threads_bag :
                \* Check if the thread is a guard
                /\ enabled_thread.type = "thread"
                /\ enabled_thread.subtype = "guard"
                \* Check if the guard is a prefix 
                /\ enabled_thread.guard.type = "guard"
                /\ enabled_thread.guard.subtype = "prefix"
                \* Check if the prefix is a receive action
                /\ enabled_thread.guard.action.type = "action"
                /\ enabled_thread.guard.action.subtype = "receive"
                /\ eval(location.mem, enabled_thread.guard.action.location) \in Locations
                \* The trusted immortal cannot be suspected as failed!
                /\ eval(location.mem, enabled_thread.guard.action.location) # trim
                /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently "skips" the "rcv" action
                                !.threads_bag = 
                                    \* To implement this, we remove the bag with enabled_thread
                                    (location.threads_bag (-) SetToBag({ enabled_thread }))
                                    \* And we add the guarded thread flattened 
                                    (+) flatten_thread_compositions(enabled_thread.guard.thread)
                        ]}
                /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Send action for the representation of threads as trees.  The action     *)
(* encodes the SND rule from the operational semantics of the calculus.    *)
(* Important: the action allows programmers to specify the target channel, *)
(* the target location and the value as expressions.  This means that we   *)
(* know the actual channel, location and value after we evaluate their     *)
(* expressions.                                                            *)
(*                                                                         *)
(* This action picks one of the message in the outgoing bag and checks if  *)
(* the channel expression can be reduced to a channel, if the location can *)
(* be reduced to a location and if the value can be reduced to a value as  *)
(* part of its enabling condition.                                         *)
(*                                                                         *)
(* If the enabling condition is satisfied, we remove the message from the  *)
(* outgoing bag with messages and we add the evaluated message (with the   *)
(* evaluated channel, the id of the current location, the evaluated target *)
(* location and the evaluated value) to the bag with messages in transit.  *)
(*                                                                         *)
(* Important: If after removing the message from the outgoing bag, the     *)
(* outgoing bag becomes empty, we simply remove the whole send action from *)
(* the thread, executing it.                                               *)
(***************************************************************************)
Snd == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                \* Check if the thread is a guard
                /\ enabled_thread.type = "thread"
                /\ enabled_thread.subtype = "guard"
                \* Check if the guard is a prefix 
                /\ enabled_thread.guard.type = "guard"
                /\ enabled_thread.guard.subtype = "prefix"
                \* Check if the prefix is a send action
                /\ enabled_thread.guard.action.type = "action"
                /\ enabled_thread.guard.action.subtype = "send"
                /\ \E outgoing_msg \in DOMAIN enabled_thread.guard.action.msgs :
                    /\ eval(location.mem, outgoing_msg.channel) \in Channels
                    /\ eval(location.mem, outgoing_msg.target) \in Locations
                    /\ loc_network' = 
                            \* We remove the current version of the location from the global state
                            (loc_network \ { location }) \union 
                            \* And add it again with the new state
                            { [location EXCEPT 
                                    \* The state of the process, i.e. the threads running it in stay the same
                                    \* except for the thread, which currently executes the "snd" step
                                    !.threads_bag = 
                                        \* To implement this, we remove the bag with enabled_thread
                                        (location.threads_bag (-) SetToBag({ enabled_thread }))
                                        \* And we add the guarded thread flattened 
                                        (+) 
                                        \* Here we check what is the cardinatily of the msgs to be send.
                                        \* If it is equal to 1 in the current step, this means that we are 
                                        \* currently sending this one message and there are no more messages
                                        \* to send.  
                                        (IF BagCardinality(enabled_thread.guard.action.msgs) = 1
                                            \* In that case we remove the prefix and continue with
                                            \* the continuation thread. Since we don't know if this is 
                                            \* a composition, we try to flatten it.
                                            THEN flatten_thread_compositions(enabled_thread.guard.thread)
                                            \* Otherwise, we remove the message from the outgoing bag
                                            \* and we keep the send prefix in the thread.
                                            \* So here we need to return changed thread as part of a bag,
                                            \* since we the union expects another bag.
                                            ELSE SetToBag({
                                                [enabled_thread EXCEPT !.guard.action.msgs =
                                                    \* Here we do the actual message removal
                                                    enabled_thread.guard.action.msgs (-) SetToBag({outgoing_msg})
                                                ]
                                            })
                                        )
                            ]}
                    \* Here we add the evaluated message to the outgoing bag
                    /\ msg_aether' = 
                            msg_aether 
                            (+) 
                            SetToBag({ 
                                msg(
                                    eval(location.mem, outgoing_msg.channel),
                                    \* [type |-> "literal", value |-> location.id],
                                    eval(location.mem, location.id),
                                    eval(location.mem, outgoing_msg.target),
                                    eval(location.mem, outgoing_msg.value)
                                ) 
                            })
                    /\ UNCHANGED << thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Conditional step for the representation of threads as trees.  This      *)
(* action encodes the TRUE rule from the operational semantics of the      *)
(* calculus.  It checks if the expression inside of the conditional thread *)
(* can be evaluated to TRUE.  If this is the case, it substitues the       *)
(* conditional thread with its "then" thread in the state of the location. *)
(***************************************************************************)
True == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                 \* Check if the thread is a conditional thread
                 /\ enabled_thread.type = "thread"
                 /\ enabled_thread.subtype = "conditional"
                 \* Check if the condition can be evaluated to TRUE
                 /\ eval(location.mem, enabled_thread.condition) = [type |-> "literal", value |-> TRUE]
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "TRUE" step
                                !.threads_bag = 
                                    \* To implement this, we remove the bag with enabled_thread
                                    (location.threads_bag (-) SetToBag({ enabled_thread }))
                                    \* And we add the flattened then_thread  
                                    (+) flatten_thread_compositions(enabled_thread.then_thread)
                        ]}
                 /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Conditional step for the representation of threads as trees.  This      *)
(* action encodes the FALSE rule from the operational semantics of the     *)
(* calculus.  It checks if the expression inside of the conditional thread *)
(* can be evaluated to FALSE.  If this is the case, it substitues the      *)
(* conditional thread with its "else" thread in the state of the location. *)
(***************************************************************************)
False == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                 \* Check if the thread is a conditional thread
                 /\ enabled_thread.type = "thread"
                 /\ enabled_thread.subtype = "conditional"
                 \* Check if the condition can be evaluated to FALSE
                 /\ eval(location.mem, enabled_thread.condition) = [type |-> "literal", value |-> FALSE]
                 /\ loc_network' = 
                        \* We remove the current version of the location from the global state
                        (loc_network \ { location }) \union 
                        \* And add it again with the new state
                        { [location EXCEPT 
                                \* The state of the process, i.e. the threads running it in stay the same
                                \* except for the thread, which currently executes the "FALSE" step
                                !.threads_bag = 
                                    \* To implement this, we remove the bag with enabled_thread
                                    (location.threads_bag (-) SetToBag({ enabled_thread }))
                                    \* And we add the flattened then_thread  
                                    (+) flatten_thread_compositions(enabled_thread.else_thread)
                        ]}
                 /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* Indetifier step for the representation of threads as trees.  This       *)
(* action encodes the IDENT rule from the operational semantics of the     *)
(* calculus.  It checks if there is a defining equation for the identifier *)
(* in the thr_defs variable.  If there is a defining equation and all free *)
(* variables, mentioned in the equation, are defined in the memory of the  *)
(* location, this action simply substitues the found identifier with the   *)
(* thread body, provided in the definition.  The check for the free        *)
(* variables ensures that they are dynamically captured.                   *)
(***************************************************************************)
Ident == 
    /\  \E location \in loc_network : 
        /\ trim # Undefined
        /\ location.id \notin failed_locs
        /\ \E enabled_thread \in DOMAIN location.threads_bag :
                 \* Check if the thread is a conditional thread
                 /\ enabled_thread.type = "thread"
                 /\ enabled_thread.subtype = "identifier"
                 \* Check if the identifier is defined in thr_def
                 /\ \E thr_def \in thr_defs : 
                        /\ enabled_thread.thr_identifier = thr_def.name
                        \* Check if all free variables of the defined thread are defined in the memory of the location
                        /\ \A idx \in 1..Len(thr_def.free_vars) : location.mem[thr_def.free_vars[idx]] # Undefined
                        /\ loc_network' = 
                               \* We remove the current version of the location from the global state
                               (loc_network \ { location }) \union 
                               \* And add it again with the new state
                               { [location EXCEPT 
                                       \* The state of the process, i.e. the threads running it in stay the same
                                       \* except for the thread, which currently executes the "IDENT" step
                                       !.threads_bag = 
                                           \* To implement this, we remove the bag with enabled_thread
                                           (location.threads_bag (-) SetToBag({ enabled_thread }))
                                           \* We wrap the thread body, which itself is a guard,
                                           \* in a thread of type guard 
                                           \* and add it to the threads of the location
                                           (+) SetToBag({ thr_guard(thr_def.thr_body) })
                               ]}
                        /\ UNCHANGED << msg_aether, thr_defs, trim, failed_locs >>

(***************************************************************************)
(* This action encodes the TRIM rule from the operational semantics of the *)
(* calculus.  It checks if the "trusted immortal" for the current          *)
(* execution is undefined.  If this is the case, it chooses one of the     *)
(* locations as "trusted immportal" randomly and saves it into the trim    *)
(* variable.                                                               *)
(***************************************************************************)
Trim == 
    /\ trim = Undefined
    /\ trim' \in Locations 
    /\ UNCHANGED << loc_network, msg_aether, thr_defs, failed_locs >>

(***************************************************************************)
(* This action encodes the FAIL rule from the operational semantics of the *)
(* calculus.  It checks if the "trusted immortal" has already been chosen  *)
(* and then picks a new location "k" from the rest of the locations, which *)
(* have not already failed and are not equal to the trusted immortal, and  *)
(* adds it to the set of failed locations.                                 *)
(***************************************************************************)
Fail == 
    /\ trim # Undefined
    /\ \E k \in (Locations \ (failed_locs \union { trim })) :
        /\ failed_locs' = failed_locs \union { k }
        /\ UNCHANGED << loc_network, msg_aether, thr_defs, trim >>

(***************************************************************************)
(* All variables, which are used by terms of the calculus, need to be      *)
(* declared as TLA+ constants.  These variables are records with field     *)
(* "type" equal to "identifier", and field "name" equal to the string      *)
(* represenation of the variable symbol.                                   *)
(***************************************************************************)
CONSTANTS x, y, z

\* ASSUME Variables = {x, y, z}

(***************************************************************************)
(* The channels, which are used by terms of the calculus, also need to be  *)
(* declared as TLA+ constants.  These channels are records with field      *)
(* "type" equal to "identifier", and field "name" equal to the string      *)
(* representation of the channel symbol.                                   *)
(***************************************************************************)
CONSTANTS c

(***************************************************************************)
(* The thread identifiers, which are defined for use in the calculus, are  *)
(* also declared as TLA+ constants.  These thread identifiers are records  *)
(* with field "type" equal to "identifier", and field "name" equal to the  *)
(* string representation of the thread identifier symbol.                  *)
(***************************************************************************)
CONSTANTS L

(***************************************************************************)
(* Some small examples, which show the "syntax" of the encoding of the     *)
(* calculus in TLA+ as lists.                                              *)
(***************************************************************************)
as_list_t1 == << var(x), var(y), Nil >>
\* t2 == << var(x), assign(x, 10), Nil >>
as_list_t2 == << var(x), assign(x, num(10)), Nil >>
as_list_t3 == << var(x), assign(x, num(10)), assign(x, add(x, num(1))), Nil >>

as_list_loc_1 == loc("loc_1", [X \in Variables |-> Undefined], { as_list_t3 })

\*t3 == << var(x), var(y), Nil >>
\*t5 == comp(<< var(x), var(y), Nil >>, << var(x), assign(x,x+1), Nil >>)
\*t4 == prefix(var(x), prefix(var(x), Nil))
\*var_x_var_y_0 == <<[type |-> "decl", var |-> x], [var |-> y], Nil>>
\*var_x_assign_x_x_plus_1_0 == <<[var |-> x], [type |-> "assign", lhs |-> "x", rhs |-> "x+1"], Nil>>

(***************************************************************************)
(* Thread that declares two variables (x, y) seqentially.                  *)
(***************************************************************************)
as_tree_t1 ==
    thr_guard(prefix(var(x),
        thr_guard(prefix(var(y),
            thr_guard(Nil)
        ))
    ))

as_tree_t1_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t1))

(***************************************************************************)
(* Thread that declares the variable x and then assigns 10 to it.          *)
(***************************************************************************)
as_tree_t2 ==
    thr_guard(prefix(var(x),
        thr_guard(prefix(assign(x, num(10)),
            thr_guard(Nil)
        ))
     ))
     
as_tree_t2_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t2))

(***************************************************************************)
(* Thread that declares the variable x, assigns 10 to it and then          *)
(* increments the variable by 1.                                           *)
(***************************************************************************)
as_tree_t3 == 
    thr_guard(prefix(var(x),
        thr_guard(prefix(assign(x, num(10)),
            thr_guard(prefix(assign(x, add(x, num(1))),
                thr_guard(Nil)
            ))
        ))
    ))

as_tree_t3_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t3))

(***************************************************************************)
(* Composition thread: one thread declares the variable x and assigns the  *)
(* value 10 to it.  The other thread declares the variable y.              *)
(***************************************************************************)
as_tree_t4 == 
    thr_comp(
         thr_guard(prefix(var(x),
            thr_guard(prefix(assign(x, num(10)),
                thr_guard(Nil)
            ))
         )),
         thr_guard(prefix(var(y),
            thr_guard(Nil)
         ))
    )

as_tree_t4_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t4))

(***************************************************************************)
(* Composition thread: One thread declares the variable x and assigns the  *)
(* value 10 to it.  The other thread declares the value y and assigns the  *)
(* value 100 to it.                                                        *)
(***************************************************************************)
as_tree_t5 == 
    thr_comp(
       thr_guard(prefix(var(x),
           thr_guard(prefix(assign(x, num(10)),
               thr_guard(Nil)
           ))
        )),
        thr_guard(prefix(var(y),
           thr_guard(prefix(assign(y, num(100)),
               thr_guard(Nil)
           ))
        ))
    )
as_tree_t5_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t5))

(***************************************************************************)
(* Composition thread: One thread is itself a composition of two           *)
(* sequential threads (both of these declare a variable and assign a value *)
(* to if).  The other thread is a sequential thread that itself declares a *)
(* variable and assigns a value to it.                                     *)
(***************************************************************************)
as_tree_t6 ==
    thr_comp(
        thr_comp(
           thr_guard(prefix(var(x),
               thr_guard(prefix(assign(x, num(10)),
                   thr_guard(Nil)
               ))
            )),
            thr_guard(prefix(var(y),
               thr_guard(prefix(assign(y, num(100)),
                   thr_guard(Nil)
               ))
            ))
        ),
        thr_guard(prefix(var(z),
           thr_guard(prefix(assign(z, num(1000)),
               thr_guard(Nil)
           ))
        ))
    )

as_tree_t6_loc_0 == loc(lit("loc_0"), [X \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t6))

(***************************************************************************)
(* Thread that declares a variable and then sets the variable through a    *)
(* receive action.                                                         *)
(***************************************************************************)
as_tree_t7 ==
    thr_guard(prefix(var(x),
        thr_guard(prefix(rcv(c, lit("loc_0"), x),
            thr_guard(Nil)
        ))
    ))

(***************************************************************************)
(* Message in transit.  Sent on channel c, from location 0 to location 1,  *)
(* carrying the value 20.                                                  *)
(***************************************************************************)
msg_on_c_from_loc0_to_loc1 == 
    msg(c, lit("loc_0"), lit("loc_1"), num(20))

(***************************************************************************)
(* Location 1, all variables are undefined, flattens the compositions from *)
(* the root thread, so that they are ready to be picked up by the rules.   *)
(***************************************************************************)
as_tree_t7_loc_1 == loc(lit("loc_1"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t7))

(***************************************************************************)
(* Location 0, which is sending a single message on channel c to location  *)
(* 1, containing the value 30.                                             *)
(***************************************************************************)
as_tree_t8 ==
    thr_guard(
        prefix(snd(SetToBag({
            out_msg(c, lit("loc_1"), num(20))
        })),
        thr_guard(Nil)
    ))
    
as_tree_t8_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t8))

(***************************************************************************)
(* Location 0, which is sending a two messages on channel c to location 1. *)
(* One of the message contains the value "first_message", the other        *)
(* "second_message".                                                       *)
(***************************************************************************)
as_tree_t9 ==
    thr_guard(
        prefix(snd(SetToBag({
            out_msg(c, lit("loc_1"), str("first_message")),
            out_msg(c, lit("loc_1"), str("second_message"))
        })),
        thr_guard(Nil)
    ))
    
as_tree_t9_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t9))

(***************************************************************************)
(* Location 0, which is sending a two messages on channel c to location 1. *)
(* Both messages are exactly the same!                                     *)
(***************************************************************************)
as_tree_t10 ==
    thr_guard(
        prefix(snd(
            SetToBag({ out_msg(c, lit("loc_1"), str("first_message")) })
            (+)
            SetToBag({ out_msg(c, lit("loc_1"), str("first_message")) })
        ),
        thr_guard(Nil)
    ))
    
as_tree_t10_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t10))

(***************************************************************************)
(* Simple conditional thread.  Depending on the condition 0 = 0 (which is  *)
(* always true), the thread can continue as the thread, which declares the *)
(* variable x in the "then" case, or continue as the thread, which         *)
(* declares the variable y in the "else" case.                             *)
(***************************************************************************)
as_tree_t11 ==
    thr_cond(
        equal(num(0), num(0)),
        thr_guard(prefix(var(x),
            thr_guard(Nil)
        )),
        thr_guard(prefix(var(y),
            thr_guard(Nil)
        ))
    )

as_tree_t11_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t11))

(***************************************************************************)
(* Simple conditional thread.  Depending on the condition 0 = 1 (which is  *)
(* always false), the thread can continue as the thread, which declares    *)
(* the variable x in the "then" case, or continue as the thread, which     *)
(* declares the variable y in the "else" case.                             *)
(***************************************************************************)
as_tree_t12 ==
    thr_cond(
        equal(num(0), num(1)),
        thr_guard(prefix(var(x),
            thr_guard(Nil)
        )),
        thr_guard(prefix(var(y),
            thr_guard(Nil)
        ))
    )

as_tree_t12_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t12))

(***************************************************************************)
(* Simple thread identifier definition.  For the definition, we need to    *)
(* provide the name of the defined thread, we need to declare its free     *)
(* variables and we need to define its body.                               *)
(*                                                                         *)
(* In this simple case, the list with free variables is empty and from the *)
(* thread body, we simply declare a new variable x and then halt.          *)
(***************************************************************************)
as_tree_simple_thr_def ==
    thr_ident_def(
        L,
        <<>>,
        prefix(var(x),
            thr_guard(Nil)
        )
    )

(***************************************************************************)
(* Thread that simply instatiates a thread, given by an thread identifier. *)
(***************************************************************************)
as_tree_t13 ==
    thr_ident(L)

as_tree_t13_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t13))

(***************************************************************************)
(* Simple thread identifier definition.  For the definition, we need to    *)
(* provide the name of the defined thread, we need to declare its free     *)
(* variables and we need to define its body.                               *)
(*                                                                         *)
(* In this simple case, the list with free variables contains only x and   *)
(* from the thread body, we simply assgin the value 10 to the variable x   *)
(* and then halt.                                                          *)
(***************************************************************************)
as_tree_thr_def_that_changes_var ==
    thr_ident_def(
        L,
        <<x>>,
        prefix(
            assign(x, num(10)),
            thr_guard(Nil)
        )
    )

(***************************************************************************)
(* Thread that declares a variable x and then instatiates another thread,  *)
(* given by an thread identifier, which assigns the value 10 to the        *)
(* variable x.                                                             *)
(***************************************************************************)
as_tree_t14 ==
    thr_guard(
        prefix(
            var(x),
            thr_ident(L)
        )
    )

as_tree_t14_loc_0 == loc(lit("loc_0"), [variable \in Variables |-> Undefined], flatten_thread_compositions(as_tree_t14))

(***************************************************************************)
(* Consensus algorithm from the "Store Locally, Prove Globally" paper.     *)
(***************************************************************************)
CONSTANTS input_seq
CONSTANTS chan, r, output

M_bot == [variable \in Variables |-> Undefined]

N == Len(input_seq)

def_of_L(l) ==
    thr_ident_def(
        ident(l),
        << chan, x, r, output >>,
        prefix(
            assign(r, add(r, lit(1))),
            thr_cond(
                leq(r, lit(N)),
                thr_cond(
                    equal(lit(l), r),
                    thr_guard(
                        prefix(
                            snd( SetToBag({ out_msg(chan, lit(j), x) : j \in (1..N \ {l}) }) ),
                            thr_ident(ident(l))
                        )
                    ),
                    thr_guard(
                        prefix(
                            rcv(c, r, x),
                            thr_ident(ident(l))
                        )
                    )
                ),
                thr_guard(
                    prefix(
                        assign(output, x),
                        Nil
                    )
                )
            )
        )
    )

all_defs == { def_of_L(l) : l \in 1..Len(input_seq) }

Consensus(input) ==
    { loc(lit(l), [M_bot EXCEPT ![chan] = c,
                                ![x] = input[l],
                                ![r] = lit(0),
                                ![output] =  Initialized], flatten_thread_compositions(thr_ident(ident(l))) ) 
    : l \in 1..Len(input) }
    
(***************************************************************************)
(* The initial state formula, which composes the initial state of the      *)
(* system (declared in the calculus).                                      *)
(***************************************************************************)
Init == /\ loc_network = Consensus(input_seq)
        /\ msg_aether = SetToBag({ })
        /\ thr_defs = all_defs
        /\ trim = Undefined
        /\ failed_locs = { }

(***************************************************************************)
(* The next state relation, which simply states that the next states are   *)
(* generated by the rules of the operational semantics of the calculus.    *)
(***************************************************************************)
Next == Trim \/ Fail \/ Decl \/ Assign \/ Rcv \/ Susp \/ Snd \/ True \/ False \/ Ident

Spec == Init /\ [][Next]_vars

FairSpec == Spec /\ WF_vars(Next)

=============================================================================
\* Modification History
\* Last modified Tue Aug 12 10:15:33 CEST 2025 by todormoskov
\* Created Mon Aug 04 15:06:46 CEST 2025 by todormoskov
