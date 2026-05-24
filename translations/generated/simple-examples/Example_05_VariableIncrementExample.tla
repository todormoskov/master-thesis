---------------- MODULE Example_05_VariableIncrementExample ----------------
EXTENDS Integers, Sequences

(* --EPC Example_05_VariableIncrementExample

(p = 1) [[
    [x |-> Initialized]
    <|
    (t = 1) (
        assign0: << x := 0 >>;
        callThread: Increment()
    )
]]

def Increment(x) = 
    if x < 10 
        then << x := x + 1 >>; Increment()
        else {{ }}
    fi
    
*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized

VARIABLES pc, ra, x

vars == <<pc, ra, x>>

ProcSet == {1}

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "assign0"]]
        /\ ra = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> << >>]]
        /\ x = [self \in ProcSet |-> CASE self = 1 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p = 1, thread t = 1
assign0(self, thd) == /\ pc[self][thd] = "assign0"
                      /\ x[self] # Undefined
                      /\ x' = [x EXCEPT ![self] = 0]
                      /\ pc' = [pc EXCEPT ![self][thd] = "callThread"]
                      /\ UNCHANGED <<ra>>

callThread(self, thd) == /\ pc[self][thd] = "callThread"
                         /\ x[self] # Undefined
                         /\ pc' = [pc EXCEPT ![self][thd] = "Increment_1C"]
                         /\ UNCHANGED <<ra, x>>

p_t(self, thd) == \/ assign0(self, thd)
                  \/ callThread(self, thd)

p == \E self \in {1}:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* def Increment(x)
Increment_1T(self, thd) == /\ pc[self][thd] = "Increment_1C"
                           /\ x[self] < 10
                           /\ pc' = [pc EXCEPT ![self][thd] = "Increment_2A"]
                           /\ UNCHANGED <<ra, x>>

Increment_1F(self, thd) == /\ pc[self][thd] = "Increment_1C"
                           /\ ~(x[self] < 10)
                           /\ pc' = [pc EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                                THEN Head(ra[self][thd])
                                                                ELSE "Done"]
                           /\ ra' = [ra EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                                THEN Tail(ra[self][thd])
                                                                ELSE ra[self][thd]]
                           /\ UNCHANGED <<x>>

Increment_2A(self, thd) == /\ pc[self][thd] = "Increment_2A"
                           /\ x[self] # Undefined
                           /\ x' = [x EXCEPT ![self] = x[self] + 1]
                           /\ pc' = [pc EXCEPT ![self][thd] = "Increment_3I"]
                           /\ UNCHANGED <<ra>>

Increment_3I(self, thd) == /\ pc[self][thd] = "Increment_3I"
                           /\ x[self] # Undefined
                           /\ pc' = [pc EXCEPT ![self][thd] = "Increment_1C"]
                           /\ UNCHANGED <<ra, x>>

Increment == \E self \in ProcSet:
               \E thd \in ThreadSet[self]:
                 \/ Increment_1T(self, thd)
                 \/ Increment_1F(self, thd)
                 \/ Increment_2A(self, thd)
                 \/ Increment_3I(self, thd)

Next == \/ p
        \/ Increment

Spec == Init /\ [][Next]_vars


\* END TRANSLATION

=============================================================================
\* Modification History
\* Last modified Sun May 24 15:56:19 CEST 2026 by todormoskov
\* Created Sun May 24 15:55:13 CEST 2026 by todormoskov
