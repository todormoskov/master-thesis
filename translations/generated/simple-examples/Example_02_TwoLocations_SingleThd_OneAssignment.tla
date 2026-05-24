---------- MODULE Example_02_TwoLocations_SingleThd_OneAssignment ----------
EXTENDS Integers, Sequences

(* --EPC Example_02_TwoLocations_SingleThd_OneAssignment

(p = 1) [[
    [x |-> Initialized]
    <|
    (t = 1) (
        << x := 0 >>
    )
]]
|| 
(q = 2) [[
    [x |-> Initialized]
    <|
    (t = 1) (
        << x := 0 >>
    )
]]

*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized

VARIABLES pc, x

vars == <<pc, x>>

ProcSet == {1} \union {2}

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}
                                   []   self = 2 -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_t_1A"]
                                      []   self = 2 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "q_t_2A"]]
        /\ x = [self \in ProcSet |-> CASE self = 1 -> Initialized
                                     []   self = 2 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p = 1, thread t = 1
p_t_1A(self, thd) == /\ pc[self][thd] = "p_t_1A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 0]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED << >>

p_t(self, thd) == p_t_1A(self, thd)

p == \E self \in {1}:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* Location q = 2, thread t = 1
q_t_2A(self, thd) == /\ pc[self][thd] = "q_t_2A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 0]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED << >>

q_t(self, thd) == q_t_2A(self, thd)

q == \E self \in {2}:
       \E thd \in ThreadSet[self]:
         q_t(self, thd)

Next == \/ p
        \/ q

Spec == Init /\ [][Next]_vars


\* END TRANSLATION

=============================================================================
\* Modification History
\* Last modified Sun May 24 15:49:27 CEST 2026 by todormoskov
\* Created Sun May 24 15:48:55 CEST 2026 by todormoskov
