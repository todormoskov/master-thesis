------------ MODULE Example_01_SingleLoc_SingleThd_OneAssignment ------------
EXTENDS Integers, Sequences

(* --EPC Example_01_SingleLoc_SingleThd_OneAssignment

(p = 1) [[
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

ProcSet == {1}

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_t_1A"]]
        /\ x = [self \in ProcSet |-> CASE self = 1 -> Initialized
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

Next == p

Spec == Init /\ [][Next]_vars


\* END TRANSLATION

=============================================================================
\* Modification History
\* Last modified Sun May 24 15:45:49 CEST 2026 by todormoskov
\* Created Sun May 24 15:45:12 CEST 2026 by todormoskov
