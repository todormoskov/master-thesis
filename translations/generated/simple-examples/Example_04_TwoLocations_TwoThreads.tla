----------------- MODULE Example_04_TwoLocations_TwoThreads -----------------
EXTENDS Integers, Sequences

(* --EPC Example_04_TwoLocations_TwoThreads

(p \in 1..2) [[
    [x |-> Initialized]
    <|
    (t \in 1..2) (
        << x := 0 >>
    )
]]

*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized

VARIABLES pc, x

vars == <<pc, x>>

ProcSet == 1..2

ThreadSet == [self \in ProcSet |-> CASE self \in 1..2 -> 1..2]

Init == /\ pc = [self \in ProcSet |-> CASE self \in 1..2 -> [thd \in ThreadSet[self] |-> CASE thd \in 1..2 -> "p_t_1A"]]
        /\ x = [self \in ProcSet |-> CASE self \in 1..2 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p \in 1..2, thread t \in 1..2
p_t_1A(self, thd) == /\ pc[self][thd] = "p_t_1A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 0]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED << >>

p_t(self, thd) == p_t_1A(self, thd)

p == \E self \in 1..2:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

Next == p

Spec == Init /\ [][Next]_vars


\* END TRANSLATION


=============================================================================
\* Modification History
\* Last modified Sun May 24 15:53:55 CEST 2026 by todormoskov
\* Created Sun May 24 15:53:04 CEST 2026 by todormoskov
