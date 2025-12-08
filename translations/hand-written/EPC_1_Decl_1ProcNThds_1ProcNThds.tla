------------------ MODULE EPC_1_Decl_1ProcNThds_1ProcNThds ------------------
EXTENDS Integers, Sequences, Bags

CONSTANTS N

(* --EPC DeclaringVariables3

(p = "P") [[ [] <| (j \in 1..N) (var x) ]] 
||
(q = "Q") [[ [] <| (j \in 1..N) (var x) ]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> 1..N 
                                   []   self = "Q" -> 1..N]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> "Lbl_1"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> "Lbl_2"]]
        /\ x = [self \in ProcSet |-> Undefined]

Lbl_1(self, thd) == /\ pc[self][thd] = "Lbl_1"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p(self, thd) == Lbl_1(self, thd)

Lbl_2(self, thd) == /\ pc[self][thd] = "Lbl_2"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q(self, thd) == Lbl_2(self, thd)

Terminating == /\ \A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done" 
               /\ UNCHANGED vars

pNext == \E thd \in ThreadSet["P"] : p("P", thd)

qNext == \E thd \in ThreadSet["Q"] : q("Q", thd)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(\A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 17:59:16 CET 2025 by todormoskov
\* Created Sun Dec 07 16:44:09 CET 2025 by todormoskov
