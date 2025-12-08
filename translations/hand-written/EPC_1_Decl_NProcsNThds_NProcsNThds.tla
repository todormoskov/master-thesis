----------------- MODULE EPC_1_Decl_NProcsNThds_NProcsNThds -----------------
EXTENDS Integers, Sequences, Bags

CONSTANTS N

(* --EPC DeclaringVariables4

(p \in 1..N) [[ [] <| (j \in 1..N) (var x) ]] 
||
(q \in (N+1)..(2*N)) [[ [] <| (j \in (N+1)..(2*N)) (var x) ]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == 1..N \union (N+1)..(2*N)
ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> 1..N 
                                   []   self \in (N+1)..(2*N) -> (N+1)..(2*N)]

Init == /\ pc = [self \in ProcSet |-> CASE self \in 1..N -> [thd \in ThreadSet[self] |-> "Lbl_1"]
                                      []   self \in (N+1)..(2*N) -> [thd \in ThreadSet[self] |-> "Lbl_2"]]
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

pNext == \E self \in 1..N: \E thd \in ThreadSet[self] : p(self, thd)

qNext == \E self \in (N+1)..(2*N): \E thd \in ThreadSet[self] : q(self, thd)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(\A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 17:59:25 CET 2025 by todormoskov
\* Created Sun Dec 07 16:44:48 CET 2025 by todormoskov
