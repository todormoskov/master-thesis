------------- MODULE EPC_4_Snd_ExplicitMultiSet_WithOneElement -------------
EXTENDS Integers, Sequences, Bags

(* --EPC SendingAn_ExplicitMultiSet_WithOneElement
new c

(p = 1) [[ 
    [] 
    <| 
    (j = 1) (
        {{ c@2!"Hello" }}
    )
]] 

*)
CONSTANTS Undefined, Initialized
VARIABLES pc, c

vars == << pc, c >>

ProcSet == { 1 }
ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> "p_j_1S"]]
        /\ c = EmptyBag

\* Process p = 1, thread j = 1
p_j_1S(self, thd) == /\ pc[self][thd] = "p_j_1S"
                     /\ c' = c (+) SetToBag({<< 1, 2, "Hello" >>})
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1S(self, thd)

pNext == p_j(1, 1)

Terminating == /\ pc[1][1] = "Done"
               /\ UNCHANGED vars

Next == pNext

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc[1][1] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:54:09 CET 2025 by todormoskov
\* Created Sun Dec 07 18:06:00 CET 2025 by todormoskov
