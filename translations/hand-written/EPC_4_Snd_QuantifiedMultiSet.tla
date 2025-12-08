-------------------- MODULE EPC_4_Snd_QuantifiedMultiSet --------------------
EXTENDS Integers, Sequences, Bags

CONSTANT N
(* --EPC Snd_QuantifiedMultiSet
new c

(p = 1) [[ 
    [] 
    <| 
    (j = 1) (
        {{ (j \in 1..N) c@j!<< j >> }}
    )
]] 

*)
CONSTANTS Undefined, Initialized
VARIABLES pc, c, O

vars == << pc, c, O >>

ProcSet == { 1 }
ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> "p_j_1S"]]
        /\ c = EmptyBag
        /\ O = [self \in ProcSet |-> 
                 CASE self = 1 -> 
                   [thd \in ThreadSet[self] |->
                     [p_j_1S |-> 1..N] \* set with indices of the elements that need to be send with this action
                   ]
               ]

\* Process p = 1, thread j = 1
p_j_1S(self, thd) == /\ pc[self][thd] = "p_j_1S"
                     /\ \E j \in O[self][thd]["p_j_1S"] :
                        /\ c' = c (+) SetToBag({<< self, j, j >>})
                        /\ O' = IF O[self][thd]["p_j_1S"] # { j }
                                  THEN [O EXCEPT ![self][thd]["p_j_1S"] = @ \ { j } ]
                                  ELSE [O EXCEPT ![self][thd]["p_j_1S"] = 1..N ]
                        /\ pc' = IF O[self][thd]["p_j_1S"] # { j } 
                                  THEN pc
                                  ELSE [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1S(self, thd)

p == p_j(1, 1)

Terminating == /\ pc[1][1] = "Done"
               /\ UNCHANGED vars

Next == p

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc[1][1] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:54:36 CET 2025 by todormoskov
\* Created Sun Dec 07 18:43:27 CET 2025 by todormoskov
