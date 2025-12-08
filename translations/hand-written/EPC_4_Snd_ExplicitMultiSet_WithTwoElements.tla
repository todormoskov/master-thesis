------------- MODULE EPC_4_Snd_ExplicitMultiSet_WithTwoElements -------------
EXTENDS Integers, Sequences, Bags

(* --EPC Sending_AnExplicitMultiSet_WithTwoElements
new c

(p = 1) [[ 
    [] 
    <| 
    (j = 1) (
        {{ c@2!"Hello", c@3!"Hello" }}
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
                     [p_j_1S |-> 1..2] \* set with indices of the elements that need to be send with this action
                   ]
               ]

\* Process p = 1, thread j = 1
p_j_1S(self, thd) == /\ pc[self][thd] = "p_j_1S"
                     /\ \E i \in O[self][thd]["p_j_1S"] :
                        /\ c' = CASE i = 1 -> c (+) SetToBag({<< 1, 2, "Hello" >>})
                                []   i = 2 -> c (+) SetToBag({<< 1, 3, "Hello" >>})
                        /\ O' = IF O[self][thd]["p_j_1S"] # { i }
                                  THEN [O EXCEPT ![self][thd]["p_j_1S"] = @ \ { i } ]
                                  ELSE [O EXCEPT ![self][thd]["p_j_1S"] = 1..2 ]
                        /\ pc' = IF O[self][thd]["p_j_1S"] # { i } 
                                  THEN pc
                                  ELSE [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1S(self, thd)

pNext == p_j(1, 1)

Terminating == /\ pc[1][1] = "Done"
               /\ UNCHANGED vars

Next == pNext

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc[1][1] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:51:27 CET 2025 by todormoskov
\* Created Sun Dec 07 18:16:32 CET 2025 by todormoskov
