------------------- MODULE EPC_1_Decl_1Proc1Thd_1Proc1Thd -------------------
EXTENDS Integers, Sequences, Bags

(* --EPC DeclaringVariables1

(p = "P") [[ [] <| (j = 1) (var x) ]] 
||
(q = "Q") [[ [] <| (j = 1) (var x) ]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1}
                                   []   self = "Q" -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> "Lbl_1"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> "Lbl_2"]]
        /\ x = [self \in ProcSet |-> Undefined]

\* Process p = "P", thread j = 1
Lbl_1(self, thd) == /\ pc[self][thd] = "Lbl_1"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p(self, thd) == Lbl_1(self, thd)

\* Process q = "Q", thread j = 1
Lbl_2(self, thd) == /\ pc[self][thd] = "Lbl_2"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q(self, thd) == Lbl_2(self, thd)

Terminating == /\ pc["P"][1] = "Done"
               /\ pc["Q"][1] = "Done" 
               /\ UNCHANGED vars

pNext == p("P", 1)

qNext == q("Q", 1)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc["P"][1] = "Done" /\ pc["Q"][1] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 17:58:57 CET 2025 by todormoskov
\* Created Sun Dec 07 16:41:55 CET 2025 by todormoskov
