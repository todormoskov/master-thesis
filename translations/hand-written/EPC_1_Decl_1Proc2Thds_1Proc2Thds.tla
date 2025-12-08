------------------ MODULE EPC_1_Decl_1Proc2Thds_1Proc2Thds ------------------
EXTENDS Integers, Sequences, Bags

(* --EPC DeclaringVariables2

(p = "P") [[ [] <| (j = 1) (Lbl_1: var x) | (k = 2) (Lbl_2: var x) ]]
||
(q = "Q") [[ [] <| (j = 1) (Lbl_3: var x) | (k = 2) (Lbl_4: var x) ]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1, 2}
                                   []   self = "Q" -> {1, 2}]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "Lbl_1"
                                                                                      []   thd = 2 -> "Lbl_2"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "Lbl_3"
                                                                                      []   thd = 2 -> "Lbl_4"] ]
        /\ x = [self \in ProcSet |-> Undefined]

Lbl_1(self, thd) == /\ pc[self][thd] = "Lbl_1"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == Lbl_1(self, thd)

Lbl_2(self, thd) == /\ pc[self][thd] = "Lbl_2"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_k(self, thd) == Lbl_2(self, thd)

p(self, thd) == p_j(self, thd) \/ p_k(self, thd)

Lbl_3(self, thd) == /\ pc[self][thd] = "Lbl_3"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q_j(self, thd) == Lbl_3(self, thd)

Lbl_4(self, thd) == /\ pc[self][thd] = "Lbl_4"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q_k(self, thd) == Lbl_4(self, thd)

q(self, thd) == q_j(self, thd) \/ q_k(self, thd)

Terminating == /\ pc["P"][1] = "Done"
               /\ pc["P"][2] = "Done"
               /\ pc["Q"][1] = "Done"
               /\ pc["Q"][2] = "Done" 
               /\ UNCHANGED vars

pNext == p("P", 1) \/ p("P", 2)

qNext == q("Q", 1) \/ q("Q", 2)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc["P"][1] = "Done" /\ pc["P"][2] = "Done" /\ pc["Q"][1] = "Done" /\ pc["Q"][2] = "Done")

=============================================================================
\* Modification History
\* Last modified Sun Dec 07 19:27:06 CET 2025 by todormoskov
\* Created Sun Dec 07 16:43:24 CET 2025 by todormoskov
