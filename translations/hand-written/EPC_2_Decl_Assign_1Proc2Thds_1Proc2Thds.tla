-------------- MODULE EPC_2_Decl_Assign_1Proc2Thds_1Proc2Thds --------------
EXTENDS Integers, Sequences, Bags

(* --EPC DeclaringVariablesAndAssigningValuesToThem2

(p = "P") [[ 
    [] 
    <|
    (j = 1) (
        p_j_1: var x;
        p_j_2: x := 10;
    ) 
    | (k = 2) (
        p_k_1: var x;
        p_k_2: x := 10;
    ) 
]]
||
(q = "Q") [[ [] <| 
    (j = 1) (
        q_j_1: var x;
        q_j_2: x := 10;
    ) 
    | (k = 2) (
        q_k_1: var x;
        q_k_2: x := 10;
    ) 
]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1, 2}
                                   []   self = "Q" -> {1, 2}]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_j_1"
                                                                                      []   thd = 2 -> "p_k_1"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "q_j_1"
                                                                                      []   thd = 2 -> "q_k_1"] ]
        /\ x = [self \in ProcSet |-> Undefined]

\* Location p = "P", thread j = 1
p_j_1D(self, thd) == /\ pc[self][thd] = "p_j_1"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_j_2"]

p_j_2A(self, thd) == /\ pc[self][thd] = "p_j_2"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1D(self, thd) \/ p_j_2A(self, thd)

\* Location p = "P", thread k = 2
p_k_1D(self, thd) == /\ pc[self][thd] = "p_k_1"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_k_2"]

p_k_2A(self, thd) == /\ pc[self][thd] = "p_k_2"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_k(self, thd) == p_k_1D(self, thd) \/ p_k_2A(self, thd)

\* Location p = "P", threads j = 1 or k = 2
p(self, thd) == p_j(self, thd) \/ p_k(self, thd)

\* Location q = "Q", thread j = 1 
q_j_1D(self, thd) == /\ pc[self][thd] = "q_j_1"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "q_j_2"]

q_j_2A(self, thd) == /\ pc[self][thd] = "q_j_2"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q_j(self, thd) == q_j_1D(self, thd) \/ q_j_2A(self, thd)

\* Location q = "Q", thread k = 2
q_k_1D(self, thd) == /\ pc[self][thd] = "q_k_1"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "q_k_2"]

q_k_2A(self, thd) == /\ pc[self][thd] = "q_k_2"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q_k(self, thd) == q_k_1D(self, thd) \/ q_k_2A(self, thd)

\* Location q = "Q", threads j = 1 or k = 2
q(self, thd) == q_j(self, thd) \/ q_k(self, thd)

Terminating == /\ pc["P"][1] = "Done"
               /\ pc["P"][2] = "Done"
               /\ pc["Q"][2] = "Done"
               /\ pc["Q"][2] = "Done" 
               /\ UNCHANGED vars

pNext == p("P", 1) \/ p("P", 2)

qNext == q("Q", 1) \/ q("Q", 2)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc["P"][1] = "Done" /\ pc["P"][2] = "Done" /\ pc["Q"][1] = "Done" /\ pc["Q"][2] = "Done")


=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:00:03 CET 2025 by todormoskov
\* Created Sun Dec 07 15:23:04 CET 2025 by todormoskov
