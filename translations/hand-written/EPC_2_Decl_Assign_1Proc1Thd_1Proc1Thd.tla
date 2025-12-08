--------------- MODULE EPC_2_Decl_Assign_1Proc1Thd_1Proc1Thd ---------------
EXTENDS Integers, Sequences, Bags

(* --EPC DeclaringVariablesAndAssigningValuesToThem1

(p = "P") [[ 
    [] <| 
    (j = 1) (
        p_j_1: var x;
        p_j_2: x := 10;
        p_j_3: x := x + 1
    )
]] 
||
(q = "Q") [[
    [] <|
    (j = 1) (
        q_j_1: var x;
        q_j_2: x := 10
        q_j_3: x := x + 1
    ) 
]]

*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> {1}
                                   []   self = "Q" -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> "p_j_1"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> "q_j_1"]]
        /\ x = [self \in ProcSet |-> Undefined]

\* Location P, thread j
p_j_1(self, thd) == /\ pc[self][thd] = "p_j_1"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "p_j_2"]

p_j_2(self, thd) == /\ pc[self][thd] = "p_j_2"
                    /\ x[self] # Undefined
                    /\ x' = [x EXCEPT ![self] = 10]
                    /\ pc' = [pc EXCEPT ![self][thd] = "p_j_3"]

p_j_3(self, thd) == /\ pc[self][thd] = "p_j_3"
                    /\ x[self] # Undefined
                    /\ x' = [x EXCEPT ![self] = x[self] + 1]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p(self, thd) == p_j_1(self, thd) \/ p_j_2(self, thd) \/ p_j_3(self, thd)

\* Location Q, thread j
q_j_1(self, thd) == /\ pc[self][thd] = "q_j_1"
                    /\ x[self] = Undefined
                    /\ x' = [x EXCEPT ![self] = Initialized]
                    /\ pc' = [pc EXCEPT ![self][thd] = "q_j_2"]

q_j_2(self, thd) == /\ pc[self][thd] = "q_j_2"
                    /\ x[self] # Undefined
                    /\ x' = [x EXCEPT ![self] = 10]
                    /\ pc' = [pc EXCEPT ![self][thd] = "q_j_3"]

q_j_3(self, thd) == /\ pc[self][thd] = "q_j_3"
                    /\ x[self] # Undefined
                    /\ x' = [x EXCEPT ![self] = x[self] + 1]
                    /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q(self, thd) == q_j_1(self, thd) \/ q_j_2(self, thd) \/ q_j_3(self, thd) 

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
\* Last modified Sun Dec 07 17:59:43 CET 2025 by todormoskov
\* Created Sun Dec 07 13:54:50 CET 2025 by todormoskov
