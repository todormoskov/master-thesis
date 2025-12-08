-------------- MODULE EPC_2_Decl_Assign_1ProcNThds_1ProcNThds --------------
EXTENDS Integers, Sequences, Bags

CONSTANTS N

(* --EPC DeclaringVariablesAndAssigningValuesToThem3

(p = "P") [[ [] <| 
    (j \in 1..N) (
        p_j_1: var x;
        p_j_2: x := 10;
    )
]] 
||
(q = "Q") [[ [] <|
    (j \in 1..N) (
        q_j_1: var x;
        q_j_2: x := 10;
    )
]]
*)

CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == {"P"} \union {"Q"}
ThreadSet == [self \in ProcSet |-> CASE self = "P" -> 1..N 
                                   []   self = "Q" -> 1..N]

Init == /\ pc = [self \in ProcSet |-> CASE self = "P" -> [thd \in ThreadSet[self] |-> "p_j_1D"]
                                      []   self = "Q" -> [thd \in ThreadSet[self] |-> "q_j_1D"]]
        /\ x = [self \in ProcSet |-> Undefined]

\* Location p = "P", thread j \in 1..N
p_j_1D(self, thd) == /\ pc[self][thd] = "p_j_1D"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_j_2A"]

p_j_2A(self, thd) == /\ pc[self][thd] = "p_j_2A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1D(self, thd) \/ p_j_2A(self, thd)

\* Location q = "Q", thread j \in 1..N
q_j_1D(self, thd) == /\ pc[self][thd] = "q_j_1D"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "q_j_2A"]

q_j_2A(self, thd) == /\ pc[self][thd] = "q_j_2A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

q_j(self, thd) == q_j_1D(self, thd) \/ q_j_2A(self, thd)


Terminating == /\ \A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done" 
               /\ UNCHANGED vars

pNext == \E thd \in ThreadSet["P"] : p_j("P", thd)

qNext == \E thd \in ThreadSet["Q"] : q_j("Q", thd)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(\A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done")


=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:00:11 CET 2025 by todormoskov
\* Created Sun Dec 07 15:56:25 CET 2025 by todormoskov
