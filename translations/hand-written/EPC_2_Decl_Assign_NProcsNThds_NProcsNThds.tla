------------- MODULE EPC_2_Decl_Assign_NProcsNThds_NProcsNThds -------------
EXTENDS Integers, Sequences, Bags

CONSTANTS N
(* --EPC DeclaringVariablesAndAssigningValuesToThem4

(p \in 1..N) [[ [] <| 
    (j \in 1..N) (
        p_j_1: var x;
        p_j_2: x := 10;
    )
]] 
||
(q \in (N+1)..(2*N)) [[ [] <|
    (j \in (N+1)..(2*N)) (
        q_j_1: var x;
        q_j_2: x := 10;
    )
]]
*)
CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == 1..N \union (N+1)..(2*N)
ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> 1..N 
                                   []   self \in (N+1)..(2*N) -> (N+1)..(2*N)]

Init == /\ pc = [self \in ProcSet |-> CASE self \in 1..N -> [thd \in ThreadSet[self] |-> "p_j_1D"]
                                      []   self \in (N+1)..(2*N) -> [thd \in ThreadSet[self] |-> "q_j_1D"]]
        /\ x = [self \in ProcSet |-> Undefined]

\* Location p \in 1..N, thread j \in 1..N
p_j_1D(self, thd) == /\ pc[self][thd] = "p_j_1D"
                     /\ x[self] = Undefined
                     /\ x' = [x EXCEPT ![self] = Initialized]
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_j_2A"]

p_j_2A(self, thd) == /\ pc[self][thd] = "p_j_2A"
                     /\ x[self] # Undefined
                     /\ x' = [x EXCEPT ![self] = 10]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1D(self, thd) \/ p_j_2A(self, thd)

\* Location q \in (N+1)..(2*N), thread j \in 1..N
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

pNext == \E self \in 1..N: \E thd \in ThreadSet[self] : p_j(self, thd)

qNext == \E self \in (N+1)..(2*N): \E thd \in ThreadSet[self] : q_j(self, thd)

Next == pNext \/ qNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(\A self \in ProcSet: \A thd \in ThreadSet[self]: pc[self][thd] = "Done")


=============================================================================
\* Modification History
\* Last modified Sun Dec 07 18:00:18 CET 2025 by todormoskov
\* Created Sun Dec 07 16:11:47 CET 2025 by todormoskov
