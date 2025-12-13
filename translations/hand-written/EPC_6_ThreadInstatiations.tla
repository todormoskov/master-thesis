--------------------- MODULE EPC_6_ThreadInstatiations ---------------------
EXTENDS Integers, Sequences, Bags

(* --EPC ThreadInstantiations

def assignXAndStop(x) = << x := 10 >>; << x := x + 1>>

(p = 1) [[ 
    [x |-> Initialized]
    <| 
    (j = 1) (
        p_j_1I: assignXAndStop()
    )
]] 

*)
CONSTANTS Undefined, Initialized
VARIABLES x, pc

vars == << x, pc >>

ProcSet == { 1 }
ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ x = [self \in ProcSet |-> CASE self = 1 -> Initialized]
        /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> "p_j_1I"]]

\* Process p = 1, thread j = 1
p_j_1I(self, thd) == /\ pc[self][thd] = "p_j_1I"
                     /\ x[self] # Undefined
                     /\ pc' = [pc EXCEPT ![self][thd] = "assignXAndStop_1A"]
                     /\ UNCHANGED << x >>

p_j(self, thd) == p_j_1I(self, thd)

p == p_j(1, 1)

\* def assignXAndStop(x) = << x := 10 >>; << x := x + 1>>
assignXAndStop_1A(self, thd) == /\ pc[self][thd] = "assignXAndStop_1A"
                                /\ x[self] # Undefined
                                /\ x' = [x EXCEPT ![self] = 10]
                                /\ pc' = [pc EXCEPT ![self][thd] = "assignXAndStop_2A"]

assignXAndStop_2A(self, thd) == /\ pc[self][thd] = "assignXAndStop_2A"
                                /\ x[self] # Undefined
                                /\ x' = [x EXCEPT ![self] = x[self] + 1]
                                /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

assignXAndStop == \E self \in ProcSet : \E thd \in ThreadSet[self] : assignXAndStop_1A(self, thd) \/ assignXAndStop_2A(self, thd)

Terminating == /\ pc[1][1] = "Done"
               /\ UNCHANGED vars

Next == p \/ assignXAndStop

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc[1][1] = "Done")

=============================================================================
\* Modification History
\* Last modified Mon Dec 08 12:01:20 CET 2025 by todormoskov
\* Created Mon Dec 08 11:43:01 CET 2025 by todormoskov
