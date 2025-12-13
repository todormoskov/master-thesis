-------------------------- MODULE EPC_5_IfThenElse --------------------------
EXTENDS Integers, Sequences, Bags

(* --EPC IfThenElse
(p = 1) [[ 
    [status |-> 404, message |-> ""]
    <| 
    (j = 1) (
        p_j_1C: if status = 404
        p_j_2A:     then << message := "Not Found" >>
        p_j_3A:     else << message := "Unknown Status Code" >>
                fi;
    )
]] 

*)
CONSTANTS Undefined, Initialized
VARIABLES status, message, pc

vars == << status, message, pc >>

ProcSet == { 1 }
ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}]

Init == /\ status = [self \in ProcSet |-> CASE self = 1 -> 402]
        /\ message = [self \in ProcSet |-> CASE self = 1 -> ""]
        /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> "p_j_1C"]]

\* Process p = 1, thread j = 1
p_j_1T(self, thd) == /\ pc[self][thd] = "p_j_1C"
                     /\ status[self] = 404
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_j_2A"]
                     /\ UNCHANGED << status, message >>

p_j_2A(self, thd) == /\ pc[self][thd] = "p_j_2A"
                     /\ message[self] # Undefined
                     /\ message' = [message EXCEPT ![self] = "Not Found"]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED << status >>

p_j_1F(self, thd) == /\ pc[self][thd] = "p_j_1C"
                     /\ ~(status[self] = 404)
                     /\ pc' = [pc EXCEPT ![self][thd] = "p_j_3A"]
                     /\ UNCHANGED << status, message >>

p_j_3A(self, thd) == /\ pc[self][thd] = "p_j_3A"
                     /\ message[self] # Undefined
                     /\ message' = [message EXCEPT ![self] = "Unknown Status Code"]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED << status >>

p_j(self, thd) == p_j_1T(self, thd) \/ p_j_2A(self, thd) \/ p_j_1F(self, thd) \/ p_j_3A(self, thd)

p == p_j(1, 1)

Terminating == /\ pc[1][1] = "Done"
               /\ UNCHANGED vars

Next == p

Spec == Init /\ [][Next]_vars
         
Termination == <>(pc[1][1] = "Done")


=============================================================================
\* Modification History
\* Last modified Mon Dec 08 11:33:55 CET 2025 by todormoskov
\* Created Sun Dec 07 18:56:24 CET 2025 by todormoskov
