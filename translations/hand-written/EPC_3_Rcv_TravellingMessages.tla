---------------------- MODULE EPC_3_Rcv_TravellingMessages ----------------------
EXTENDS Integers, Sequences, Bags

CONSTANT N
(* --EPC ReceivingTravellingMessages
new c

(p \in 1..N) [[ 
    [x |-> 0]
    <| 
    (j = 1) (
        p_j_1: c@0?x
    )
]]
|| {{ (j \in 1..N) c_{0 -> j}:j }}

*)
CONSTANTS Undefined, Initialized
VARIABLES x, pc, c

vars == << x, pc, c >>

ProcSet == 1..N
ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> {1}]

Init == /\ pc = [self \in ProcSet |-> CASE self \in 1..N -> [thd \in ThreadSet[self] |-> "p_j_1"]]
        /\ x = [self \in ProcSet |-> CASE self \in 1..N -> 0]
        /\ c = EmptyBag (+) SetToBag({ << 0, j, j >> : j \in 1..N }) (* initial message aether *)

\* Location p \in 1..N, thread j
p_j_1(self, thd) == /\ pc[self][thd] = "p_j_1"
                    /\ x[self] # Undefined   (* the variable from the receive action is defined in the location *)
                    /\ \E msg \in DOMAIN c : (* and there is a message in the channel with *)
                        /\ msg[1] = 0        (* a matching sender, *)
                        /\ msg[2] = self     (* and a maching receiver. *)
                        /\ x' = [x EXCEPT ![self] = msg[3]] (* Then we write the payload in the variable *)
                        /\ c' = c (-) SetToBag({ msg })     (* And remove the message from the channel *)
                        /\ pc' = [pc EXCEPT ![self][thd] = "Done"]

p_j(self, thd) == p_j_1(self, thd)

pNext == \E self \in 1..N : p_j(self, 1)

Terminating == /\ \A self \in ProcSet: \A thd \in ThreadSet[self] : pc[self][thd] = "Done"
               /\ UNCHANGED vars

Next == pNext \/ Terminating

Spec == Init /\ [][Next]_vars
         
Termination == <>(\A self \in ProcSet: \A thd \in ThreadSet[self] : pc[self][thd] = "Done")


=============================================================================
\* Modification History
\* Last modified Mon Dec 08 10:22:01 CET 2025 by todormoskov
\* Created Sun Dec 07 16:46:06 CET 2025 by todormoskov
