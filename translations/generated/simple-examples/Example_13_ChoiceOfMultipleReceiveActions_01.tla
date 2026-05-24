------------ MODULE Example_13_ChoiceOfMultipleReceiveActions_01 ------------
EXTENDS Integers, Sequences, Bags

(* --EPC Example_13_ChoiceOfMultipleReceiveActions_01
chan c

(p \in 1..3) [[
    [x |-> self]
    <|
    (t = 1) (
        {{ c@4<< x >> }}
    )
]]
||
(q = 4) [[
    [y |-> Initialized]
    <|
    (t = 1) (
        c@1?(y) + c@2?(y) + c@3?(y)
    )
]]

*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized, c

VARIABLES AE, pc, x, y

vars == <<AE, pc, x, y>>

ProcSet == 1..3 \union {4}

ThreadSet == [self \in ProcSet |-> CASE self \in 1..3 -> {1}
                                   []   self = 4 -> {1}]

ChanSet == {c}

Init == /\ AE = [_chan \in ChanSet |-> CASE _chan = c -> EmptyBag]
        /\ pc = [self \in ProcSet |-> CASE self \in 1..3 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_t_1S"]
                                      []   self = 4 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "q_t_2R"]]
        /\ x = [self \in ProcSet |-> CASE self \in 1..3 -> self
                                     [] OTHER -> Undefined]
        /\ y = [self \in ProcSet |-> CASE self = 4 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p \in 1..3, thread t = 1
p_t_1S(self, thd) == /\ pc[self][thd] = "p_t_1S"
                     /\ c \in ChanSet
                     /\ 4 \in ProcSet
                     /\ AE' = [AE EXCEPT ![c] = AE[c] (+) SetToBag({<<self, 4, x[self]>>})]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED <<x, y>>

p_t(self, thd) == p_t_1S(self, thd)

p == \E self \in 1..3:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* Location q = 4, thread t = 1
q_t_2_1Rcv(self, thd) == /\ pc[self][thd] = "q_t_2R"
                         /\ c \in ChanSet
                         /\ 1 \in ProcSet
                         /\ y[self] # Undefined
                         /\ \E msg \in DOMAIN AE[c]:
                              /\ msg[1] = 1
                              /\ msg[2] = self
                              /\ y' = [y EXCEPT ![self] = msg[3]]
                              /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                         /\ UNCHANGED <<x>>

q_t_2_2Rcv(self, thd) == /\ pc[self][thd] = "q_t_2R"
                         /\ c \in ChanSet
                         /\ 2 \in ProcSet
                         /\ y[self] # Undefined
                         /\ \E msg1 \in DOMAIN AE[c]:
                              /\ msg1[1] = 2
                              /\ msg1[2] = self
                              /\ y' = [y EXCEPT ![self] = msg1[3]]
                              /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg1})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                         /\ UNCHANGED <<x>>

q_t_2_3Rcv(self, thd) == /\ pc[self][thd] = "q_t_2R"
                         /\ c \in ChanSet
                         /\ 3 \in ProcSet
                         /\ y[self] # Undefined
                         /\ \E msg2 \in DOMAIN AE[c]:
                              /\ msg2[1] = 3
                              /\ msg2[2] = self
                              /\ y' = [y EXCEPT ![self] = msg2[3]]
                              /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg2})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                         /\ UNCHANGED <<x>>

q_t(self, thd) == \/ q_t_2_1Rcv(self, thd)
                  \/ q_t_2_2Rcv(self, thd)
                  \/ q_t_2_3Rcv(self, thd)

q == \E self \in {4}:
       \E thd \in ThreadSet[self]:
         q_t(self, thd)

Next == \/ p
        \/ q

Spec == Init /\ [][Next]_vars


\* END TRANSLATION


=============================================================================
\* Modification History
\* Last modified Sun May 24 16:21:19 CEST 2026 by todormoskov
\* Created Sun May 24 16:19:29 CEST 2026 by todormoskov
