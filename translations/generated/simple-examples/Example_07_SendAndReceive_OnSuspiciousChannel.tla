----------- MODULE Example_07_SendAndReceive_OnSuspiciousChannel -----------
EXTENDS Integers, Sequences, Bags

(* --EPC Example_07_SendAndReceive_OnSuspiciousChannel
suspchan c

(p = 1) [[
    [x |-> "Hello"]
    <|
    (t = 1) (
        {{ c@2<< x >> }}
    )
]]
||
(q = 2) [[
    [y |-> Initialized]
    <|
    (t = 1) (
        c@1?(y)
    )
]]

*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized, c

VARIABLES AE, D, pc, x, y

vars == <<AE, D, pc, x, y>>

ProcSet == {1} \union {2}

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}
                                   []   self = 2 -> {1}]

ChanSet == {c}

SuspChanSet == {c}

Init == /\ AE = [_chan \in ChanSet |-> CASE _chan = c -> EmptyBag]
        /\ D = [self \in ProcSet |-> [_suspchan \in SuspChanSet |-> [_p \in ProcSet |-> TRUE]]]
        /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_t_1S"]
                                      []   self = 2 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "q_t_2R"]]
        /\ x = [self \in ProcSet |-> CASE self = 1 -> "Hello"
                                     [] OTHER -> Undefined]
        /\ y = [self \in ProcSet |-> CASE self = 2 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p = 1, thread t = 1
p_t_1S(self, thd) == /\ pc[self][thd] = "p_t_1S"
                     /\ c \in ChanSet
                     /\ 2 \in ProcSet
                     /\ AE' = [AE EXCEPT ![c] = AE[c] (+) SetToBag({<<self, 2, x[self]>>})]
                     /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                     /\ UNCHANGED <<D, x, y>>

p_t(self, thd) == p_t_1S(self, thd)

p == \E self \in {1}:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* Location q = 2, thread t = 1
q_t_2Rcv(self, thd) == /\ pc[self][thd] = "q_t_2R"
                       /\ c \in ChanSet \ SuspChanSet
                       /\ 1 \in ProcSet
                       /\ y[self] # Undefined
                       /\ \E msg \in DOMAIN AE[c]:
                            /\ msg[1] = 1
                            /\ msg[2] = self
                            /\ y' = [y EXCEPT ![self] = msg[3]]
                            /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg})]
                       /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                       /\ UNCHANGED <<D, x>>

q_t_2RcvS(self, thd) == /\ pc[self][thd] = "q_t_2R"
                        /\ c \in SuspChanSet
                        /\ 1 \in ProcSet
                        /\ y[self] # Undefined
                        /\ \E msg1 \in DOMAIN AE[c]:
                             /\ msg1[1] = 1
                             /\ msg1[2] = self
                             /\ D' = [D EXCEPT ![self][c][1] = TRUE]
                             /\ y' = [y EXCEPT ![self] = msg1[3]]
                             /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg1})]
                        /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                        /\ UNCHANGED <<x>>

q_t_2Susp(self, thd) == /\ pc[self][thd] = "q_t_2R"
                        /\ c \in SuspChanSet
                        /\ 1 \in ProcSet
                        /\ D' = [D EXCEPT ![self][c][1] = FALSE]
                        /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                        /\ UNCHANGED <<AE, x, y>>

q_t(self, thd) == \/ q_t_2Rcv(self, thd)
                  \/ q_t_2RcvS(self, thd)
                  \/ q_t_2Susp(self, thd)

q == \E self \in {2}:
       \E thd \in ThreadSet[self]:
         q_t(self, thd)

Next == \/ p
        \/ q

Spec == Init /\ [][Next]_vars


\* END TRANSLATION


=============================================================================
\* Modification History
\* Last modified Sun May 24 16:01:08 CEST 2026 by todormoskov
\* Created Sun May 24 16:00:05 CEST 2026 by todormoskov
