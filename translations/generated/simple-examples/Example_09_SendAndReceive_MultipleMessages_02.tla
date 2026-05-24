----------- MODULE Example_09_SendAndReceive_MultipleMessages_02 -----------
EXTENDS Integers, Sequences, Bags

(* --EPC Example_09_SendAndReceive_MultipleMessages_02
chan c

(p = 1) [[
    [x |-> "Hello"]
    <|
    (t = 1) (
        {{ (j \in 2..4) c@j<< x >> }}
    )
]]
||
(q \in 2..4) [[
    [y |-> Initialized]
    <|
    (t = 1) (
        c@1?(y)
    )
]]

*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized, c

VARIABLES AE, O, pc, x, y

vars == <<AE, O, pc, x, y>>

ProcSet == {1} \union 2..4

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}
                                   []   self \in 2..4 -> {1}]

ChanSet == {c}

Init == /\ AE = [_chan \in ChanSet |-> CASE _chan = c -> EmptyBag]
        /\ O = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> {}]]
        /\ pc = [self \in ProcSet |-> CASE self = 1 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "p_t_1S"]
                                      []   self \in 2..4 -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "q_t_2R"]]
        /\ x = [self \in ProcSet |-> CASE self = 1 -> "Hello"
                                     [] OTHER -> Undefined]
        /\ y = [self \in ProcSet |-> CASE self \in 2..4 -> Initialized
                                     [] OTHER -> Undefined]

\* Location p = 1, thread t = 1
p_t_1S(self, thd) == /\ pc[self][thd] = "p_t_1S"
                     /\ \E j \in 2..4 \ O[self][thd]:
                          /\ c \in ChanSet
                          /\ j \in ProcSet
                          /\ AE' = [AE EXCEPT ![c] = AE[c] (+) SetToBag({<<self, j, x[self]>>})]
                          /\ pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup {j} # 2..4
                                                               THEN pc[self][thd]
                                                               ELSE "Done"]
                          /\ O' = [O EXCEPT ![self][thd] = IF O[self][thd] \cup {j} # 2..4
                                                             THEN O[self][thd] \cup {j}
                                                             ELSE {}]
                     /\ UNCHANGED <<x, y>>

p_t(self, thd) == p_t_1S(self, thd)

p == \E self \in {1}:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* Location q \in 2..4, thread t = 1
q_t_2Rcv(self, thd) == /\ pc[self][thd] = "q_t_2R"
                       /\ c \in ChanSet
                       /\ 1 \in ProcSet
                       /\ y[self] # Undefined
                       /\ \E msg \in DOMAIN AE[c]:
                            /\ msg[1] = 1
                            /\ msg[2] = self
                            /\ y' = [y EXCEPT ![self] = msg[3]]
                            /\ AE' = [AE EXCEPT ![c] = AE[c] (-) SetToBag({msg})]
                       /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                       /\ UNCHANGED <<O, x>>

q_t(self, thd) == q_t_2Rcv(self, thd)

q == \E self \in 2..4:
       \E thd \in ThreadSet[self]:
         q_t(self, thd)

Next == \/ p
        \/ q

Spec == Init /\ [][Next]_vars


\* END TRANSLATION


=============================================================================
\* Modification History
\* Last modified Sun May 24 16:04:54 CEST 2026 by todormoskov
\* Created Sun May 24 16:04:13 CEST 2026 by todormoskov
