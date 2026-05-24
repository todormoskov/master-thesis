------------------------ MODULE Example_10_FailStop ------------------------
EXTENDS Integers, Sequences, Bags

(* --EPC (FailStop) Example_10_FailStop
chan c

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

VARIABLES F, AE, pc, x, y

vars == <<F, AE, pc, x, y>>

ProcSet == {1} \union {2}

ThreadSet == [self \in ProcSet |-> CASE self = 1 -> {1}
                                   []   self = 2 -> {1}]

ChanSet == {c}

Init == /\ F = {}
        /\ AE = [_chan \in ChanSet |-> CASE _chan = c -> EmptyBag]
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
                     /\ UNCHANGED <<F, x, y>>

p_t(self, thd) == p_t_1S(self, thd)

p == \E self \in {1} \ F:
       \E thd \in ThreadSet[self]:
         p_t(self, thd)

\* Location q = 2, thread t = 1
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
                       /\ UNCHANGED <<F, x>>

q_t(self, thd) == q_t_2Rcv(self, thd)

q == \E self \in {2} \ F:
       \E thd \in ThreadSet[self]:
         q_t(self, thd)

Env == \E f \in SUBSET (ProcSet \ F):
         /\ f # {}
         /\ F' = F \cup f
         /\ UNCHANGED <<AE, pc, x, y>>

Next == \/ p
        \/ q
        \/ Env

Spec == Init /\ [][Next]_vars


\* END TRANSLATION


=============================================================================
\* Modification History
\* Last modified Sun May 24 16:06:31 CEST 2026 by todormoskov
\* Created Sun May 24 16:06:01 CEST 2026 by todormoskov
