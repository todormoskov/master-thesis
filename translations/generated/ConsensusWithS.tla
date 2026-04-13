--------------------------- MODULE ConsensusWithS ---------------------------
EXTENDS Integers, Sequences, Bags

CONSTANT input
N == Len(input)

(* --EPC (FailStop, WeakAccuracy, Terminating, Termination, NextWF) Consensus
suspchan c

(p \in 1..N) [[
    [channel |-> c,
     x |-> input[self],
     r |-> 1,
     output |-> Initialized]
    <|
    (j = 1) ( 
        run: L();
        out: << output := x >>
    )
]]

def L(channel, x, r, output) = 
  go: if r <= N 
        then role: if self = r 
                     then mcast: {{ (j \in 1..N : j # self) channel@j<< x >> }} 
                     else  wait: channel@r?(x)
                   fi;
              inc: << r := r + 1 >>;
             loop: L()
      fi
*)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized, c

VARIABLES TI, F, AE, O, D, pc, ra, channel, x, r, output

vars == <<TI, F, AE, O, D, pc, ra, channel, x, r, output>>

ProcSet == 1..N

ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> {1}]

ChanSet == {c}

SuspChanSet == {c}

suspectOK(ti, f, suspected, suspector) == ti # {} /\ suspected \notin ti

Init == /\ TI = {}
        /\ F = {}
        /\ AE = [_chan \in ChanSet |-> CASE _chan = c -> EmptyBag]
        /\ O = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> {}]]
        /\ D = [self \in ProcSet |-> [_suspchan \in SuspChanSet |-> [_p \in ProcSet |-> TRUE]]]
        /\ pc = [self \in ProcSet |-> CASE self \in 1..N -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "run"]]
        /\ ra = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> << >>]]
        /\ channel = [self \in ProcSet |-> CASE self \in 1..N -> c
                                           [] OTHER -> Undefined]
        /\ x = [self \in ProcSet |-> CASE self \in 1..N -> input[self]
                                     [] OTHER -> Undefined]
        /\ r = [self \in ProcSet |-> CASE self \in 1..N -> 1
                                     [] OTHER -> Undefined]
        /\ output = [self \in ProcSet |-> CASE self \in 1..N -> Initialized
                                          [] OTHER -> Undefined]

\* Location p \in 1..N, thread j = 1
run(self, thd) == /\ pc[self][thd] = "run"
                  /\ channel[self] # Undefined
                  /\ x[self] # Undefined
                  /\ r[self] # Undefined
                  /\ output[self] # Undefined
                  /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                  /\ ra' = [ra EXCEPT ![self][thd] = <<"out">> \o ra[self][thd]]
                  /\ UNCHANGED <<TI, F, AE, O, D, channel, x, r, output>>

out(self, thd) == /\ pc[self][thd] = "out"
                  /\ output[self] # Undefined
                  /\ output' = [output EXCEPT ![self] = x[self]]
                  /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, r>>

p_j(self, thd) == \/ run(self, thd)
                  \/ out(self, thd)

p == \E self \in 1..N \ F:
       \E thd \in ThreadSet[self]:
         p_j(self, thd)

\* def L(channel, x, r, output)
goT(self, thd) == /\ pc[self][thd] = "go"
                  /\ r[self] <= N
                  /\ pc' = [pc EXCEPT ![self][thd] = "role"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, r, output>>

goF(self, thd) == /\ pc[self][thd] = "go"
                  /\ ~(r[self] <= N)
                  /\ pc' = [pc EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Head(ra[self][thd])
                                                       ELSE "Done"]
                  /\ ra' = [ra EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Tail(ra[self][thd])
                                                       ELSE ra[self][thd]]
                  /\ UNCHANGED <<TI, F, AE, O, D, channel, x, r, output>>

roleT(self, thd) == /\ pc[self][thd] = "role"
                    /\ self = r[self]
                    /\ pc' = [pc EXCEPT ![self][thd] = "mcast"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, r, output>>

roleF(self, thd) == /\ pc[self][thd] = "role"
                    /\ ~(self = r[self])
                    /\ pc' = [pc EXCEPT ![self][thd] = "wait"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, r, output>>

mcast(self, thd) == /\ pc[self][thd] = "mcast"
                    /\ \E j \in {j1 \in 1..N: j1 # self} \ O[self][thd]:
                         /\ channel[self] \in ChanSet
                         /\ j \in ProcSet
                         /\ AE' = [AE EXCEPT ![channel[self]] = AE[channel[self]] (+) SetToBag({<<self, j, x[self]>>})]
                         /\ pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup {j} # {j1 \in 1..N: j1 # self}
                                                              THEN pc[self][thd]
                                                              ELSE "inc"]
                         /\ O' = [O EXCEPT ![self][thd] = IF O[self][thd] \cup {j} # {j1 \in 1..N: j1 # self}
                                                            THEN O[self][thd] \cup {j}
                                                            ELSE {}]
                    /\ UNCHANGED <<TI, F, D, ra, channel, x, r, output>>

waitRcv(self, thd) == /\ pc[self][thd] = "wait"
                      /\ channel[self] \in ChanSet \ SuspChanSet
                      /\ r[self] \in ProcSet
                      /\ x[self] # Undefined
                      /\ \E msg \in DOMAIN AE[channel[self]]:
                           /\ msg[1] = r[self]
                           /\ msg[2] = self
                           /\ x' = [x EXCEPT ![self] = msg[3]]
                           /\ AE' = [AE EXCEPT ![channel[self]] = AE[channel[self]] (-) SetToBag({msg})]
                      /\ pc' = [pc EXCEPT ![self][thd] = "inc"]
                      /\ UNCHANGED <<TI, F, O, D, ra, channel, r, output>>

waitRcvS(self, thd) == /\ pc[self][thd] = "wait"
                       /\ channel[self] \in SuspChanSet
                       /\ r[self] \in ProcSet
                       /\ x[self] # Undefined
                       /\ \E msg1 \in DOMAIN AE[channel[self]]:
                            /\ msg1[1] = r[self]
                            /\ msg1[2] = self
                            /\ D' = [D EXCEPT ![self][channel[self]][r[self]] = TRUE]
                            /\ x' = [x EXCEPT ![self] = msg1[3]]
                            /\ AE' = [AE EXCEPT ![channel[self]] = AE[channel[self]] (-) SetToBag({msg1})]
                       /\ pc' = [pc EXCEPT ![self][thd] = "inc"]
                       /\ UNCHANGED <<TI, F, O, ra, channel, r, output>>

waitSusp(self, thd) == /\ pc[self][thd] = "wait"
                       /\ channel[self] \in SuspChanSet
                       /\ r[self] \in ProcSet
                       /\ suspectOK(TI, F, r[self], self)
                       /\ D' = [D EXCEPT ![self][channel[self]][r[self]] = FALSE]
                       /\ pc' = [pc EXCEPT ![self][thd] = "inc"]
                       /\ UNCHANGED <<TI, F, AE, O, ra, channel, x, r, output>>

inc(self, thd) == /\ pc[self][thd] = "inc"
                  /\ r[self] # Undefined
                  /\ r' = [r EXCEPT ![self] = r[self] + 1]
                  /\ pc' = [pc EXCEPT ![self][thd] = "loop"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, output>>

loop(self, thd) == /\ pc[self][thd] = "loop"
                   /\ channel[self] # Undefined
                   /\ x[self] # Undefined
                   /\ r[self] # Undefined
                   /\ output[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, channel, x, r, output>>

L == \E self \in ProcSet \ F:
       \E thd \in ThreadSet[self]:
         \/ goT(self, thd)
         \/ goF(self, thd)
         \/ roleT(self, thd)
         \/ roleF(self, thd)
         \/ mcast(self, thd)
         \/ waitRcv(self, thd)
         \/ waitRcvS(self, thd)
         \/ waitSusp(self, thd)
         \/ inc(self, thd)
         \/ loop(self, thd)

Env == \E ti, f \in SUBSET (ProcSet \ (TI \cup F)):
         /\ ti \cap f = {}
         /\ TI' = TI \cup ti
         /\ F' = F \cup f
         /\ UNCHANGED <<AE, O, D, pc, ra, channel, x, r, output>>

Terminating == /\ \A self \in ProcSet \ F:
                    \A thd \in ThreadSet[self]:
                      pc[self][thd] = "Done"
               /\ UNCHANGED vars

Next == \/ p
        \/ L
        \/ Env
        \/ Terminating

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

Termination == <>(\A self \in ProcSet \ F:
                    \A thd \in ThreadSet[self]:
                      pc[self][thd] = "Done")


\* END TRANSLATION

=============================================================================
\* Modification History
\* Last modified Mon Apr 13 20:08:53 CEST 2026 by todormoskov
\* Created Mon Apr 13 20:07:44 CEST 2026 by todormoskov
