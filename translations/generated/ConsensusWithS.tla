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

VARIABLES TI, trim, F, AE, O, D, pc, ra, channel, x, r, output

vars == <<TI, trim, F, AE, O, D, pc, ra, channel, x, r, output>>

ProcSet == 1..N

ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> {1}]

ChanSet == {c}

SuspChanSet == {c}

SuspectOK(ti, f, suspect, suspector) == ti # {} /\ suspect \notin ti

Init == /\ TI = {}
        /\ trim = Undefined
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
                  /\ UNCHANGED <<TI, trim, F, AE, O, D, channel, x, r, output>>

out(self, thd) == /\ pc[self][thd] = "out"
                  /\ output[self] # Undefined
                  /\ output' = [output EXCEPT ![self] = x[self]]
                  /\ pc' = [pc EXCEPT ![self][thd] = "Done"]
                  /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, r>>

p_j(self, thd) == \/ run(self, thd)
                  \/ out(self, thd)

p == \E self \in 1..N \ F:
       \E thd \in ThreadSet[self]:
         p_j(self, thd)

\* def L(channel, x, r, output)
goT(self, thd) == /\ pc[self][thd] = "go"
                  /\ r[self] <= N
                  /\ pc' = [pc EXCEPT ![self][thd] = "role"]
                  /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, r, output>>

goF(self, thd) == /\ pc[self][thd] = "go"
                  /\ ~(r[self] <= N)
                  /\ pc' = [pc EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Head(ra[self][thd])
                                                       ELSE "Done"]
                  /\ ra' = [ra EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Tail(ra[self][thd])
                                                       ELSE ra[self][thd]]
                  /\ UNCHANGED <<TI, trim, F, AE, O, D, channel, x, r, output>>

roleT(self, thd) == /\ pc[self][thd] = "role"
                    /\ self = r[self]
                    /\ pc' = [pc EXCEPT ![self][thd] = "mcast"]
                    /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, r, output>>

roleF(self, thd) == /\ pc[self][thd] = "role"
                    /\ ~(self = r[self])
                    /\ pc' = [pc EXCEPT ![self][thd] = "wait"]
                    /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, r, output>>

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
                    /\ UNCHANGED <<TI, trim, F, D, ra, channel, x, r, output>>

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
                      /\ UNCHANGED <<TI, trim, F, O, D, ra, channel, r, output>>

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
                       /\ UNCHANGED <<TI, trim, F, O, ra, channel, r, output>>

waitSusp(self, thd) == /\ pc[self][thd] = "wait"
                       /\ channel[self] \in SuspChanSet
                       /\ r[self] \in ProcSet
                       /\ SuspectOK(TI, F, r[self], self)
                       /\ D' = [D EXCEPT ![self][channel[self]][r[self]] = FALSE]
                       /\ pc' = [pc EXCEPT ![self][thd] = "inc"]
                       /\ UNCHANGED <<TI, trim, F, AE, O, ra, channel, x, r, output>>

inc(self, thd) == /\ pc[self][thd] = "inc"
                  /\ r[self] # Undefined
                  /\ r' = [r EXCEPT ![self] = r[self] + 1]
                  /\ pc' = [pc EXCEPT ![self][thd] = "loop"]
                  /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, output>>

loop(self, thd) == /\ pc[self][thd] = "loop"
                   /\ channel[self] # Undefined
                   /\ x[self] # Undefined
                   /\ r[self] # Undefined
                   /\ output[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                   /\ UNCHANGED <<TI, trim, F, AE, O, D, ra, channel, x, r, output>>

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

SetMin(S) == CHOOSE i \in S: \A j \in S: i <= j

Env == \E ti, f \in SUBSET (ProcSet \ (TI \cup F)):
         /\ ti # {} \/ f # {}
         /\ ti \cap f = {}
         /\ TI' = TI \cup ti
         /\ trim' = IF TI = {} /\ ti # {} THEN SetMin(ti) ELSE trim
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

Undecided == { input[i] : i \in 1..N }

TypeOK == /\ TI \subseteq ProcSet
          /\ trim \in { Undefined } \union ProcSet
          /\ F \in SUBSET (ProcSet \ TI)
          /\ \A chan \in ChanSet:
                \A msg \in DOMAIN AE[chan]:
                    msg \in (ProcSet \X ProcSet \X Undecided)
          /\ D \in [ProcSet -> [ChanSet -> [ProcSet -> BOOLEAN]]]
          /\ \A self \in ProcSet:
                /\ pc[self] \in [ThreadSet[self] -> {"run", "out", "go", "role", "mcast", "wait", "inc", "loop", "Done"}]
                /\ ra[self] \in [ThreadSet[self] -> {<< >>, << "out" >>}]
                /\ O[self] \in [ThreadSet[self] -> SUBSET {j \in 1..N: j # self}]
          /\ channel \in [ProcSet -> {c}]
          /\ x \in [ProcSet -> Undecided]
          /\ r \in [ProcSet -> 1..(N+1)]
          /\ output \in [ProcSet -> {Initialized} \union Undecided]

Inv == /\ TypeOK 
       /\ \A self \in 1..N:
            (***************************************************************)
            (* Until trim is chosen, we know that suspicions are not       *)
            (* allowed.  This means that no process can suspect any other  *)
            (* process and with this that process 1 will have its x value  *)
            (* received by everybody in the first round (possibly).  If    *)
            (* trim is chosen in the first round (while some processes     *)
            (* have received the input of process 1 and others have not),  *)
            (* we simply start claiming the following properties about the *)
            (* trim process.  This is also the case if trim is chosen in a *)
            (* later round.                                                *)
            (***************************************************************)
            LET trimOrFirst == IF trim # Undefined THEN trim ELSE 1
            IN  /\ r[self] < trimOrFirst => x[self] \in Undecided
                /\ \A msg \in DOMAIN AE[c]: msg[1] = trimOrFirst /\ msg[2] = self => msg[3] = x[trimOrFirst]
                /\ self # trimOrFirst /\ r[self] = trimOrFirst 
                     => /\ pc[self][1] \in {"run", "go", "role", "wait"} => x[self] \in Undecided
                        /\ pc[self][1] = "inc" => x[self] = x[trimOrFirst]
                /\ r[self] > trimOrFirst => x[self] = x[trimOrFirst]
                /\ self > trimOrFirst => \A msg \in DOMAIN AE[c]: msg[1] = self => msg[3] = x[trimOrFirst]
                /\ pc[self][1] = "out" => x[self] = x[trimOrFirst]
                /\ pc[self][1] = "Done" => output[self] = x[trimOrFirst]


=============================================================================
\* Modification History
\* Last modified Mon May 11 15:46:47 CEST 2026 by todormoskov
\* Created Mon Apr 13 20:07:44 CEST 2026 by todormoskov
