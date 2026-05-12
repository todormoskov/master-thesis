----------------------- MODULE ConsensusWithDiamondS -----------------------
EXTENDS Integers, Sequences, Bags

CONSTANT input
N == Len(input)

Cdr(r) == ((r - 1) % N) + 1

SecondProjection(S) == { p[2] : p \in S }

SetMax(S) ==  CHOOSE i \in S : \A j \in S : i >= j

Best(props) == LET domain == DOMAIN props
                   maxStamp == SetMax(SecondProjection(domain))
                   pairWithMaxStamp == CHOOSE bst \in domain : bst[2] = maxStamp
               IN  pairWithMaxStamp[1]

CeilDiv(a, b) == a \div b + IF a % b = 0 THEN 0 ELSE 1

Majority == CeilDiv(N + 1, 2)

PositiveMajority(acks) == acks[TRUE] >= Majority

MaxFailures == N - Majority

FailOK(failed, failCandidates) == Cardinality(failed \union failCandidates) <= MaxFailures

(* --EPC (FailStop, EventualWeakAccuracy, Terminating, Termination, NextWF)
         ConsensusWithDiamondS
chan prop, learn, ack, decide
suspchan learn

(l \in 1..N) [[
    [v |-> input[self],
     d |-> FALSE,
     r |-> 0,
     s |-> 0,
     output |-> Initialized,
     p |-> Initialized,
     a |-> Initialized,
     Props |-> EmptyBag,
     Acks |-> EmptyBag]
     <|
       (t1 = 1) (  cp_init: Coll_p() )
     | (t2 = 2) (  ca_init: Coll_a() )
     | (t3 = 3) (  p1_init: P1()     )
     | (t4 = 4) ( dec_init: Dec()    )
]]

def Coll_p(p, Props) =
   propRcv: (k \in 1..N) prop@k?(p);
  upd_prop: << Props := SetToBag({p}) (+) Props >>;
   cp_loop: Coll_p()

def Coll_a(a, Acks) =
   ackRcv: (k \in 1..N) ack@k?(a);
 upd_acks: << Acks := SetToBag({a}) (+) Acks >>;
  ca_loop: Coll_a()

def P1(d, r, v) =
       go: if ~d
             then inc: << r := r + 1 >>;
              propSnd: {{ prop @ Cdr(r) << <<v, s>> >> }};
                 p2_1: P2()
           fi

def P2(r, Props) =
     crd2: if self = Cdr(r)
             then propsRcv: if BagCardinality(Props) < Majority
                              then    p2_2: P2()
                              else bestSnd: {{ (k \in 1..N) learn @ k << Best(Props) >> }};
                                      p3_c: P3()
                            fi
             else     p3_p: P3() 
           fi

def P3(r, v, s) =
 learnRcv: learn@Cdr(r)?(v);
  trusted: if D[learn][Cdr(r)]
             then  ackSnd: {{ ack @ Cdr(r) << TRUE  >> }}; 
                    stamp: << s := r >>
             else nackSnd: {{ ack @ Cdr(r) << FALSE >> }}
           fi;
  p4_init: P4()

def P4(r, Acks, output, v) =
     crd4: if self = Cdr(r)
             then acksRcv: if BagCardinality(Acks) < Majority
                             then p4_loop: P4()
                             else  enough: if PositiveMajority(Acks)
                                             then  out: << output := v >>;
                                                spread: {{ (k \in 1..N) decide @ k << v >> }}
                                             else p1_c: P1()
                                           fi
                           fi
               else  p1_p: P1()
           fi

def Dec(output, d) =
  decRcv: (k \in 1..N) decide @ k ? (output);
     dec: << d := TRUE >>;
respread: {{ (k \in 1..N) decide @ k << output >> }}

 *)
\* BEGIN TRANSLATION
CONSTANTS Undefined, Initialized, prop, learn, ack, decide

VARIABLES TI, F, AE, O, D, pc, ra, v, d, r, s, output, p, a, Props, Acks

vars == <<TI, F, AE, O, D, pc, ra, v, d, r, s, output, p, a, Props, Acks>>

ProcSet == 1..N

ThreadSet == [self \in ProcSet |-> CASE self \in 1..N -> {1} \union {2} \union {3} \union {4}]

ChanSet == {prop, learn, ack, decide}

SuspChanSet == {learn}

SuspectOK(ti, f, suspect, suspector) == suspect \notin ti

Init == /\ TI = {}
        /\ F = {}
        /\ AE = [_chan \in ChanSet |-> CASE _chan = prop -> EmptyBag
                                       []   _chan = learn -> EmptyBag
                                       []   _chan = ack -> EmptyBag
                                       []   _chan = decide -> EmptyBag]
        /\ O = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> {}]]
        /\ D = [self \in ProcSet |-> [_suspchan \in SuspChanSet |-> [_p \in ProcSet |-> TRUE]]]
        /\ pc = [self \in ProcSet |-> CASE self \in 1..N -> [thd \in ThreadSet[self] |-> CASE thd = 1 -> "cp_init"
                                                                                         []   thd = 2 -> "ca_init"
                                                                                         []   thd = 3 -> "p1_init"
                                                                                         []   thd = 4 -> "dec_init"]]
        /\ ra = [self \in ProcSet |-> [thd \in ThreadSet[self] |-> << >>]]
        /\ v = [self \in ProcSet |-> CASE self \in 1..N -> input[self]
                                     [] OTHER -> Undefined]
        /\ d = [self \in ProcSet |-> CASE self \in 1..N -> FALSE
                                     [] OTHER -> Undefined]
        /\ r = [self \in ProcSet |-> CASE self \in 1..N -> 0
                                     [] OTHER -> Undefined]
        /\ s = [self \in ProcSet |-> CASE self \in 1..N -> 0
                                     [] OTHER -> Undefined]
        /\ output = [self \in ProcSet |-> CASE self \in 1..N -> Initialized
                                          [] OTHER -> Undefined]
        /\ p = [self \in ProcSet |-> CASE self \in 1..N -> Initialized
                                     [] OTHER -> Undefined]
        /\ a = [self \in ProcSet |-> CASE self \in 1..N -> Initialized
                                     [] OTHER -> Undefined]
        /\ Props = [self \in ProcSet |-> CASE self \in 1..N -> EmptyBag
                                         [] OTHER -> Undefined]
        /\ Acks = [self \in ProcSet |-> CASE self \in 1..N -> EmptyBag
                                        [] OTHER -> Undefined]

\* Location l \in 1..N, thread t1 = 1
cp_init(self, thd) == /\ pc[self][thd] = "cp_init"
                      /\ p[self] # Undefined
                      /\ Props[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "propRcv"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

l_t1(self, thd) == cp_init(self, thd)

\* Location l \in 1..N, thread t2 = 2
ca_init(self, thd) == /\ pc[self][thd] = "ca_init"
                      /\ a[self] # Undefined
                      /\ Acks[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "ackRcv"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

l_t2(self, thd) == ca_init(self, thd)

\* Location l \in 1..N, thread t3 = 3
p1_init(self, thd) == /\ pc[self][thd] = "p1_init"
                      /\ d[self] # Undefined
                      /\ r[self] # Undefined
                      /\ v[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

l_t3(self, thd) == p1_init(self, thd)

\* Location l \in 1..N, thread t4 = 4
dec_init(self, thd) == /\ pc[self][thd] = "dec_init"
                       /\ output[self] # Undefined
                       /\ d[self] # Undefined
                       /\ pc' = [pc EXCEPT ![self][thd] = "decRcv"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

l_t4(self, thd) == dec_init(self, thd)

l == \E self \in 1..N \ F:
       \E thd \in ThreadSet[self]:
         \/ l_t1(self, thd)
         \/ l_t2(self, thd)
         \/ l_t3(self, thd)
         \/ l_t4(self, thd)

\* def Coll_p(p, Props)
propRcvRcv(self, thd) == /\ pc[self][thd] = "propRcv"
                         /\ \E k \in 1..N:
                              /\ prop \in ChanSet \ SuspChanSet
                              /\ k \in ProcSet
                              /\ p[self] # Undefined
                              /\ \E msg \in DOMAIN AE[prop]:
                                   /\ msg[1] = k
                                   /\ msg[2] = self
                                   /\ p' = [p EXCEPT ![self] = msg[3]]
                                   /\ AE' = [AE EXCEPT ![prop] = AE[prop] (-) SetToBag({msg})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "upd_prop"]
                         /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, output, a, Props, Acks>>

propRcvRcvS(self, thd) == /\ pc[self][thd] = "propRcv"
                          /\ \E k \in 1..N:
                               /\ prop \in SuspChanSet
                               /\ k \in ProcSet
                               /\ p[self] # Undefined
                               /\ \E msg1 \in DOMAIN AE[prop]:
                                    /\ msg1[1] = k
                                    /\ msg1[2] = self
                                    /\ D' = [D EXCEPT ![self][prop][k] = TRUE]
                                    /\ p' = [p EXCEPT ![self] = msg1[3]]
                                    /\ AE' = [AE EXCEPT ![prop] = AE[prop] (-) SetToBag({msg1})]
                          /\ pc' = [pc EXCEPT ![self][thd] = "upd_prop"]
                          /\ UNCHANGED <<TI, F, O, ra, v, d, r, s, output, a, Props, Acks>>

propRcvSusp(self, thd) == /\ pc[self][thd] = "propRcv"
                          /\ \E k \in 1..N:
                               /\ prop \in SuspChanSet
                               /\ k \in ProcSet
                               /\ SuspectOK(TI, F, k, self)
                               /\ D' = [D EXCEPT ![self][prop][k] = FALSE]
                          /\ pc' = [pc EXCEPT ![self][thd] = "upd_prop"]
                          /\ UNCHANGED <<TI, F, AE, O, ra, v, d, r, s, output, p, a, Props, Acks>>

upd_prop(self, thd) == /\ pc[self][thd] = "upd_prop"
                       /\ Props[self] # Undefined
                       /\ Props' = [Props EXCEPT ![self] = SetToBag({p[self]}) (+) Props[self]]
                       /\ pc' = [pc EXCEPT ![self][thd] = "cp_loop"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Acks>>

cp_loop(self, thd) == /\ pc[self][thd] = "cp_loop"
                      /\ p[self] # Undefined
                      /\ Props[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "propRcv"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

Coll_p == \E self \in ProcSet \ F:
            \E thd \in ThreadSet[self]:
              \/ propRcvRcv(self, thd)
              \/ propRcvRcvS(self, thd)
              \/ propRcvSusp(self, thd)
              \/ upd_prop(self, thd)
              \/ cp_loop(self, thd)

\* def Coll_a(a, Acks)
ackRcvRcv(self, thd) == /\ pc[self][thd] = "ackRcv"
                        /\ \E k \in 1..N:
                             /\ ack \in ChanSet \ SuspChanSet
                             /\ k \in ProcSet
                             /\ a[self] # Undefined
                             /\ \E msg2 \in DOMAIN AE[ack]:
                                  /\ msg2[1] = k
                                  /\ msg2[2] = self
                                  /\ a' = [a EXCEPT ![self] = msg2[3]]
                                  /\ AE' = [AE EXCEPT ![ack] = AE[ack] (-) SetToBag({msg2})]
                        /\ pc' = [pc EXCEPT ![self][thd] = "upd_acks"]
                        /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, output, p, Props, Acks>>

ackRcvRcvS(self, thd) == /\ pc[self][thd] = "ackRcv"
                         /\ \E k \in 1..N:
                              /\ ack \in SuspChanSet
                              /\ k \in ProcSet
                              /\ a[self] # Undefined
                              /\ \E msg3 \in DOMAIN AE[ack]:
                                   /\ msg3[1] = k
                                   /\ msg3[2] = self
                                   /\ D' = [D EXCEPT ![self][ack][k] = TRUE]
                                   /\ a' = [a EXCEPT ![self] = msg3[3]]
                                   /\ AE' = [AE EXCEPT ![ack] = AE[ack] (-) SetToBag({msg3})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "upd_acks"]
                         /\ UNCHANGED <<TI, F, O, ra, v, d, r, s, output, p, Props, Acks>>

ackRcvSusp(self, thd) == /\ pc[self][thd] = "ackRcv"
                         /\ \E k \in 1..N:
                              /\ ack \in SuspChanSet
                              /\ k \in ProcSet
                              /\ SuspectOK(TI, F, k, self)
                              /\ D' = [D EXCEPT ![self][ack][k] = FALSE]
                         /\ pc' = [pc EXCEPT ![self][thd] = "upd_acks"]
                         /\ UNCHANGED <<TI, F, AE, O, ra, v, d, r, s, output, p, a, Props, Acks>>

upd_acks(self, thd) == /\ pc[self][thd] = "upd_acks"
                       /\ Acks[self] # Undefined
                       /\ Acks' = [Acks EXCEPT ![self] = SetToBag({a[self]}) (+) Acks[self]]
                       /\ pc' = [pc EXCEPT ![self][thd] = "ca_loop"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props>>

ca_loop(self, thd) == /\ pc[self][thd] = "ca_loop"
                      /\ a[self] # Undefined
                      /\ Acks[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "ackRcv"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

Coll_a == \E self \in ProcSet \ F:
            \E thd \in ThreadSet[self]:
              \/ ackRcvRcv(self, thd)
              \/ ackRcvRcvS(self, thd)
              \/ ackRcvSusp(self, thd)
              \/ upd_acks(self, thd)
              \/ ca_loop(self, thd)

\* def P1(d, r, v)
goT(self, thd) == /\ pc[self][thd] = "go"
                  /\ ~d[self]
                  /\ pc' = [pc EXCEPT ![self][thd] = "inc"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

goF(self, thd) == /\ pc[self][thd] = "go"
                  /\ ~(~d[self])
                  /\ pc' = [pc EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Head(ra[self][thd])
                                                       ELSE "Done"]
                  /\ ra' = [ra EXCEPT ![self][thd] = IF ra[self][thd] # << >>
                                                       THEN Tail(ra[self][thd])
                                                       ELSE ra[self][thd]]
                  /\ UNCHANGED <<TI, F, AE, O, D, v, d, r, s, output, p, a, Props, Acks>>

inc(self, thd) == /\ pc[self][thd] = "inc"
                  /\ r[self] # Undefined
                  /\ r' = [r EXCEPT ![self] = r[self] + 1]
                  /\ pc' = [pc EXCEPT ![self][thd] = "propSnd"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, s, output, p, a, Props, Acks>>

propSnd(self, thd) == /\ pc[self][thd] = "propSnd"
                      /\ prop \in ChanSet
                      /\ Cdr(r[self]) \in ProcSet
                      /\ AE' = [AE EXCEPT ![prop] = AE[prop] (+) SetToBag({<<self, Cdr(r[self]), <<v[self], s[self]>>>>})]
                      /\ pc' = [pc EXCEPT ![self][thd] = "p2_1"]
                      /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p2_1(self, thd) == /\ pc[self][thd] = "p2_1"
                   /\ r[self] # Undefined
                   /\ Props[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "crd2"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

P1 == \E self \in ProcSet \ F:
        \E thd \in ThreadSet[self]:
          \/ goT(self, thd)
          \/ goF(self, thd)
          \/ inc(self, thd)
          \/ propSnd(self, thd)
          \/ p2_1(self, thd)

\* def P2(r, Props)
crd2T(self, thd) == /\ pc[self][thd] = "crd2"
                    /\ self = Cdr(r[self])
                    /\ pc' = [pc EXCEPT ![self][thd] = "propsRcv"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

crd2F(self, thd) == /\ pc[self][thd] = "crd2"
                    /\ ~(self = Cdr(r[self]))
                    /\ pc' = [pc EXCEPT ![self][thd] = "p3_p"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

propsRcvT(self, thd) == /\ pc[self][thd] = "propsRcv"
                        /\ BagCardinality(Props[self]) < Majority
                        /\ pc' = [pc EXCEPT ![self][thd] = "p2_2"]
                        /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

propsRcvF(self, thd) == /\ pc[self][thd] = "propsRcv"
                        /\ ~(BagCardinality(Props[self]) < Majority)
                        /\ pc' = [pc EXCEPT ![self][thd] = "bestSnd"]
                        /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p2_2(self, thd) == /\ pc[self][thd] = "p2_2"
                   /\ r[self] # Undefined
                   /\ Props[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "crd2"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

bestSnd(self, thd) == /\ pc[self][thd] = "bestSnd"
                      /\ \E k \in 1..N \ O[self][thd]:
                           /\ learn \in ChanSet
                           /\ k \in ProcSet
                           /\ AE' = [AE EXCEPT ![learn] = AE[learn] (+) SetToBag({<<self, k, Best(Props[self])>>})]
                           /\ pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                                THEN pc[self][thd]
                                                                ELSE "p3_c"]
                           /\ O' = [O EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                              THEN O[self][thd] \cup {k}
                                                              ELSE {}]
                      /\ UNCHANGED <<TI, F, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p3_c(self, thd) == /\ pc[self][thd] = "p3_c"
                   /\ r[self] # Undefined
                   /\ v[self] # Undefined
                   /\ s[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "learnRcv"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p3_p(self, thd) == /\ pc[self][thd] = "p3_p"
                   /\ r[self] # Undefined
                   /\ v[self] # Undefined
                   /\ s[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "learnRcv"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

P2 == \E self \in ProcSet \ F:
        \E thd \in ThreadSet[self]:
          \/ crd2T(self, thd)
          \/ crd2F(self, thd)
          \/ propsRcvT(self, thd)
          \/ propsRcvF(self, thd)
          \/ p2_2(self, thd)
          \/ bestSnd(self, thd)
          \/ p3_c(self, thd)
          \/ p3_p(self, thd)

\* def P3(r, v, s)
learnRcvRcv(self, thd) == /\ pc[self][thd] = "learnRcv"
                          /\ learn \in ChanSet \ SuspChanSet
                          /\ Cdr(r[self]) \in ProcSet
                          /\ v[self] # Undefined
                          /\ \E msg4 \in DOMAIN AE[learn]:
                               /\ msg4[1] = Cdr(r[self])
                               /\ msg4[2] = self
                               /\ v' = [v EXCEPT ![self] = msg4[3]]
                               /\ AE' = [AE EXCEPT ![learn] = AE[learn] (-) SetToBag({msg4})]
                          /\ pc' = [pc EXCEPT ![self][thd] = "trusted"]
                          /\ UNCHANGED <<TI, F, O, D, ra, d, r, s, output, p, a, Props, Acks>>

learnRcvRcvS(self, thd) == /\ pc[self][thd] = "learnRcv"
                           /\ learn \in SuspChanSet
                           /\ Cdr(r[self]) \in ProcSet
                           /\ v[self] # Undefined
                           /\ \E msg5 \in DOMAIN AE[learn]:
                                /\ msg5[1] = Cdr(r[self])
                                /\ msg5[2] = self
                                /\ D' = [D EXCEPT ![self][learn][Cdr(r[self])] = TRUE]
                                /\ v' = [v EXCEPT ![self] = msg5[3]]
                                /\ AE' = [AE EXCEPT ![learn] = AE[learn] (-) SetToBag({msg5})]
                           /\ pc' = [pc EXCEPT ![self][thd] = "trusted"]
                           /\ UNCHANGED <<TI, F, O, ra, d, r, s, output, p, a, Props, Acks>>

learnRcvSusp(self, thd) == /\ pc[self][thd] = "learnRcv"
                           /\ learn \in SuspChanSet
                           /\ Cdr(r[self]) \in ProcSet
                           /\ SuspectOK(TI, F, Cdr(r[self]), self)
                           /\ D' = [D EXCEPT ![self][learn][Cdr(r[self])] = FALSE]
                           /\ pc' = [pc EXCEPT ![self][thd] = "trusted"]
                           /\ UNCHANGED <<TI, F, AE, O, ra, v, d, r, s, output, p, a, Props, Acks>>

trustedT(self, thd) == /\ pc[self][thd] = "trusted"
                       /\ D[self][learn][Cdr(r[self])]
                       /\ pc' = [pc EXCEPT ![self][thd] = "ackSnd"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

trustedF(self, thd) == /\ pc[self][thd] = "trusted"
                       /\ ~(D[self][learn][Cdr(r[self])])
                       /\ pc' = [pc EXCEPT ![self][thd] = "nackSnd"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

ackSnd(self, thd) == /\ pc[self][thd] = "ackSnd"
                     /\ ack \in ChanSet
                     /\ Cdr(r[self]) \in ProcSet
                     /\ AE' = [AE EXCEPT ![ack] = AE[ack] (+) SetToBag({<<self, Cdr(r[self]), TRUE>>})]
                     /\ pc' = [pc EXCEPT ![self][thd] = "stamp"]
                     /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

stamp(self, thd) == /\ pc[self][thd] = "stamp"
                    /\ s[self] # Undefined
                    /\ s' = [s EXCEPT ![self] = r[self]]
                    /\ pc' = [pc EXCEPT ![self][thd] = "p4_init"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, output, p, a, Props, Acks>>

nackSnd(self, thd) == /\ pc[self][thd] = "nackSnd"
                      /\ ack \in ChanSet
                      /\ Cdr(r[self]) \in ProcSet
                      /\ AE' = [AE EXCEPT ![ack] = AE[ack] (+) SetToBag({<<self, Cdr(r[self]), FALSE>>})]
                      /\ pc' = [pc EXCEPT ![self][thd] = "p4_init"]
                      /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p4_init(self, thd) == /\ pc[self][thd] = "p4_init"
                      /\ r[self] # Undefined
                      /\ Acks[self] # Undefined
                      /\ output[self] # Undefined
                      /\ v[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "crd4"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

P3 == \E self \in ProcSet \ F:
        \E thd \in ThreadSet[self]:
          \/ learnRcvRcv(self, thd)
          \/ learnRcvRcvS(self, thd)
          \/ learnRcvSusp(self, thd)
          \/ trustedT(self, thd)
          \/ trustedF(self, thd)
          \/ ackSnd(self, thd)
          \/ stamp(self, thd)
          \/ nackSnd(self, thd)
          \/ p4_init(self, thd)

\* def P4(r, Acks, output, v)
crd4T(self, thd) == /\ pc[self][thd] = "crd4"
                    /\ self = Cdr(r[self])
                    /\ pc' = [pc EXCEPT ![self][thd] = "acksRcv"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

crd4F(self, thd) == /\ pc[self][thd] = "crd4"
                    /\ ~(self = Cdr(r[self]))
                    /\ pc' = [pc EXCEPT ![self][thd] = "p1_p"]
                    /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

acksRcvT(self, thd) == /\ pc[self][thd] = "acksRcv"
                       /\ BagCardinality(Acks[self]) < Majority
                       /\ pc' = [pc EXCEPT ![self][thd] = "p4_loop"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

acksRcvF(self, thd) == /\ pc[self][thd] = "acksRcv"
                       /\ ~(BagCardinality(Acks[self]) < Majority)
                       /\ pc' = [pc EXCEPT ![self][thd] = "enough"]
                       /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p4_loop(self, thd) == /\ pc[self][thd] = "p4_loop"
                      /\ r[self] # Undefined
                      /\ Acks[self] # Undefined
                      /\ output[self] # Undefined
                      /\ v[self] # Undefined
                      /\ pc' = [pc EXCEPT ![self][thd] = "crd4"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

enoughT(self, thd) == /\ pc[self][thd] = "enough"
                      /\ PositiveMajority(Acks[self])
                      /\ pc' = [pc EXCEPT ![self][thd] = "out"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

enoughF(self, thd) == /\ pc[self][thd] = "enough"
                      /\ ~(PositiveMajority(Acks[self]))
                      /\ pc' = [pc EXCEPT ![self][thd] = "p1_c"]
                      /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

out(self, thd) == /\ pc[self][thd] = "out"
                  /\ output[self] # Undefined
                  /\ output' = [output EXCEPT ![self] = v[self]]
                  /\ pc' = [pc EXCEPT ![self][thd] = "spread"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, p, a, Props, Acks>>

spread(self, thd) == /\ pc[self][thd] = "spread"
                     /\ \E k \in 1..N \ O[self][thd]:
                          /\ decide \in ChanSet
                          /\ k \in ProcSet
                          /\ AE' = [AE EXCEPT ![decide] = AE[decide] (+) SetToBag({<<self, k, v[self]>>})]
                          /\ pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                               THEN pc[self][thd]
                                                               ELSE IF ra[self][thd] # << >>
                                                                      THEN Head(ra[self][thd])
                                                                      ELSE "Done"]
                          /\ ra' = [ra EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                               THEN ra[self][thd]
                                                               ELSE IF ra[self][thd] # << >>
                                                                      THEN Tail(ra[self][thd])
                                                                      ELSE ra[self][thd]]
                          /\ O' = [O EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                             THEN O[self][thd] \cup {k}
                                                             ELSE {}]
                     /\ UNCHANGED <<TI, F, D, v, d, r, s, output, p, a, Props, Acks>>

p1_c(self, thd) == /\ pc[self][thd] = "p1_c"
                   /\ d[self] # Undefined
                   /\ r[self] # Undefined
                   /\ v[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

p1_p(self, thd) == /\ pc[self][thd] = "p1_p"
                   /\ d[self] # Undefined
                   /\ r[self] # Undefined
                   /\ v[self] # Undefined
                   /\ pc' = [pc EXCEPT ![self][thd] = "go"]
                   /\ UNCHANGED <<TI, F, AE, O, D, ra, v, d, r, s, output, p, a, Props, Acks>>

P4 == \E self \in ProcSet \ F:
        \E thd \in ThreadSet[self]:
          \/ crd4T(self, thd)
          \/ crd4F(self, thd)
          \/ acksRcvT(self, thd)
          \/ acksRcvF(self, thd)
          \/ p4_loop(self, thd)
          \/ enoughT(self, thd)
          \/ enoughF(self, thd)
          \/ out(self, thd)
          \/ spread(self, thd)
          \/ p1_c(self, thd)
          \/ p1_p(self, thd)

\* def Dec(output, d)
decRcvRcv(self, thd) == /\ pc[self][thd] = "decRcv"
                        /\ \E k \in 1..N:
                             /\ decide \in ChanSet \ SuspChanSet
                             /\ k \in ProcSet
                             /\ output[self] # Undefined
                             /\ \E msg6 \in DOMAIN AE[decide]:
                                  /\ msg6[1] = k
                                  /\ msg6[2] = self
                                  /\ output' = [output EXCEPT ![self] = msg6[3]]
                                  /\ AE' = [AE EXCEPT ![decide] = AE[decide] (-) SetToBag({msg6})]
                        /\ pc' = [pc EXCEPT ![self][thd] = "dec"]
                        /\ UNCHANGED <<TI, F, O, D, ra, v, d, r, s, p, a, Props, Acks>>

decRcvRcvS(self, thd) == /\ pc[self][thd] = "decRcv"
                         /\ \E k \in 1..N:
                              /\ decide \in SuspChanSet
                              /\ k \in ProcSet
                              /\ output[self] # Undefined
                              /\ \E msg7 \in DOMAIN AE[decide]:
                                   /\ msg7[1] = k
                                   /\ msg7[2] = self
                                   /\ D' = [D EXCEPT ![self][decide][k] = TRUE]
                                   /\ output' = [output EXCEPT ![self] = msg7[3]]
                                   /\ AE' = [AE EXCEPT ![decide] = AE[decide] (-) SetToBag({msg7})]
                         /\ pc' = [pc EXCEPT ![self][thd] = "dec"]
                         /\ UNCHANGED <<TI, F, O, ra, v, d, r, s, p, a, Props, Acks>>

decRcvSusp(self, thd) == /\ pc[self][thd] = "decRcv"
                         /\ \E k \in 1..N:
                              /\ decide \in SuspChanSet
                              /\ k \in ProcSet
                              /\ SuspectOK(TI, F, k, self)
                              /\ D' = [D EXCEPT ![self][decide][k] = FALSE]
                         /\ pc' = [pc EXCEPT ![self][thd] = "dec"]
                         /\ UNCHANGED <<TI, F, AE, O, ra, v, d, r, s, output, p, a, Props, Acks>>

dec(self, thd) == /\ pc[self][thd] = "dec"
                  /\ d[self] # Undefined
                  /\ d' = [d EXCEPT ![self] = TRUE]
                  /\ pc' = [pc EXCEPT ![self][thd] = "respread"]
                  /\ UNCHANGED <<TI, F, AE, O, D, ra, v, r, s, output, p, a, Props, Acks>>

respread(self, thd) == /\ pc[self][thd] = "respread"
                       /\ \E k \in 1..N \ O[self][thd]:
                            /\ decide \in ChanSet
                            /\ k \in ProcSet
                            /\ AE' = [AE EXCEPT ![decide] = AE[decide] (+) SetToBag({<<self, k, output[self]>>})]
                            /\ pc' = [pc EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                                 THEN pc[self][thd]
                                                                 ELSE IF ra[self][thd] # << >>
                                                                        THEN Head(ra[self][thd])
                                                                        ELSE "Done"]
                            /\ ra' = [ra EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                                 THEN ra[self][thd]
                                                                 ELSE IF ra[self][thd] # << >>
                                                                        THEN Tail(ra[self][thd])
                                                                        ELSE ra[self][thd]]
                            /\ O' = [O EXCEPT ![self][thd] = IF O[self][thd] \cup {k} # 1..N
                                                               THEN O[self][thd] \cup {k}
                                                               ELSE {}]
                       /\ UNCHANGED <<TI, F, D, v, d, r, s, output, p, a, Props, Acks>>

Dec == \E self \in ProcSet \ F:
         \E thd \in ThreadSet[self]:
           \/ decRcvRcv(self, thd)
           \/ decRcvRcvS(self, thd)
           \/ decRcvSusp(self, thd)
           \/ dec(self, thd)
           \/ respread(self, thd)

Env == \E ti, f \in SUBSET (ProcSet \ (TI \cup F)):
         /\ ti # {}
         /\ f # {}
         /\ ti \cap f = {}
         /\ FailOK(F, f)
         /\ TI' = TI \cup ti
         /\ F' = F \cup f
         /\ UNCHANGED <<AE, O, D, pc, ra, v, d, r, s, output, p, a, Props, Acks>>

Terminating == /\ \A self \in ProcSet \ F:
                    \A thd \in ThreadSet[self]:
                      pc[self][thd] = "Done"
               /\ UNCHANGED vars

Next == \/ l
        \/ Coll_p
        \/ Coll_a
        \/ P1
        \/ P2
        \/ P3
        \/ P4
        \/ Dec
        \/ Env
        \/ Terminating

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

Termination == <>(\A self \in ProcSet \ F:
                    \A thd \in ThreadSet[self]:
                      pc[self][thd] = "Done")


\* END TRANSLATION

=============================================================================
\* Modification History
\* Last modified Tue May 12 15:43:05 CEST 2026 by todormoskov
\* Created Tue Feb 03 17:23:17 CET 2026 by todormoskov
