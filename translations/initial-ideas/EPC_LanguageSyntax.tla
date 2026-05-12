------------------------- MODULE EPC_LanguageSyntax -------------------------
EXTENDS Integers, Sequences, TLC, TLAPS, Bags

(***************************************************************************
Variable declarations and assignments:

--EPC Consensus

NEW c

(A = 1) [[ [x |-> Undefined] <| var x . << x := 10 >> . 0 ]]


EPC:
[[ [x |-> Undefined] <| var x . << x := 1 >> . 0 ]]
-->(DECL)
[[ [x |-> Initialized] <| << x := 1 >> . 0 ]]
-->(ASSIGN)
[[ [x |-> 1] <| 0 ]]
-/->


NEW c

(1) [[ [] |>  c@2<< "Hello" >>. 0 ]] ||  (2) [[ [] |> var x. c@1(x). 0]]

-->(SEND)

(1) [[ [] |> 0 ]] || {{ c_{1 -> 2}<<"Hello">> }} || (2) [[ [x |-> Initilized] |> c@1(x). 0]]

--> (RECEIVE)

(1) [[ [] |> 0 ]] || {{ }} || (2) [[ [x |-> "Hello"] |> 0]]


TLA+:
CONSTANTS Undefined, Initialized
VARIABLES pc, x

Init == /\ pc = "1"
        /\ x  = Undefined

Next == /\ x = {1,2,3}
        /\ x' = x \union {4}


var x. << x := 10 >> . << x := x + 1 >> . 0
----------------------------------------------------------------------------
Locations:

Defining a single location (using only the location id):

1 [[ [x |-> Undefined] |> var x.<x := 10>. 0 ]]

"a" [[ [x |-> Undefined] |> var x.<x := 10>. 0 ]]
----------------------------------------------------------------------------
or similar to PlusCal (using an operator name and a location id;
Names and locations have to be diffenent):

(p = 1) [[ [x |-> Undefined] |> a: var x. <x := 10>. Done: 0 ]]

(q = "a") [[ [x |-> Undefined] |> var x.<x := 10>. 0 ]]

----------------------------------------------------------------------------
Defining multiple locations (example as in PlusCal):

|| (p \in 1..10) [[ [x |-> Undefined] |> var x.<x := 10>. 0 ]]

|| (q \in {"a", "'b"}) [[ [x |-> Undefined] |> var x.<x := 10>. 0 ]]

All location names have to be of the same type: Either integers or strings.
----------------------------------------------------------------------------
Thread compositions:

var a. ( var x. <x := 10>. 0
       | var y. <y := 100>. 0
       | var z. <z := 1000>. 0)

maybe using a syntax with similar to TLA+ list conjuctions?

var a .| var x. <x := 10>. 0
       | var y. <y := 100>. 0
       | var z. <z := 1000>. 0

This will look like this within a location:
(p = 1) [[ 
    [] |> 
    var a. ( var x. <x := 10>. 0
           | var y. <y := 100>. 0
           | var z. <z := 1000>. 0)
]]

or like this: 

|| (p \in 1..10) [[ 
    [] |>
    var a. ( var x. <x := 10>. 0
           | var y. <y := 100>. 0
           | var z. <z := 1000>. 0)
]]
----------------------------------------------------------------------------
Receving a message on a channel c:
(The channel needs to be globally declared, in order to be able to receive on it)
(To do this, we can use NEW as in Pict, or channel as in DistPlusCal)

NEW c

or alternatively:

channel c;

What syntax do we want to indicate that we want to receive?

CCS-like: c@2(x)

Pict-like: c@2?(x)

Go-like: x := <-c@2

What syntax do we want to use for travelling messages?

c{1 -> 2}<1>

c_{1 -> 2}<"Hello">

c[1 -> 2]<1>

c_[1 -> 2]<"Hello">

What syntax do we want to use for multisets?

{{ ... }}

{| ... |}

How it might look:

(p = 1) [[ [] |> var x. c@2(x). 0] ]] || {{ c_{2 -> 1}<"Hello"> }}

(p = 1) [[ [] |> var x. c@2(x). 0] ]] || {| c_{2 -> 1}<"Hello"> |}
----------------------------------------------------------------------------
Sending a message on a channel c:

CCS-like: c@2<1>

Pict/Akka-like: c@2!<1>

Go-like: c@2 <- 1

How it might look:

(p = 1) [[ [] |> c@2<x>. 0] ]] || (q = 2) [[ ... ]]

(p = 1) [[ [] |> c@2!<x>. 0] ]] || (q = 2) [[ ... ]]
----------------------------------------------------------------------------
Sending multiple messages:

As location defintions?

(j \in 1..10) c@j<x>

((j \in 1..10) c@j<x>)

How it might look:


(p = 1) [[ [] |> (j \in 1..10) c@j<x>. 0] ]] || (q = 2) [[ ... ]]

(p = 1) [[ [] |> ((j \in 1..10) c@j<x>). 0] ]] || (q = 2) [[ ... ]]


Maybe with additional constraints?

(p = 1) [[ [] |> (j \in 1..10 : j # 1) c@j<x>. 0] ]] || (q = 2) [[ ... ]]

or as in PlusCal, where self refers to the locations id:

(p = 1) [[ [] |> (j \in 1..10 : j # self) c@j<x>. 0] ]] || (q = 2) [[ ... ]]

Here I am using the expression language of TLA+

Problem: How do I send the same message twice?
Is it necessary? Theoretically, it is a different step anyway,
so I can just send it again as the next action:

(p = 1) [[ [] |> c@2<x>. c@2<x>. 0] ]] || (q = 2) [[ ... ]]

or should I have a syntax like:

(p = 1) [[ [] |> {{ c@2<x>, c@2<x> }}. 0] ]] || (q = 2) [[ ... ]]

Then what about sending multiple messages:

(p = 1) [[ [] |> {{ (j \in 1..10 : j # self) c@j<x> }}. 0] ]] || (q = 2) [[ ... ]]

What should the exact semantics of {{ ... }} be? My initial idea was to translate it to:
SetToBag({ ... })

But this will not work if I have the same element twice, I will have to translate it to:
SetToBag({ c@2<x> }) (+) SetToBag({ c@2<x> })

----------------------------------------------------------------------------
Simple conditional threads:

(p = 1) [[ [] |> if 1 = 1 then var x. 0 else var y. 0 ]]

(p = 1) [[ [] |> 
    if 1 = 1 
        then var x. 0 
        else var y. 0 
]]

or:

(p = 1) [[ [] |> if (1 = 1) then var x. 0 else var y. 0 ]]

(p = 1) [[ [] |> 
    if (1 = 1) 
        then var x. 0 
        else var y. 0 
]]
----------------------------------------------------------------------------
Thread definitions:

def declareXAndStop() = var x. 0

Call a thread :

(p = 1) [[ [] |> declareXAndStop() ]]

(p = 1) [[ [] |> run declareXAndStop ]]

(p = 1) [[ [] |> spawn declareXAndStop ]]

What about the parameters?
These are the free variables in the terms.
In the calculus, we don't pass references to those variables,
but make sure that there are no variables, which will remain free
after we call the thread:

def assign10toX(x) = <x := 10>. 0 

(p = 1) [[ [x |-> Initialized] |> declareXAndStop(x) ]] \* here we don't need to do that, right?


Not necessary:

(p = 1) [[ [x |-> Initialized] |> run declareXAndStop ]]

(p = 1) [[ [x |-> Initialized] |> spawn declareXAndStop ]]

----------------------------------------------------------------------------
What about defining networks and starting/spawning them in a separate step?

Not necessary!!!

def someLocation = (p = 1) [[ [] |> ... ]]

And then in a second step something like this:

run someLocation || ...

spawn someLocation

start someLocation


 ***************************************************************************)

(***************************************************************************
def anyThread(c, l, a) =
    c'@l(a).(var x.<x := x + 1>.0 | var y.<y := y + 1>.0 | var z.<z := z + 1>.0)

def anyThread(c, l, a) =
    c'@l(a);
        | var x.<x := x + 1>.0
        | var y.<y := y + 1>.0
        | var z.<z := z + 1>.0

def anyThread(c, l, a) {
    c'@l(a);
    (var x; <x := x + 1>; | var y; <y := y + 1>; | var z; <z := z + 1>;)
}
 ***************************************************************************)

(***************************************************************************
Comparison to PlusCal:

--algorithm Test {
    define {
        Literals(S) == [type : {"literal"}, val : S]
        
        CONSTANT Undefined
        ASSUME Undefined \notin Literals(Int)
        ASSUME Undefined \notin Int
        
        \* Undefined == CHOOSE Undefined : Undefined \notin Literals(Int)
        
        Literal(v) == [type |-> "literal", val |-> v]
    }
    process(p = 1)
        variables y = Undefined;
    {
        first: y := 10;
    }
    
    process(q \in 2..3)
        variables y = Undefined;
    {
        second: y := 10
    }
        
}
 ***************************************************************************)
\* BEGIN TRANSLATION (chksum(pcal) = "8a658289" /\ chksum(tla) = "49fda7d5")
\* Process variable y of process p at line 302 col 19 changed to y_

VARIABLE pc

(* define statement *)
Literals(S) == [type : {"literal"}, val : S]

CONSTANT Undefined
ASSUME Undefined \notin Literals(Int)
ASSUME Undefined \notin Int

Literal(v) == [type |-> "literal", val |-> v]

VARIABLES y_, y

vars == << pc, y_, y >>

ProcSet == {1} \cup (2..3)

Init == (* Process p *)
        /\ y_ = Undefined
        (* Process q *)
        /\ y = [self \in 2..3 |-> Undefined]
        /\ pc = [self \in ProcSet |-> CASE self = 1 -> "first"
                                        [] self \in 2..3 -> "second"]

first == /\ pc[1] = "first"
         /\ y_' = 10
         /\ pc' = [pc EXCEPT ![1] = "Done"]
         /\ y' = y

p == first

second(self) == /\ pc[self] = "second"
                /\ y' = [y EXCEPT ![self] = 10]
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ y_' = y_

q(self) == second(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == p
           \/ (\E self \in 2..3: q(self))
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 

THEOREM Literal(1) # Undefined
BY Undefined \notin Literals(Int) DEF Literals, Literal

THEOREM 1 # Undefined
BY Undefined \notin Int

test == 1

(*
\* c[n \in {1,2}, m \in {3,4}] == EmptyBag
VARIABLE c, pc

InitC == /\ c = [n, m \in {1,2} |-> EmptyBag]
         /\ pc = "1"

NextC == /\ pc = "1"
         /\ c' = [c EXCEPT ![1,1] = @ (+) SetToBag({"Hello"})]
         /\ pc' = "2"
*)


=============================================================================
\* Modification History
\* Last modified Mon Dec 15 10:32:06 CET 2025 by todormoskov
\* Created Fri Oct 17 12:03:56 CEST 2025 by todormoskov
