---
title: The Iterated Prisoner's Dilemma with Mind-Reading
published: February 25, 2017
tags: fun, haskell
---

Just for fun, I wrote a little iterated prisoner's dilemma simulator
in Haskell, with the following twist. As well as examining the
immediate history of decisions made by itself and its opponent, an
agent may also simulate how its opponent will behave in different
situations. An agent may ask "What would my opponent do right now
against agent A?", and also "What is my opponent going to do against
me right now?".

Of course, the latter question could lead to an infinite regress. When
asking this question, an agent must supply a default decision to be
used in the case we have hit our maximum recursive depth. All up, our
agents are given by an term of:

``` haskell
data Action = Cooperate | Defect
data Agent = Const Action
           | Not Agent
           | And Agent Agent
           | SelfHistory Int Action
           | OpHistory   Int Action
           | Oracle Action
           | SimulateVs Agent
           | Hypothetical Action Action Agent
```

Here `Not x` flips the output of `x`. `And x y` treats `Cooperate` as
true and `Defect` as false. `SelfHistory n a` and `OpHistory n a`
query what each agent has did `n` steps in the past, or returns `a` if
this takes us before the start of simulation. `Oracle a` asks what our
opponent is about to do against us, or returns `a` if we are at the
maximum depth. `SimulateVs x` asks what our opponent would do if
instead he were playing against `x`. Finally, `Hypothetical a a' x`
adds a pair of actions to the history and then runs `x`, so if `x`
checks the immediate history, it appears we had done `a` and our
opponent had done `a'`.

Here are some obvious handwritten agents to try:

``` haskell
alwaysCooperate :: Agent
alwaysCooperate = Const Cooperate

alwaysDefect :: Agent
alwaysDefect = Const Defect

titfortat :: Agent
titfortat = OpHistory 1 Cooperate

-- Cooperates until the opponent defects
unforgiving :: Agent
unforgiving = And (SelfHistory 1 Cooperate) (OpHistory 1 Cooperate)

-- Does whatever our opponent will
mirror :: Agent
mirror = Oracle Cooperate

-- Cooperates, if us cooperating this turn means our opponent will cooperate next turn
justice :: Agent
justice = And (Hypothetical Cooperate Cooperate mirror)
              (Hypothetical Cooperate Defect mirror)

-- Cooperates if our opponent cooperates with `alwaysCooperate`
karma :: Agent
karma = SimulateVs alwaysCooperate

-- Cooperates if our opponent (given a good start) cooperates with `titfortat`
untrusting :: Agent
untrusting = Hypothetical Cooperate Cooperate (SimulateVs titfortat)

-- Cooperates, unless we don't get punished for not cooperating
exploitative :: Agent
exploitative = And (OpHistory 1 Cooperate) (Not (Hypothetical Defect Cooperate (Oracle Defect)))
```

If we run these all against each other in battles of 100 trials and tally up the total points:

``` text
(alwaysDefect    , 1208)
(alwaysCooperate , 1800)
(justice         , 1900)
(untrusting      , 1900)
(titfortat       , 2000)
(karma           , 2004)
(titfortat       , 2199)
(exploitative    , 2398)
```

Maybe more interesting is if we look for good agents by randomly
generating them. I did this by looking for agents with good scores
against the current pool, and if (to my eyes) it didn't look like
anything better was going to be found, stopping the run and adding the
new champion into the pool. Here are these agents, the workings of
which are pretty mysterious to me:

``` haskell
good1 = SimulateVs (Not (Oracle Defect))
good2 = Hypothetical Cooperate Cooperate (SimulateVs (Not (Oracle Cooperate)))
good3 = SimulateVs (SimulateVs (Const Cooperate))
good4 = SimulateVs (And (Not (Oracle Defect)) (SimulateVs (OpHistory 5 Cooperate)))
good5 = Not (And (Oracle Defect) (Not (SimulateVs (And (Oracle Cooperate) (SimulateVs (Const Defect))))))
good6 = Not (And (SimulateVs (Const Cooperate)) (Oracle Defect))
good7 = Not (And (Oracle Defect) (Hypothetical Cooperate Defect (Oracle Defect)))
good8 = Not (SimulateVs (Not (Hypothetical Defect Cooperate (Oracle Defect))))
good9 = And (Not (Oracle Defect)) (OpHistory 1 Cooperate)
good10 = And (Not (Hypothetical Defect Cooperate (And (Oracle Defect) (Not (SelfHistory 3 Defect))))) (OpHistory 3 Cooperate)
```

Which when battled:

``` text
(justice         , 3012)
(mirror          , 3100)
(alwaysCooperate , 3300)
(good2           , 3300)
(good3           , 3320)
(good6           , 3650)
(untrusting      , 3704)
(good1           , 3750)
(alwaysDefect    , 3824)
(good4           , 4294)
(karma           , 4318)
(good5           , 4400)
(good7           , 4850)
(good8           , 4850)
(titfortat       , 4999)
(good9           , 5679)
(exploitative    , 6611)
(good10          , 6626)
```

It makes sense that `alwaysCooperate` places so poorly: good agents should find it easy to exploit. Also, our `exploitative` agent still seems hard to beat!

<!-- Now let's be a little more systematic. TODO -->

Here's the code: [github](https://github.com/mvr/oracle/blob/master/src/Exp.hs)

The next thing to try would be to enumerate all agents of a given
size, and draw a graph of who beats who.

Another avenue would be to change the available primitives. Instead of
using `Oracle`, we could instead allow an agent access to its opponent's
code and add primitives to pattern match against it.
