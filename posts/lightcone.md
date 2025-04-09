---
title: Searching for Conduits at Lightspeed
published: February 9, 2025
tags: gol, code
---

<script defer src="/life/lv-plugin.js"></script>

Conduits are a central piece of technology in Conway's Game of Life,
especially when engineering new pieces of stable circuitry. A conduit
moves an active pattern (like a [Herschel] or [B-heptomino]) from one
location to another by allowing it to react with a collection of
stable catalysts. Along the way, it may release some gliders, produce
additional active patterns or perform some other useful interaction,
and so new pieces of machinery can be constructed by chaining conduits
together.

[Herschel]: https://conwaylife.com/wiki/Herschel
[B-heptomino]: https://conwaylife.com/wiki/B-heptomino

As one very famous example we have the [Fx77] conduit discovered by
Dave Buckingham, which moves a Herschel forwards in 77 generations,
also flipping it.

[Fx77]: https://conwaylife.com/wiki/Fx77

```lifeviewer
x = 41, y = 23, rule = LifeHistory
11.A$11.3A12.B.3B.B$14.A10.13B$13.2A3.B5.2B2A11BD$13.8B3.2B2A9B3DB$
15.8B.13BDBD$15.22BD$14.15B.2B2.2B$12.17B$10.18B$10.2BC15B$9.3BCBC4B.
7B$10.2B3C13B$9.5BC14B$8.10B2.8B$7.4B11.6B$6.4B12.5B$5.4B14.4B$4.4B
15.4B$3.4B17.B.B2A$2.4B20.BA.A$.4B24.A$4B25.2A!

#C [[ AUTOSTART ]]
#C [[ GPS 20 ]]
#C [[ GRID PAUSE 2 T 77 PAUSE 2 LOOP 78 ]]
```

Many software tools have been written to search for new conduits:
[`ptbsearch`] by Paul Callahan, [Catalyst] by Gabriel Nivasch, `catgl` by
Dave Greene, [CatForce] by Michael Simkin and [LocalForce] by Nico Brown,
to name a few. Typically these tools are handed an active pattern and
a collection of catalysts, and spit out all placements of catalysts
that interact with the active pattern and recover. At this point,
hundreds of conduits are known, converting to and from various active
patterns. But the more the merrier!

[`ptbsearch`]: https://github.com/conwaylife/ptbsearch
[Catalyst]: https://github.com/dvgrn/b3s23life/tree/main/catgl
[CatForce]: https://github.com/simsim314/CatForce
[LocalForce]: https://github.com/nicobrownmath/LocalForce

I've been working on yet another search tool, called [Lightcone].
Lightcone is pretty speedy: it can find apg's [Spartan G-to-W] on
[this] input file in just a couple of seconds. In this post I'll
explain some of the tricks it uses to cut down the search time.

[Lightcone]: https://github.com/mvr/lightcone
[Spartan G-to-W]: (https://conwaylife.com/wiki/Spartan_G-to-W-to-H)
[this]: https://github.com/mvr/lightcone/blob/master/tests/thessalonic.toml

<!--more-->

Clicking on the patterns in this post will copy them to the clipboard
so you can paste them into Golly. I'm using Chris Rowett's
[LifeViewer](https://github.com/rowett/lifeviewer) to display the
patterns.


Brute Force and Just-in-time
----------------------------

To search for conduits, an obvious method is brute force: simply test
each possible arrangement of catalysts and check whether all the
catalysts recover. This is the approach taken by the original version
of CatForce by Michael Simkin, hence the name.

Original CatForce does cut down on the search space slightly by first
calculating the reaction envelope of the input pattern and making sure
that at least one catalyst lies in that envelope. After that, though,
all possible placements for the other catalysts are tried (within
user-specified bounds). This is the case even if the other catalysts
do not interact with the pattern at all! Original CatForce applies
most of its filtering *after* a candidate configuration has already
been run all the way through.

For a small number of catalysts, say 1 to 3, this strategy can run to
completion in a reasonable amount of time. And if many of the
catalysts are expected to be transparent then the brute force approach
is not too wasteful, because the configurations have to be run to
completion to determine whether these transparent catalysts recover
(although testing non-interacting placements is still pointless). One
spectacular example is this Herschel-to-MWSS reaction found by Tanner
Jacobi.

```lifeviewer
x = 32, y = 32, rule = LifeHistory
20.B3D2B$19.BD2BDB$20.3BD2B$19.D3BDB$20.3BD2B$19.BDBD2B$20.6B$19.6B$
20.6B$19.6B$20.6B$12.B6.6B$11.3B6.6B$9.6B4.7B$3.4B2.7B3.8B$.27B$B2C
26B$3CB2C21BD2B$B2CB3CB2A16BDBD2B$3CB2C2B2A16BDBD2B$2C25BD4B$5.26B$7.
B2.20B$10.19B$10.17B$10.18B$11.5B.11B$11.4B2.12B$18.4B.2BA3B$23.BABA
2B$24.2A2B$26.B!

#C [[ AUTOSTART ]]
#C [[ GPS 20 ]]
#C [[ GRID PAUSE 2 T 103 PAUSE 2 LOOP 104 ]]
```

For searches involving non-transparent catalysts we can do much
better. CatForce treats the first catalyst specially to ensure that it
will interact with the active region. But we can generalise that idea
to every subsequent catalyst placement too. Rather than trying every
possible placement, just in case, throughout the search we watch the
active pattern evolve and only try catalyst placements that will
interact with it in the next generation. It is also easy to track the
history of the pattern, and avoid any placement that would have
interacted on some earlier generation. This just-in-time placement is
used by all the other search tools I am aware of, including my
"Symmetric" fork of CatForce.

On its own this is a huge improvement, because the reaction envelope
can vary a lot after just a single catalyst placement.

```lifeviewer
x = 67, y = 46, rule = LifeHistory
55.B$50.7B$50.8B$50.9B$50.10B$50.9B$51.8B$48.11B$48.12B3D$47.13BDBD$
46.14BDBDB$45.2B3D11BD2B$45.10B2D7B$44.BD5BDB2D2BD7B$43.2BD5B2DBDBD8B
$43.2BD5BD3BD10B$43.24B$44.2B.3D16B$48.B.3BD10B$51.2BD9B$51.2BD8B$43.
A7.11B$43.3A5.11B$46.A4.12B$17.4B24.2A3.B.11B$16.2D5B22.18B$16.2D5B2D
22.17B$17.6B2D22.13BD4B$16.9B21.14BD5B$14.5BD5B19.16BD4B$12.7BD5B17.
23B$12.2BC4BD3B.B2A15.2BC19B$11.3BCBC4B3.BA.A13.3BCBC7BD8B$12.2B3C4B
6.A14.2B3C7BD7B$11.5BC4B6.2A12.5BC7BD7B$10.10B20.10B2.8B$9.4B26.4B10.
7B$8.4B26.4B12.6B$7.4B26.4B12.7B$6.4B26.4B14.6B$5.4B26.4B14.6B$4.4B
26.4B15.7B$3.4B26.4B16.6B$2.4B26.4B18.4B$.4B26.4B19.4B$4B26.4B21.2B!

#C [[ AUTOSTART ]]
#C [[ GPS 30 ]]
#C [[ GRID PAUSE 2 T 169 PAUSE 2 LOOP 170 ]]
```

The overall structure of the search is then a depth-first search over
catalyst positions. At each node of the search tree, we either place a
catalyst which interacts on the current generation, or advance to the
next generation.


Contacts and Approaches
-----------------------

The first new idea, which functions independently of the others, is to
only try placements where the active pattern is in the right shape for
the catalysis to succeed. Consider the ordinary eater:

```lifeviewer
x = 39, y = 7, rule = LifeHistory
5.C9.2C8.C10.C$5.C.C7.2C8.C9.C.C$4.D2C7.D2C7.DC8.DC2.C$2.2AD7.2AD7.2A
D7.2AD.2C$.A.A7.A.A7.A.A7.A.A$.A9.A9.A9.A$2A8.2A8.2A8.2A!
```

The vast majority of successful interactions with the head of the
eater begin with the birth of exactly those two marked cells, cells
that wouldn't otherwise be born if not for the eater. I have been
calling such perturbed cells the *contact point* of the catalyst.
These contact points are easy to calculate in a pre-processing step.

If the initial interaction does not match, it is overwhelmingly likely
that the catalyst is going to explode rather than recovering, so
knowing the contact points lets us cut down the number of placements
to check. Looking at the upper contact cell for the eater, a birth is
caused when it gains exactly two active neighbours. When iterating
through the reaction envelope, we only need to consider offsets for
the eater that put that contact cell next to exactly two active cells.

In addition to checking the contact point, we can look at more of the
active pattern. The following (contrived) situation causes the correct
births, but is also overwhelmingly likely to not work because the
active pattern has the wrong shape.

```lifeviewer
x = 6, y = 6, rule = LifeHistory
3C.C$4.DC$2.2AD$.A.A$.A$2A!
```

The required shape of active cells I have been calling the *approach*.
In Lightcone, I check that the approach is correct by extracting a 5x5
"signature" around each possible contact point in the active pattern,
and comparing it to the precomputed 5x5 signature around the contact
point in the catalyst. (This is a bit faster than dealing with the
entire state.) For the eater, the approach looks like:

```lifeviewer
x = 6, y = 6, rule = LifeHistory
2.3DC$2.3DC$2.2A2D$.ADA2D$.A4D$2A!
```

The cells to the right of the two active ones are unrestricted, there
are many possibilities that work. Other catalysts are much more
particular about the approach they will successfully interact with.

```lifeviewer
x = 14, y = 11, rule = LifeHistory
3.2A$3.A5.DC2D$2A.A5.2D3C$A.2A.2A2.5D$5.A.A.DA3D$5.A.4A3D$6.A6.A$7.A
2.4A$6.2A2.A$11.A$10.2A!
```

There is a trade-off to this strategy, of course. We are deliberately
excluding unusual interactions that begin in some other way. Each
individual unusual interaction has a very small chance of working, but
collectively we might be cutting out a lot of interesting results.
Checking approaches really does make a huge difference to performance,
so the reader will have to decide whether losing out on these
serendipitous results is worth it.


Problems and their Lightcones
-----------------------------

If a non-transparent catalyst is destroyed, it is unlikely to reform
by chance. For this reason, most search tools allow the user to
specify *required* cells of the catalyst, which are not allowed to
have their state flipped at any point. For the eater, this might be
all the marked cells in:

```lifeviewer
x = 5, y = 6, rule = LifeHistory 
2.2D$.2DCA$.DCDC$2DC2D$D2CD$4D!
```

This includes most of the cells in a halo around the catalyst; if a
neighbouring cell on the left side is flipped on, the eater is
probably in the process of exploding.

If after advancing the overall state we find that one of these
required cells has its state flipped, we can drop that branch of the
search: it is safe to assume that the catalyst will never recover.

A required cell failing is an example of what I call a *problem* that
may occur with a given configuration. There are a handful of other
problems that Lightcone can currently test for, depending on the input
parameters:

* Required cells failing, as above.
* A filter for a particular state not being matched. For example, you
  may want to make sure that a glider escapes from a reaction.
* A catalyst failing to recover within a specified time.
* An overall time limit being reached without all catalysts recovered.
* An overall time limit being reached without any catalysts
  interacting at all.
* A transparent catalyst not actually acting transparently. (This is
  most common with blocks, and is intended to avoid "boring" results.)
* A stationary piece of junk surviving for too long.

Any time one of these problems occurs, we can safely drop that branch
of the search. A key property of these problems is that they are
"monotonic", in the sense that no placement that interacts on a later
generation can possibly cause the problem to be avoided.

Here is the next key idea. Rather than waiting until a problem occurs
to react to it, at each node of the search tree we run a lookahead
(with no further catalyst placements) to find the next problem that
will occur. Once its time and location is known, this is a strong
constraint on any possible solution. There *must* be at least one
additional catalyst placement at a time and location that has a chance
of averting the problem. That is, there must be at least one catalyst
placement that has its contact point within the past lightcone of the
problem, and so we can save time by restricting our attention to
finding this catalyst placement first.

For example, consider the following interaction, with the required
cells of the eater marked. After 36 generations we encounter a
problem: one of the required cells of the eater has flipped.

```lifeviewer
x = 46, y = 13, rule = LifeHistory
33.4B$32.A6B$32.2A6B$33.7B$32.10B$30.12B$28.12B2A$A27.12BA2B$A.A8.ACD
13.10B4.A2D$3A7.DC4D12.9B3.DC4D$2.A7.2D3CD11.BA8B3.2D3CD$11.3DCD10.2A
8B5.3DCD$13.3D11.2A14.3D!
```

Now we rewind back to generation 29, the moment we interacted with the
first eater. To have a chance at solving the problem, the next
placement must be in the following box.

```lifeviewer
x = 23, y = 15, rule = LifeHistory
8.D2C12D$6.2AD3C11D$6.2A3D2C10D$5.3BCDC2DC9D$3.5B3C2D2C8D$.7BCDCD2C9D
$.7B4D3C8D$2BA5B6D2C7D$.A6B6DC8D$.3A4B7D3C5D$2.6B9DC5D$8.15D$8.15D$8.
15D$8.15D!
```

This doesn't help much right now, but as we advance in time this box
gets more and more restrictive. After 2 more generations, we are down
to the following region and no longer have to test any placements that
interact with the left part of the active pattern; they are too far
away to solve the problem.

```lifeviewer
x = 21, y = 13, rule = LifeHistory
7.4AB$6.2AB4A$6.AB2AD2C8D$5.3BABDCD2C6D$3.4B2AB2CD2C6D$.6B2ABC10D$.9B
DC9D$10B4D2C5D$.A8B4DC6D$.ABA6B5D3C3D$.2A6B.7DC3D$10.11D$10.11D!
```

In fact, this is the generation that contains the unique solution to
the problem. The following placement is barely close enough to have an
impact: the difference in the evolution of the pattern travels at near
lightspeed to reach the problem in time.

```lifeviewer
x = 21, y = 14, rule = LifeHistory
18.2A$7.4AB6.A$6.2AB4A3.A.A$6.AB2AD2C2D3C3D$5.3BABDCD3C5D$3.4B2AB2CD
2C6D$.6B2ABC10D$.9BDC9D$10B4D2C5D$.A8B4DC6D$.ABA6B5D3C3D$.2A6B.7DC3D$
10.11D$10.11D!
```

In general, after solving one problem there is a good chance we create
or expose a new problem that needs solving. In the following situation
the second eater saves the first one, and is in turn saved by the
third one.

```lifeviewer
x = 31, y = 22, rule = LifeHistory
14.2A$15.A$15.A.AB10.2A$16.2AB.2B7.A$18.4B4.BA.A$18.6B2.B2A$17.10B$
10.4B3.11B$9.2D13B2D2B$9.2D13B2D2B$10.18B$9.17B$7.19B$5.19B$5.2BC15B$
4.3BCBC4B.3BD3B$5.2B3C4B2.2BD2B$4.5BC4B4.D4B$3.10B8.2A$2.4B15.A$.4B
17.3A$4B20.A!
```

This search strategy can cause placements to be tested
*non-chronologically*, because after solving a problem that occurs in
one part of the state, we may free ourselves to go back and interact
with some other part of the state. This non-chronological placement
appears to be fairly rare in practice.

```lifeviewer
x = 29, y = 19, rule = LifeHistory
22.2A$21.B2A2B$12.4B6.4B$11.2A5B2.6B$11.2A12B$12.8BD5B$11.10B4DB$9.
12BD2BDB$7.16B2DB$7.2BC15B$6.3BCBC4B.7B$7.2B3C4B2.6B$6.5BC4B3.6B$5.
10B6.4B$4.4B12.B2A2B$3.4B14.2A.B2A$2.4B18.BA.A$.4B22.A$4B23.2A!
```

The eater is placed in order to save the lower block, but once that is
done, Lightcone is able to rewind and place the upper block.

So much for the past lightcone of a problem. There is another place
where we can use lightspeed considerations to our advantage. As it
stands, after we place a catalyst, we begin anew and test all
locations within the envelope of the active pattern for whether they
are suitable for a catalyst placement. But we don't need to throw away
all the work we've already done: the new active reaction can only be
different from the previous one within the future lightcone of the
placement we just made. Outside that lightcone, the determinations we
made for whether a placement is valid can be reused.


Bloom Filter
------------

Some catalysts interact in similar ways and can cause the same
perturbation of an active pattern. The eater 1 and eater 2 are
notorious examples.

```lifeviewer
x = 37, y = 10, rule = LifeHistory
5.C9.2C8.C9.2C$5.C.C7.2C8.C.C7.2C$5.2C8.2C8.2C8.2C$2.2A8.2A6.A.2A6.A.
2A$.A.A7.A.A4.3A.2A4.3A.2A$.A9.A5.A9.A$2A8.2A6.3A.2A4.3A.2A$20.A.A7.A
.A$20.A.A7.A.A$21.A9.A!
```

Unless this duplication is somehow dealt with, it can cause a
combinatorial explosion in the search where all combinations of eater
1s and eater 2s are tried, even if they lead to the same perturbation.

```lifeviewer
x = 97, y = 41, rule = LifeHistory
56.A24.A$55.A.A22.A.A$55.A.A22.A.A$5.A24.A22.3A.2A19.3A.2A$5.3A9.A12.
3A9.A9.A4.B9.A9.A4.B9.A$8.A6.3A15.A6.3A10.3AB2A6.3A10.3AB2A6.3A$7.2A
5.A17.2A5.A15.A.2A5.A15.A.2A5.A$7.6B.2A16.6B.2A16.6B.2A16.6B.2A$9.7B
18.7B18.7B18.7B$8.6B19.6B19.6B19.6B$6.5BD3B16.5BD3B16.5BD3B16.5BD3B$
4.6BDBD2B14.6BDBD2B14.6BDBD2B14.6BDBD2B$4.2BC3BDBD.B2A12.2BC3BDBD.B2A
.A10.2BC3BDBD.B2A12.2BC3BDBD.B2A.A$3.3BCBC2BDB.BA.A10.3BCBC2BDB.B2AB
3A7.3BCBC2BDB.BA.A10.3BCBC2BDB.B2AB3A$4.2B3C4B4.A11.2B3C4B3.B4.A7.2B
3C4B4.A11.2B3C4B3.B4.A$3.5BC4B4.2A9.5BC4B2.2A.3A7.5BC4B4.2A9.5BC4B2.
2A.3A$2.10B15.10B4.A.A8.10B15.10B4.A.A$.D3B21.D3B11.A.A7.D3B21.D3B11.
A.A$D3B21.D3B13.A7.D3B21.D3B13.A$3D22.3D22.3D22.3D2$56.A24.A$16.A24.A
13.A.A8.A13.A.A8.A$15.A.A22.A.A12.A.A7.A.A12.A.A7.A.A$5.A9.A.A12.A9.A
.A10.3A.2A6.A.A10.3A.2A6.A.A$5.3A6.2A.3A10.3A6.2A.3A7.A4.B6.2A.3A7.A
4.B6.2A.3A$8.A6.B4.A12.A6.B4.A7.3AB2A6.B4.A7.3AB2A6.B4.A$7.2A5.2AB3A
12.2A5.2AB3A10.A.2A5.2AB3A10.A.2A5.2AB3A$7.6B.2A.A14.6B.2A.A14.6B.2A.
A14.6B.2A.A$9.7B18.7B18.7B18.7B$8.6B19.6B19.6B19.6B$6.5BD3B16.5BD3B
16.5BD3B16.5BD3B$4.6BDBD2B14.6BDBD2B14.6BDBD2B14.6BDBD2B$4.2BC3BDBD.B
2A12.2BC3BDBD.B2A.A10.2BC3BDBD.B2A12.2BC3BDBD.B2A.A$3.3BCBC2BDB.BA.A
10.3BCBC2BDB.B2AB3A7.3BCBC2BDB.BA.A10.3BCBC2BDB.B2AB3A$4.2B3C4B4.A11.
2B3C4B3.B4.A7.2B3C4B4.A11.2B3C4B3.B4.A$3.5BC4B4.2A9.5BC4B2.2A.3A7.5BC
4B4.2A9.5BC4B2.2A.3A$2.10B15.10B4.A.A8.10B15.10B4.A.A$.D3B21.D3B11.A.
A7.D3B21.D3B11.A.A$D3B21.D3B13.A7.D3B21.D3B13.A$3D22.3D22.3D22.3D!

#C [[ AUTOSTART ]]
#C [[ GPS 15 ]]
#C [[ GRID PAUSE 2 T 45 PAUSE 2 LOOP 46 ]]
```

Here I follow Adam P. Goucher's (unimplemented) idea for Silk to use a
Bloom filter to quickly test whether the reaction has been seen
previously. A Bloom filter lets us store the hashes of many millions
of states using relatively little memory. The trade-off is a small
risk of false positives, once too many hashes have been added to the
filter. Again following a suggestion by apg, the filter is reset to
empty once the chance of false positives reaches an intolerable level.

The criteria for when the active pattern is tested against the Bloom
filter were arrived at through trial and error. Right now, the pattern
is tested and added to the filter when

* all non-transparent catalysts are present,
* at least two generations have passed since the most recent catalyst
  was placed, and
* the population of the active pattern is at least 12.

Of course, there is another balance to be struck here. We want to
catch as many repeated reactions as possible, but not accidentally
consider some dying sparks as a repeated reaction if they were
actually arrived at by different means.


Future Ideas
------------

Lightcone is certainly not the last word in conduit searching. It's
much smarter than a brute force search, but still makes choices that
are obviously silly to the human eye. I'd love to hear ideas on any of
the following topics:

### Catalyst Welding

Lightcone only tries placing complete catalysts, and excludes any
placement that will cause two catalysts to interact. This inevitably
misses some solutions that involve non-trivial welds to pack catalysts
slightly closer together than otherwise possible. One unsatisfying
solution is for the user to provide pre-welded catalyst pairs as
input, but in an ideal world Lightcone would do this systematically
and automatically.

This will likely mean testing all interacting offsets of one catalyst
against another, and testing whether a weld is possible. Presumably a
SAT solver could make short work of this.

In a branch, I've had a go at implementing this, but the performance
hit is unreasonably high (at least with the method I'm using
currently).

### Avoiding Hopeless Placements

Currently, some time is spent on placements where the recovery
sequence of one catalyst leads to a cascade of interactions with
additional catalysts. For example, the following completely hopeless
chain of block reactions which takes 8 generations to finally fail:

```lifeviewer
x = 12, y = 10, rule = LifeHistory
3.2A.3A$5.A$2D8.2D$DCA2.2A2.ACD$DCA.D2CD.ACD$2D2.4D2.2D2$.2A6.2A$D2CD
4.D2CD$4D4.4D!

#C [[ AUTOSTART ]]
#C [[ GPS 2 ]]
#C [[ GRID PAUSE 2 T 7 PAUSE 2 T 8 PAUSE 2 LOOP 9 ]]
```

It would be nice if these could be avoided without hardcoding those
nearby block placements to be forbidden. It's unclear to me what the
correct heuristic is here. We have to be careful: not all tightly
packed catalysts are useless. The similarly close eater in the
following example is necessary for the block to survive.

```lifeviewer
x = 10, y = 11, rule = LifeHistory
2.2A$A.2A$3A$.A2$.2A3.2D$D2CD.AC2D$4D.CDCD$5.2DC2D$6.D2CD$6.4D!

#C [[ AUTOSTART ]]
#C [[ GPS 4 ]]
#C [[ GRID PAUSE 2 T 20 PAUSE 2 LOOP 21 ]]
```

### Beyond Tree Search

Lightcone always tackles the chronologically earliest *problem* that
occurs with the current configuration. In some situations this leads
to poor performance, especially when the active region splits into two
separated pieces.

```lifeviewer
x = 35, y = 31, rule = LifeHistory
26.2A$21.2A3.A.A2.2A$21.A6.A3.A$13.2A3.2A.A2.4A.3A$13.A4.A.A3.A2.A.A$
14.3A.A8.ABAB3A$16.A.2A2.B3.2B2ABA2.A$17.7B.6B2.2A$19.10B$10.A8.9B$
10.3A6.10B$13.A6.8B$12.2A3.B3.9B$12.8B.9B$14.18B2D$14.18BDBD$13.21BD$
11.20BDB2D$9.22B2D$9.2BC19B$8.3BCBC16B$9.2B3C15B$8.5BC12B.2B$7.10B2.
8B$6.4B10.9B$5.4B12.6B.2A$4.4B12.8B2A$3.4B14.6B.B$2.4B15.BDBD$.4B17.D
BD$4B18.3D!
```

A problem in the left region may be solved, revealing a problem in the
right region, and vice versa. This alternation causes a combinatorial
blowup in the time it takes to make progress on either side. In theory
it should be possible to choose which problem to focus on more
intelligently, but doing so naively causes even bigger issues:
Lightcone might go down a rabbithole solving problems on the left,
when it's easy to show that the problem on the right is unsolvable.

This may be an opportunity for some parallelism: if there are multiple
problems that are lightcone separated, we could attempt to resolve
them in different threads.

Results
-------

Finally, the fruits of our labour.

Sep 13rd, 2024: [Lx65
duplicator](https://conwaylife.com/wiki/Lx65_duplicator)

```lifeviewer
x = 39, y = 30, rule = LifeHistory
26.B$25.3D$25.BDB$24.2B3D$24.5B$24.6B$11.2A11.6B6.A$12.A11.5B5.3A$12.
A.AB7.6B4.A$13.2AB.3B4.6B3.2A$15.7B.8B.3B$15.18B$16.17BAB$15.17BABAB.
B$13.18BA2BA2B2A$11.18B.2B2AB.B2A$11.2BC15B2.3B3.B$10.3BCBC4B.7B2.3B$
11.2B3C4B2.6B2.B2AB$10.5BC4B2.7B2.2A$9.10B4.6B$8.4B10.6B$7.4B12.5B$6.
4B13.6B$5.4B14.6B$4.4B15.5B$3.4B16.2B3D$2.4B18.BDB$.4B19.3D$4B21.B!
```

Oct 10th, 2024: [Many
H-to-Gs](https://conwaylife.com/forums/viewtopic.php?p=195724#p195724),
one chosen at random shown here:

```lifeviewer
x = 39, y = 27, rule = LifeHistory
28.2B$26.5B$13.A11.7B2.2D$13.3A9.8BDBD$16.A6.12BD$15.2A3.B2.12B$15.8B
.10B$17.16B$17.15B$16.17B.B$14.20B2A$12.20B.B2A$12.2BC15B4.B$11.3BCBC
15B5.2A$12.2B3C15B5.A$11.5BC4B2.10B.BA.A$10.10B3.10B.B2A$9.4B11.11B$
8.4B13.4B2A4B$7.4B16.BA2BA3B$6.4B18.B2A3B$5.4B20.4B$4.4B19.5B$3.4B20.
2A$2.4B22.A$.4B20.3A$4B21.A!
```

Nov 5th, 2024: [Many more
H-to-Gs](https://conwaylife.com/forums/viewtopic.php?p=195724#p197316),
one chosen at random shown here:


```lifeviewer
x = 36, y = 30, rule = LifeHistory
18.2A$19.A$19.A.AB$20.2AB.2A$22.2B2AB$22.4B7.2A$22.5B6.A$21.6B3.BA.A$
21.8B.B2A$13.2A6.10B$14.A5.11B$14.A.AB2.12B$15.2AB.14B$4.2A11.17B$5.A
12.16BD$5.A.AB6.13B.6BD$6.2AB.3B.14B.4B3D$8.24B$8.25B$9.24B$8.25B$6.
26B$4.18B.B.7B$4.2BC13B5.6B$3.3BCBC4B.6B6.2B$4.2B3C4B2.B.5B5.2B$3.5BC
4B7.2A4.B2AB$2.10B8.A6.2A$.4B16.3A$4B19.A!
```

Feb 11th, 2025: HF78B

```lifeviewer
x = 37, y = 29, rule = LifeHistory
23.2A$22.B2AB$22.4B$23.2B$22.5B$21.7B.2B$15.A5.6B.3B$15.3A3.2B2A9BD$
18.A2.BA2BA9BD$17.2A2.2B2A10B2D$6.A10.17B2D$6.3A10.15BD$9.A6.18B$8.2A
3.B2.12B3.B$8.19B$10.16B$10.17B$9.18B$7.20B$5.20B.B2A$5.2BC13B5.BA.A$
4.3BCBC4B.7B7.A$5.2B3C4B2.4B.B2A5.2A$4.5BC4B7.BA.A$3.10B11.A$2.4B18.
2A$.4B$4B$3B!
```

Feb 20th, 2025: HLx86B
```lifeviewer
x = 40, y = 30, rule = LifeHistory
29.D$28.3D$27.2D2BD$26.6B$26.6B6.2A$25.7B6.A$26.8B.BA.A$26.8B.B2A$15.
2A9.10B$16.A5.2B.11B$7.2A7.A.AB.3B2.10B$8.A8.2AB.8B2A5B.BA$8.A.AB6.
10BA2BA5BA.A$9.2AB.3B2.11B2A6B.2A$11.26B$11.24B$12.18B$11.19B$9.20B$
7.21B$7.2BC13B$6.3BCBC4B.7B$7.2B3C4B2.B.2B.B2A$6.5BC4B7.BA.A$5.10B11.
A$4.4B18.2A$3.4B$2.4B$.4B$4B!
```


