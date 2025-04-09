---
title: Quickly Detecting Cool Still Lifes 
published: April 8, 2025
tags: code, cuda, gol
---

<script defer src="/life/lv-plugin.js"></script>

[QuFince] is a CUDA tool written by [apg] to conduct brute-force Game
of Life searches on the cartesian product of two sets of
configurations. That is, each configuration from set A is combined
with each configuration from set B, and run until stabilised. The
result is reported if some criteria are met. Right now this mean
either the result has some interesting period^[Interesting period here
typically means period not dividing 120, to rule out [blinkers],
[pulsars], [pentadecathlons] and [figure eights], among other things.]
(to hunt for [glider syntheses] of oscillators), or the result
contains a specified pattern (to hunt for [synthesis components] for
still lifes).

[QuFince]: https://conwaylife.com/forums/viewtopic.php?f=9&t=5997
[apg]: https://cp4space.hatsya.com/
[synthesis components]: https://conwaylife.com/wiki/Synthesis_component
[blinkers]: https://conwaylife.com/wiki/Blinker
[pulsars]: https://conwaylife.com/wiki/Pulsar
[pentadecathlons]: https://conwaylife.com/wiki/Pentadecathlon 
[figure eights]: https://conwaylife.com/wiki/Figure_eight
[glider syntheses]: https://conwaylife.com/wiki/Glider_synthesis

One thing that *can't* be done currently is reporting any combination
that results in an interesting still life directly. It's not so
obvious what "interesting still life" should mean exactly, but here
are some randomly chosen examples of things that should qualify:

```lifeviewer
x = 88, y = 30, rule = B3/S23
2o2b2o16b2o16b2o18b2ob2o19b2o$o4bo15bo2bo15bobo18bobobo14b2obo2bo$b4o
15bo2b2o17bo2b2o14bo3bo14b2obob2o$20b3o19bobobo15b3o18bo$b4o18b2o16b2o
bo35b2obob2o$o4bo14b2obobo16bobobo15b3o15bo2bobo$b4o15b2obobo16bo2b2o
14bo2bo16b2o2bo$24bo15bobo17bo2bo21bobo$3b2o35b2o19b2o23b2o$3b2o11$5b
2o16b2o19b2o14b2ob2o15b2o$b2obo2bo14bo2bo18bo16bobobo14bobo$obobobo15b
ob2o15b2obo16bo3bo16bo$o4bo15b2obo17bob2o16b3o16bo$b4o15bo3bo17bobo36b
2o$20b4o17b2obobo15b3o$b2o39bo2b2o14bo3bo15b4o$bobo18b2o16bobo18bobobo
14bo4bo$2bo18bobo16b2o18b2ob2o16b3obo$22bo60bob2o!

#C [[ ZOOM 4 ]]
```

We can't simply use a population threshold as our criterion for
interestingness, because the typical result of one of these QuFince
trials is a substantial amount of uninteresting junk. One option is to
do proper object separation, like [apgsearch], and then report any
object that is sufficiently rare. But a QuFince search can conceivably
test hundreds of billions of configurations, and full object
separation is simply too slow.

[apgsearch]: https://gitlab.com/apgoucher/apgmera

Here I'll present an alternative, a simple heuristic that works well
enough. <!--more--> There are some constraints, but most important is
that our mistakes only go in one direction. False negatives are
disappointing, because we might be missing out on a cool result. But
false positives are catastrophic, because they cause a firehose of
useless results, which defeats the purpose of using QuFince in the
first place.

To evaluate how we're doing, we'll test the heuristic on the 10,000
most common still lifes. We'll declare any still life with population
$\geq 18$ to be "interesting", and any still life with population
$\leq 10$ to be a deal-breaker that *must* be rejected.^[These
thresholds are somewhat arbitrary.] Our goal is to correctly classify
as many of the remaining 10,000 as possible.


## Chunky Regions

The first thing that comes to mind is that rare still lifes often have
a dense middle, so we should devise a fast check for dense regions. If
we fill in the "holes" in the ash, a dense region will show up as a
place where we can fit a decently sized square. Or stated a different
way, if we inflate the ash a bit, and then deflate the result a bit
more, the only way anything can remain is if there was some thing
decently sized to begin with.

```cpp
chunky_locations = ~(~bx.zoi()).zoi().zoi();
```

This is quite efficient to evaluate when the state is represented by
as a bitplane (as it is in QuFince and LifeAPI). It's a good start and
correctly identifies 7223/10000 still lifes, but it does have some
unacceptable false positives:

```lifeviewer
x = 88, y = 31, rule = B3/S23
5bo17bobo18b2o16b2o22b2o$3b3o16bob2o17bo2bo16bo2bo15b2o3bo$2bo19bo19bo
bobo16bobobo15bo2bo$3b3o17b3o17bobo18bobo16bobo$5bo19bo18bo20bo18bo14$
20b3o3b3o$4b2o40b2o$3bo2bo11bo4bobo4bo14bo2bo15b2o$3bobo12bo4bobo4bo
15b2o16b2o$b2obo3bo9bo4bobo4bo11b2o$o2bo3bobo10b3o3b3o12bo2bo17bo4b2o$
obo3bo2bo32bobo16bobo2bo2bo$bo3bob2o11b3o3b3o14bo17bobo2bobo$4bobo11bo
4bobo4bo31bo4bo$3bo2bo11bo4bobo4bo$4b2o12bo4bobo4bo2$20b3o3b3o!

#C [[ ZOOM 5 ]]
```

But changing the precise kind of inflation/deflation helps. To test
this systematically, I defined a handful of "neighbourhoods" and tried
all combinations. The neighbourhoods I tried include:^[I also threw in
some more exotic neighbourhoods, but none turned out to help.]

```lifeviewer
x = 25, y = 12, rule = LifeHistory
.3D8.D8.D.D$.DAD7.DAD8.A$.3D8.D8.D.D5$5D7.D7.D3.D$5D6.3D7.D.D$2DA2D5.
2DA2D7.A$5D6.3D7.D.D$5D7.D7.D3.D!
```

What worked by far the best was the following pair, so inflating by
the radius-1 von Neumann neighbourhood on the left and deflating by
the radius-2 Moore neighbourhood on the right.

```cpp
chunky_locations = ~(~bx.vonneumann()).zoi().zoi();
```

```lifeviewer
x = 14, y = 5, rule = LifeHistory
9.5D$.D7.5D$DAD6.2DA2D$.D7.5D$9.5D!
```

This achieves 6017/10000, but with none of the painful false
positives of the previous version.


## Line-of-five

Let's inspect some of the still lifes that don't get picked up by
this.

```lifeviewer
x = 110, y = 50, rule = LifeHistory
2A2.2A15.2A21.2A15.2A18.2A17.2A$A4.A14.A2.A16.2A2.A.A13.A2.A16.A2.C
16.A2.A$.4A16.3A16.A5.A14.A2.A15.2A.C17.3A$41.5C16.3A18.C.2A$.4A16.3A
59.C.2A16.3A$A4.A14.A3.A16.3A18.5C15.AC19.A2.A$2A2.2A15.4A15.A2.A18.A
4.A15.C21.2A$41.2A22.A.A15.C.A21.2A$21.4A40.2A17.2A21.A.A$21.A2.A83.
2A11$.2A26.A11.2A23.A14.2A3.A13.2A3.2A$A2.A21.2A.A.A9.A.A2.2A18.A.A
12.A2.A.A.A.2A9.A.C.A.A$A2.A2.2A16.A.A.A.A9.A5.A14.2A.A2.A13.A.A.A.A.
A12.C.A$.3A.A.A15.A2.A.2A11.5C15.2A.A.A15.2A.A.A.A11.AC.A.A$5.2A15.A.
2A38.2A20.2A.2A11.C2.2A$3.2A16.A.A17.3A17.3A36.A.C$2.A.A15.A2.A16.A2.
A16.A2.A36.2A$2.2A17.2A18.2A18.2A13$.A22.2A16.A19.2A17.2A21.2A$A.A.2A
14.2A2.2A15.A.C17.A2.A15.A2.A.2A17.A.A$A.A.A.A13.A19.A2.C18.3A16.A.A.
A.A13.A4.A$.A4.A14.5C14.2A.CA37.A4.A13.5C$2.4A20.A16.C18.3A18.4A$23.
3A17.C2.A14.A3.A35.3A$4.2A16.A21.5C12.A.2A18.2A15.A3.A$3.A.A17.3A23.A
10.2A.A19.2A15.2A.2A$4.A20.A20.A.A11.A2.A$46.2A13.2A!

#C [[ ZOOM 4 ]]
```

One thing that jumps out is that many of them contain a orthogonal
line of length at least 5. This is also efficient to test for, doesn't
occur in any of our deal-breakers, and improves the detection rate to
7337/10000. However, there's a downside: it does pick up the
[fourteener], a surprisingly common still life that happens to contain
a line of five.

```lifeviewer
x = 7, y = 5, rule = LifeHistory
4.2A$2A2.A.A$A5.A$.5C$3.A!
```

The fourteener is the 43<sup>rd</sup> most common still life, and one
needs to go down to 75<sup>th</sup> and then 138<sup>th</sup> to see
another line like that. The improvement is too good to give up, so I
just filter these fourteeners out as a special case.

[fourteener]: https://conwaylife.com/wiki/Fourteener

The new set of missed still lifes begins as follows, and it's less
easy to spot a common feature that could be tested for.

```lifeviewer
x = 109, y = 50, rule = B3/S23
2o2b2o15b2o17b2o19b2o26bo16bo$o4bo14bo2bo16bo2bo16bo2bo21b2obobo14bobo
$b4o16b3o17b3o16bo2bo2b2o16bobobobo10b2obo2bo$61b3obobo15bo2bob2o11b2o
bobo$b4o16b3o19b3o19b2o15bob2o18b2o$o4bo14bo3bo18bo2bo16b2o16bobo17b3o
$2o2b2o15b4o20b2o15bobo15bo2bo16bo2bo$47b2o13b2o17b2o18b2o$21b4o22bobo
$21bo2bo23b2o11$b2o3bo14bo20b2o17b2o19b2o18b2o2b2o$o2bobobob2o9bobob2o
15bo2bo15bo2bob2o14bo2bo17bo4bo$bobobobobo10bobobobo15b3o16bobobobo12b
obobo18b4o$2b2obobobo11bo4bo35bo4bo12bob2o$6b2ob2o11b4o16b3o18b4o14bo
19b4o$41bo3bo36b4o14bo4bo$24b2o15bob2o18b2o21bo13b2o2b2o$23bobo14b2obo
19b2o19bobo$24bo15bo2bo40b2o$41b2o11$b2o2b2o14b2o18bo21bo21b2o15b2ob2o
$bo4bo13bo2bob2o13bobo19bobo16b2obo2bo14b2obobo$2b4o14b2obobobo12bo2bo
17bo2bo16b2obob2o17bo2bo$23bobo2bo12b3o18b2o20bo17b2o2bobo$2b4o14b3o3b
2o53b4o17bo4bo$bo3bo14bo20b3o18b4o14bo19bobo$2bo37bo3bo16bo4bo13bobo
17b2o$obo37bobobo15bobo2b2o14b2o$2o39b2ob2o14bobo$61bo!

#C [[ ZOOM 4 ]]
```

Here's where I stopped. Any ideas?

I've been putting this into practice by searching for new glider
syntheses, crashing 6 randomly placed gliders together. You can see
some of the results as I go [here]. I work a lot more efficiently than
previous attempts (like the `moog_stdin` and `5Glider_stdin`
symmetries), because 1) I make sure the gliders actually collide
rather than positioning them completely randomly, and 2) I do the vast
majority of the work in a QuFince-style CUDA kernel rather than piping
an RLE-generator into apgsearch. 

[here]: https://catagolue.hatsya.com/census/b3s23/mvr_qufince_stdin

This has already turned up a handful of record-breaking syntheses
after just a couple of hours, and I'm looking forward to a lot more.
Here's one chosen at random. If you have a powerful GPU and also want
to do some searches, let me know!

```lifeviewer
x = 27, y = 21, rule = B3/S23
obo$b2o$bo18bo$20bobo$20b2o$15bo$13bobo$14b2o7$10b3o8b2o$12bo7b2o$11bo
10bo2$24b2o$24bobo$24bo!

#C [[ AUTOSTART ]]
#C [[ GPS 20 ]]
#C [[ GRID PAUSE 1 T 70 PAUSE 1 LOOP 71 ]]
```
