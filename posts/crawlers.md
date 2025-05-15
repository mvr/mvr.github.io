---
title: Crawlers 
published: April 2, 2025
tags: code, cuda, gol
---

<script defer src="/life/lv-plugin.js"></script>

As a second foray into CUDA, I've written a simple program to hunt for
[crawlers] in the Game of Life. The kernel itself is too revolting to
make public, but the supporting [`LifeStateCU`] code may be useful for
others. (This could be seen as the first step in a GPU version of
`CatForce`/`LightCone`...)

[crawlers]: https://conwaylife.com/wiki/Crawler
[`LifestateCU`]: https://github.com/mvr/LifeAPI/blob/master/cuda/LifeStateCU.cu

<!--more-->

The method by which each state is stored is taken from apg's
[`lifelib`]. Briefly, each warp stores a 64×64 torus using one `uint4`
per thread. This `uint4` is a 2×2 array of `uint32_t` values,
representing a 64×2 strip of the state.^[Annoyingly, the decade-old
`CatForce` arranges its state in column-major rather than row-major
order, so there is a mismatch here.] Operations involving information
from multiple rows can be done using the efficient warp-level
intrinsics, avoiding any use of shared memory. 

[`lifelib`]: https://gitlab.com/apgoucher/lifelib/-/blob/master/cuda2/cudagol.h

For example, fetching the state of a single cell can be achieved
having the thread that contains the relevant row shuffle the value of
that row to all other threads:^[Of course you should try desperately
to avoid writing code that works one cell at a time! This is just an
example.]

```cpp
__device__ uint64_t LifeStateCU::row(int y) const {
  int src = (y & 63) >> 1;
  
  if (y & 1) {
    uint32_t lo = __shfl_sync(0xffffffffu, state.z, src);
    uint32_t hi = __shfl_sync(0xffffffffu, state.w, src);
    return (uint64_t)hi << 32 | lo;
  } else {
    uint32_t lo = __shfl_sync(0xffffffffu, state.x, src);
    uint32_t hi = __shfl_sync(0xffffffffu, state.y, src);
    return (uint64_t)hi << 32 | lo;
  }
}

__device__ bool LifeStateCU::get(int x, int y) const {
  uint64_t r = row(y);
  return (r >> x) & 1;
}
```

The particular bit twiddles used in the `advance()` step were
[discovered] by Julia "Vlamonster" using a SAT solver. I've added many
of the functions from `LifeAPI` to make it easier to work with.

[discovered]: https://binary-banter.github.io/game-of-life/

Given an active pattern, the program runs an almost-brute-force search
on a list of reagent placements, running them a fixed duration and
checking whether the active pattern reoccurs at any time, at any
location. Trying to match the active pattern is fairly expensive, more
expensive than advancing the state itself. We place reagents in the
reaction envelope of the pattern, rather than in some region fixed in
advance.

The list of reagents to try was determined by a one-off script that
ran some soups and catalogued the most common ash. Here's what it
found:

```lifeviewer
x = 126, y = 98, rule = B3/S23
91b3o$121b2o$b2o28b2o56bo5bo24bo2bo$b2o27bo2bo27b3o25bo5bo25bobo$31b2o
56bo5bo26bo2$91b3o21$32bo$31bobo38b2o$31bobo38b2o$32bo$91b2o$b2o24b2o
7b2o14b2o17b2o17bo2bo28bo$bobo22bo2bo5bo2bo13b2o17b2o17bo2bo27bobo$2bo
24b2o7b2o53b2o29bo2$32bo18b2o$31bobo17b2o$31bobo$32bo20$65bo$64bobo54b
o$o28bo5bo28bobo53bobo$o28bo5bo29bo25b2o27bobo$o28bo5bo55bobo27bo$92b
2o$2b3o26b3o25b2o$58bo2bo58b2o$59b2o59b2o21$119bo$119bo$62b2o55bo$b2o
28b2o29b2o28b2o$b2o28b2o58bobo$90bobo$b2o88bo$b2o27b2o29b2o$30b2o29b2o
$123b3o!

[[ ZOOM 2 ]]
```

That is, block, hive, blinker, traffic light, loaf, ship, honeyfarm,
blockade, pond, tub, half traffic light, 3/4 traffic light, teardrop,
ship, followed by

* This specific pair of block and hive: `b2o$o2bo2b2o$b2o3b2o!`
* Biblock: `2o$2o2$2o$2o!`
* Half blockade: `2o$2o2b2o$4b2o!`
* Wider half blockade: `b2o$b2o4$2o$2o!`
* Long boat: `2bo$bobo$obo$2o!` 
* This specific pair of blinkers: `o$o$o7$4b3o!`

Results
=======

Crawlers that can react more than once are surprisingly rare. Here are
the only remotely feasible ones that I found.

```lifeviewer
x = 16, y = 16, rule = B3/S23
7bo$6bobo$6bobo$7bo4$14b2o$14b2o4$o$obo$3o$2bo!

[[ ZOOM 5 ]]
```

```lifeviewer
x = 15, y = 13, rule = B3/S23
4b2o$4b2o7b2o$13b2o7$o$obo$3o$2bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 12, y = 19, rule = B3/S23
10b2o$10b2o5$4b2o$4b2o8$o$obo$3o$2bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 9, y = 7, rule = B3/S23
6b2o$5bo2bo$6b2o$o$obo$3o$2bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 15, y = 11, rule = B3/S23
o$obo$3o$2bo4$9b3o2$13b2o$13b2o!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 10, y = 17, rule = B3/S23
5bo$4bobo$4bobo$5bo$9bo$9bo$9bo7$o$obo$3o$2bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 17, y = 20, rule = B3/S23
o$obo$3o$2bo4$10bo$9bobo$9bobo$10bo2$5b2o7b2o$4bo2bo5bo2bo$5b2o7b2o2$
10bo$9bobo$9bobo$10bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 17, y = 30, rule = B3/S23
12b2o$12b2o9$o$obo$3o$2bo4$10bo$9bobo$9bobo$10bo2$5b2o7b2o$4bo2bo5bo2b
o$5b2o7b2o2$10bo$9bobo$9bobo$10bo!
[[ ZOOM 5 ]]
```

```lifeviewer
x = 22, y = 28, rule = B3/S23
21bo$21bo$21bo6$o$obo$3o$2bo4$10bo$9bobo$9bobo$10bo2$5b2o7b2o$4bo2bo5b
o2bo$5b2o7b2o2$10bo$9bobo$9bobo$10bo!
[[ ZOOM 5 ]]
```



<!-- x = 322, y = 237, rule = B3/S23 -->
<!-- bo$obo$obo$bo$11b2o$11b2o105bo$118bo$96b2o20bo$96b2o5$104bo$103b3o$21b -->
<!-- o80bo3bo$20b3o79b2ob2o$19bo3bo$19b2ob2o67$29bo$28bobo$28bobo103b2o$29b -->
<!-- o104b2o$320b2o$320b2o2$36b2o$36b2o2$214b2o98b2o$214b2o7b2o89b2o$22bo -->
<!-- 99bo100b2o$22bobo97bobo$22b3o97b3o$24bo99bo4$132bo77bo99bo$131bobo76bo -->
<!-- bo97bobo$131bobo76b3o97b3o$132bo79bo99bo2$127b2o7b2o$126bo2bo5bo2bo$ -->
<!-- 127b2o7b2o2$132bo$131bobo$131bobo$132bo33$16b2o$15bo2bo$16b2o$10bo15b -->
<!-- 2o$10bobo12bo2bo$10b3o13b2o$12bo53$12bo$12bobo196bo$12b3o196bo$14bo -->
<!-- 196bo2$127bo$126bobo$21b3o102bobo$127bo$25b2o104bo58bo$25b2o104bo58bob -->
<!-- o$131bo58b3o$192bo4$200bo$199bobo$122bo76bobo$122bobo75bo$122b3o$124bo -->
<!-- 70b2o7b2o$194bo2bo5bo2bo$195b2o7b2o2$200bo$199bobo$199bobo$200bo! -->
