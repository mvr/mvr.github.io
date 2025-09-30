---
title: No‑Three‑In‑Line, Quicker
published: September 30, 2025
tags: code, cuda, train-of-thought
---

<script defer src="/life/lv-plugin.js"></script>

The code in the [previous post](/posts/no-three-in-line.html) is
fairly quick, but of course we'd always prefer it to be quicker. I'll
keep a record here of things I've tried and whether they worked.
<!--more-->


### One-hop Cells

After a bit more thinking, I realised there's a class of lines through
a point that can be handled all at once rather than one at a time.

We saw in the last post that relatively-prime endpoints that are far
away from each other can be skipped, when an additional point in
either direction would be off the edge of the grid. What about points
a little closer than that, where it's possible the next point on the
line is still in bounds?

```lifeviewer
x = 14, y = 14, rule = LifeHistory
14B$4BA9B$14B$14B$14B$14B$7BA6B$14B$14B$14B$14B$10BD3B$14B$14B!

#C [[ ZOOM 8 ]]
```

One way to describe the location of that in-bounds cell is that we've
taken the upper endpoint and reflected it through the lower endpoint.
Now here's the trick: if we fix the lower point, we can determine
these one-hop cells for a whole collection of upper endpoints
simultaneously by reflecting the whole board through the lower
endpoint:

```lifeviewer
x = 14, y = 14, rule = LifeHistory
6BA7B$4BABA7B$14B$3BA10B$BABA10B$14B$7BA6B$14B$11BDBD$11BD2B$14B$8BDB
D3B$8BD5B$14B!

#C [[ ZOOM 8 ]]
```

This is fast to do, because it's just a bit-reverse to deal with the
horizontal reflection and a shuffle to deal with the vertical
reflection.

For some offsets, these one-hop points are the only possible
additional points on the line, so we can remove those offsets from the
mask of "relevant" ones that need the fully general treatment. The new
mask for $13 \times 13$ looks as follows: the thing to notice is that
the solid square in the middle is smaller than it was before:

```lifeviewer
x = 25, y = 25, rule = LifeHistory
AB3ABAB3A3B3ABAB3ABA$BA21BAB$ABABAB3ABA3BAB3ABABABA$A2BA2BA2BA5BA2BA
2BA2BA$ABABABABABA3BABABABABABA$5BA13BA5B$AB3ABAB3A3B3ABAB3ABA$2BA4BA
9BA4BA2B$ABABABAB4AB4ABABABABA$A2BA2BAB4AB4ABA2BA2BA$ABABABAB4AB4ABAB
ABABA$8B4AB4A8B$25B$8B4AB4A8B$ABABABAB4AB4ABABABABA$A2BA2BAB4AB4ABA2B
A2BA$ABABABAB4AB4ABABABABA$2BA4BA9BA4BA2B$AB3ABAB3A3B3ABAB3ABA$5BA13B
A5B$ABABABABABA3BABABABABABA$A2BA2BA2BA5BA2BA2BA2BA$ABABAB3ABA3BAB3AB
ABABA$BA21BAB$AB3ABAB3A3B3ABAB3ABA!

#C [[ ZOOM 8 ]]
```

We've decreased the proportion of expensive pairs from 304/625
down to 256/625.


### Soft Branching

An idea we can use from [Silk](https://gitlab.com/apgoucher/silk) is
to do more work in each node by "soft branching" on the values in a
cell, or along a row or column. This means speculatively setting the
value of a cell either way, propagating information, and finally
taking the intersection of everything learned in the two branches.
It's fairly often the case that either value of a particular unknown
cell ultimately forces some other cell to be empty regardless of which
option we take, so we do increase the information learned overall even
if neither branch directly leads to a contradiction. Here's a simple
though slightly contrived example:

```lifeviewer
x = 14, y = 14, rule = LifeHistory
14B$3BD10B$3BD10B$3BD10B$14B$3BDA9B$3BD10B$3BD2BD7B$3BD10B$3BDA9B$14B
$3BD10B$3BD10B$3BD10B!

#C [[ ZOOM 8 ]]
```

Regardless of where the two points in the nearly-full column end up
being, at least one of them is going to eliminate the point out to the
right.

Doing this check for every cell would be far too expensive so, we need
a heuristic for what it's worth soft branching on. A natural choice is
to soft branch on the vulnerable cells one-by-one. This does, at
times, make a lot of extra progress at each node without requiring any
additional interaction with the queue, but unfortunately this ends up
being a little slower overall. Maybe there's a better heuristic for
cells that it's *really* worth soft branching on, but I don't have any
ideas for this.


### Saturating 2-Bit Counter

We get a lot of mileage out of forcing cells whose value is implied by
the counts of existing values in their row or column. To count
vertically along each column, we use a binary counter and reduce
across a warp.

As it stands, this counter has two bits `bit0` and `bit1` and an
`overflow` bit, so the counter can store a state in $\{0, 1, 2, 3,
{\gt}3\}$. But, in fact, we never need to know we have a count of $3$
exactly, so we can pack the information into two bits `bit0` and
`bit1` with `bit0 & bit1` now representing ${\gt}2$.

We might worry that the bit manipulation for combining two such
counters gets more complicated, but we win here too.

```cpp
uint32_t bit0 = 
    (x.bit1 & y.bit1) | (x.bit1 & y.bit0) | (y.bit0 & y.bit1) 
  | (x.bit0 ^ y.bit0);
uint32_t bit1 = 
    x.bit1 | y.bit1 | (x.bit0 & y.bit0);
```

Calculating the new `bit1` should be clear: the result is ${\geq}2$
whenever the inputs are ${\geq}2$, or when we are calculating $1 + 1$.
The expression for `bit0` is less obvious but can be brute forced.
There are only 4 cases where the output `bit0` is 0, i.e. $0 + 0$,
$1 + 1$, $2 + 0$ and $0 + 2$, and the expression above covers all the
other cases.

By some miracle, the first line for `bit0` is calculating the *bitwise
majority* of the values `x.bit1`, `y.bit0` and `y.bit1`, and this can
be done using a single `LOP3.LUT` instruction. (Unfortunately the
compiler doesn't realise this itself, so we have to write it
manually.)

```cpp
uint32_t maj3(uint32_t x, uint32_t y, uint32_t z) {
  uint32_t w;
  asm("lop3.b32 %0,%1,%2,%3,0xE8;\n" : "=r"(w) : "r"(x), "r"(y), "r"(z));
  return w;
}

uint32_t bit0 = maj3(x.bit1, y.bit0, y.bit1) | (x.bit0 ^ y.bit0);
uint32_t bit1 = x.bit1 | y.bit1 | (x.bit0 & y.bit0);
```

In all, this should compile to just 4 instructions. This calculation
for a "saturating" 2-bit counter is surely known, but I didn't find it
after a little Googling. Using this simpler counter was about a 10%
speed improvement.


### Alternating Forcing Axis

Forcing along orthogonal lines is an idempotent operation along each
axis; once we've checked the counts horizontally and forced cells
accordingly, it's impossible for that to reveal more horizontal counts
that cause cells to be forced. It's only when alternating the
horizontal and vertical checks that we make progress.

Currently we do a horizontal round and a vertical round, and repeat
this process if it has resulted in any changes from where we started.
It seems like we might get a nice speed improvement by keeping track
specifically of which axis has caused changes, and avoiding a
pointless vertical round if the previous horizontal round didn't
change anything.

To my surprise, this is actually slightly slower than always doing a
horizontal round followed by a vertical round in pairs. I find it hard
to explain why this might be; something for future investigation I
suppose.


### Finding a Bit

Something a little unexpected is that each use of the `__ffs`
intrinsic gets compiled to a pair of instructions like

```
    BREV R30, R30
    FLO.U32.SH R33, R30
```

That is, it bit-reverses the value and then finds the leading set bit,
presumably because the underlying hardware does not have an
instruction equivalent to `__ffs`.

The two instructions above have pretty bad throughput. In most places
`__ffs` appears, it's not actually important to find the lowest set
bit; we just need to find *some* set bit. Switching to `31 - __clz(x)`
gets rid of the `BREV` step and so ends up slightly faster overall.


### 2-Factors

It would be nice to have at quick test for whether it's possible to
complete the configuration considering only the orthogonal
constraints. There's a reformulation of this problem. Define a
bipartite graph where one set of vertices represents the rows and the
other the columns. Each edge is then a cell of the grid, and we'll
only include edges where that cell of the grid has not yet been ruled
out. A completed grid is then a spanning subgraph where every node has
degree two, that is, a 2-factor. This means that the graph is
partitioned into a set of disjoint loops (a structural feature of the
problem that perhaps should have been obvious from the start).

In terms of the original grid, you can choose a point and trace out
the corresponding loop by moving to the partner of that point in the
same row or column, alternating each time so you make progress.
Sometimes there's only one loop, but not necessarily:

```lifeviewer
x = 14, y = 14, rule = LifeHistory
5BA3BA4B$6BC3DC3B$5BAD2BAD3B$BC4DC3BD3B$ADA7BD3B$BD8BDABA$BD8BCDCB$BC
DC8BDB$ABAD8BDB$3BD7BADA$3BD3BC4DCB$3BDA2BDA5B$3BC3DC6B$4BA3BA5B!

#C [[ ZOOM 8 ]]
```

So, what would be nice is a way to quickly check whether the graph for
the remaining cells admits a 2-factor. And surprisingly (to me), this
can be done in polynomial time.^[In contrast to the similar problem of
finding Hamiltonian cycles, which is NP-complete.] I was feeling
awfully clever at this point, but some simple tests show that only
around 1/1000 configurations in the search tree would be eliminated by
this check, and so it's not remotely worth doing.

<!-- Once we have a 2-factor, it would be nice to know if any of the edges -->
<!-- are "essential", that is, must appear in any 2-factor. One way to do -->
<!-- this is to take the 2-factor we've found and go through the cells -->
<!-- one-by-one, testing whether we can find a new 2-factor once that cell -->
<!-- is removed.  -->

<!-- TODO: analogue of Dulmage–Mendelsohn decomposition "Work -->
<!-- by Cornuéjols, Pulleyblank, Frank, Király, and others developed -->
<!-- f-factor decompositions that play a role analogous to the -->
<!-- DM-decomposition for perfect matchings." -->

If we in addition consider the lines with slope $\pm 1$, our hit rate
for ruling out configurations early is 1/3, which is much more
promising. Adding these constraints gets us back into the [land of
NP-completeness](https://jair.org/index.php/jair/article/view/11079)
(probably). Still: it may be worth doing a per-node lookahead of this
simpler kind, something I'll look into.


### Literature Search

This problem of filling a grid in a way that matches the orthogonal
constraints is reminiscent of
[Nonograms](https://en.wikipedia.org/wiki/Nonogram). The difference is
that for Nonograms, one is told the specific runs of points, rather
than a total count. I looked at some Nonogram solvers, but they
don't seem to hold any tricks that will help here.

Our problem is much closer to [Discrete
Tomography](https://en.wikipedia.org/wiki/Discrete_tomography), the
study of reconstructing binary images from their projections. In our
case the projections are always the same; the thing that makes our
problems distinct is that certain entries of the image are forced to
be empty in advance. There are a couple of papers in this direction
that focus on special cases, but nothing too useful.

```bib
@article {MR1355597,
    AUTHOR = {Kuba, A.},
     TITLE = {Reconstruction of unique binary matrices with prescribed
              elements},
   JOURNAL = {Acta Cybernet.},
  FJOURNAL = {Acta Cybernetica},
    VOLUME = {12},
      YEAR = {1995},
    NUMBER = {1},
     PAGES = {57--70},
      ISSN = {0324-721X},
}

@incollection {MR2446674,
    AUTHOR = {Brualdi, R. A. and Dahl, G.},
     TITLE = {Constructing {$(0,1)$}-matrices with given line sums and
              certain fixed zeros},
 BOOKTITLE = {Advances in discrete tomography and its applications},
    SERIES = {Appl. Numer. Harmon. Anal.},
     PAGES = {113--123},
 PUBLISHER = {Birkh\"{a}user Boston, Boston, MA},
      YEAR = {2007},
       DOI = {10.1007/978-0-8176-4543-4_6},
}

@article {MR3551626,
    AUTHOR = {Chen, Wei and Mo, Yanfang and Qiu, Li and Varaiya, Pravin},
     TITLE = {Constrained {$(0,1)$}-matrix completion with a staircase of
              fixed zeros},
   JOURNAL = {Linear Algebra Appl.},
  FJOURNAL = {Linear Algebra and its Applications},
    VOLUME = {510},
      YEAR = {2016},
     PAGES = {171--185},
      ISSN = {0024-3795},
       DOI = {10.1016/j.laa.2016.08.020},
}
```


### CUDA Wrangling

One head-slapper was that I was compiling for Compute Capability 8.6
whereas my hardware has Compute Capability 8.7. This was another 10%
improvement for free. Specifying `__launch_bounds__` to ensure
maximum occupancy also helped a little, though not as much as I hoped.

One wrinkle I haven't been able to smooth out is that the compiled
code is littered with `BRA.DIV` instructions, guarding against the
possibility that threads in the warp have diverged. This never
actually happens when the code is run, but I haven't been able to
convince the compiler of this. Besides wasting a little time, I'm
worried that those instructions are blocking the compiler from
performing additional optimisations.

Here's where we've landed after applying all tricks in this post:

| Size | Before (s) |   | After |
|------|------------|---|-------|
| 12   | 13.5       | → | 8.2   |
| 13   | 65         | → | 39    |
| 14   | 1231       | → | 771   |
| 15   | 15672      | → | 7801  |

There is a [mysterious
table](https://wwwhomes.uni-bielefeld.de/achim/no3in/effort.txt) on
Flammenkamp's website giving the "relative effort" of generating all
solutions for different size of grid, which seems to suggest that
$16\times 16$ should be easier than $15\times 15$. If this is so, I
haven't found the trick!

