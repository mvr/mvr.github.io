---
title: No‑Three‑In‑Line
published: September 1, 2025
tags: code, cuda
---

<script defer src="/life/lv-plugin.js"></script>

The [No-three-in-line
problem](https://en.wikipedia.org/wiki/No-three-in-line_problem) asks
how many points can be placed on a $n \times n$ grid so that no three
points are on the same line, where the lines considered are of any
slope and not just orthogonal and diagonal. Each row/column can
contain at most 2 points, so clearly the answer is at most $2n$. The
real question is, can we actually achieve $2n$ for every grid size?
It's [conjectured](https://doi.org/10.4153%2FCMB-1968-062-3) that the
answer is "no" for grids large enough, but we don't know where the
crossover point is and there's [no
indication](http://web.archive.org/web/20131027174807/http://wso.williams.edu/~bchaffin/no_three_in_line/index.htm)
that the number of $2n$-point solutions is falling away from
exponential growth, at least up to $18 \times 18$!

To my eye, the configurations that work can be quite balanced and
attractive. Here are some symmetrical ones for $14 \times 14$ (though
in general the solutions are not necessarily symmetric in this way):

```lifeviewer
x = 54, y = 14, rule = LifeHistory
5BA3BA4B6.5BA3BA4B6.5BA3BA4B$3BA3BA6B6.3BABA8B6.6BA3BA3B$8BABA3B6.9B
2A3B6.5BA3BA4B$2BA9BAB6.2BA9BAB6.BA4BA7B$A5BA7B6.ABA11B6.ABA11B$2BA
10BA6.12B2A6.11BABA$BA7BA4B6.6B2A6B6.10BABAB$4BA7BAB6.6B2A6B6.BABA10B
$A10BA2B6.2A12B6.ABA11B$7BA5BA6.11BABA6.11BABA$BA9BA2B6.BA9BA2B6.7BA
4BAB$3BABA8B6.3B2A9B6.4BA3BA5B$6BA3BA3B6.8BABA3B6.3BA3BA6B$4BA3BA5B6.
4BA3BA5B6.4BA3BA5B!

#C [[ ZOOM 8 ]]
```

The most extensive searches for configurations so far have been done
by [Achim
Flammenkamp](http://wwwhomes.uni-bielefeld.de/achim/no3in/readme.html)
and later by [Ben Chaffin](https://benchaffin.com/). I've [written
some CUDA code](https://github.com/mvr/no-three-in-line)^[By the way,
the existing `cuda-mode` for Emacs is a bit messed up, I have a fork
with some bugfixes [here](https://github.com/mvr/cuda-mode).] of my
own with the goal of pushing things a little further, and I'll explain
it in the rest of this post.

<!--more-->

## General Strategy

We'll be doing a simple depth-first search over configurations,
storing a stack in device memory that the CUDA kernel interacts with.
Each configuration consists of two bitboards: one for cells known to
contain a point and another for cells known to be empty. On each
search step, we'll propagate as much information as we can in the
given configuration. If we don't reach a contradiction, we'll use a
heuristic to choose a row or column, and branch on all possibilities
for the locations of the points in that row.

To avoid repeating work, we'll also check that the configuration is in
a canonical orientation. Here, that means it's lexicographically
earliest out of all members in its symmetry orbit. If not, we just
discard the configuration. Doing this check accurately is slightly
less straightforward than it sounds, because of cells whose state is
still unknown.

As we did^[Or rather, as APG
[did](https://gitlab.com/apgoucher/lifelib/-/blob/master/cuda2/cudagol.h).]
when representing [Game of Life
configurations](/posts/cool-still-lifes.html) on the GPU, we're going
to be representing a $32 \times 32$ board by storing a single
`uint32_t` per thread, each representing a single row. Across the 32
threads in a warp (which we might imagine stacked vertically), these
values represent the whole board.^[In this problem we'll only be using
a $n \times n$ subset of the full $32 \times 32$ board available, but
trying to be more lane-efficient here would be hugely complex for
almost no gain.] There are a handful of warp-level primitives that
make manipulating such boards easy. We just have to make sure that
execution stays synchronised across the warp at all times or it will
hang.

For example, we can test whether a board is empty by checking whether
all rows are empty, using a ballot:

```cpp
bool empty = __ballot_sync(0xffffffff, state) == 0;
```

And we can find the first set bit in a board by doing a ballot to
find the first nonempty row, and then using a shuffle to broadcast the
position in that row to all the other threads.

```cpp
unsigned x = __ffs(state) - 1;

uint32_t mask = __ballot_sync(0xffffffff, state);
unsigned y = __ffs(mask) - 1;

x = __shfl_sync(0xffffffff, x, y);

return {x, y};
```


## Propagating Orthogonally

The simplest information propagation we'll be doing is setting forced
values in each row and column. This can be done in two complementary
situations:

* If the row/column contains $2$ known points already, the rest of the
  row must be empty.
* If the row/column contains $n-2$ known empty cells, the two
  remaining cells must contain points.

For rows, this is straightforward. Each thread can use the `__popc`
primitive to count the number of points in its row directly. Columns
are more expensive, because we need to count across threads.

A small binary counter is enough: we use two bits to count $0,1,2,3$,
and another bit to record overflow for any count higher than that. We
can then tree-reduce the binary counter across the warp, to do 5
rounds of additions rather than 32. Each "bit" here is actually
represented again as a `uint32_t`, considering that 32-bit value as a
bit-vector with one position for each column. We have to do two binary
counts: one to count the known points (and force accordingly), and one
to count the not-known-empty cells (and force accordingly).

Unfortunately, we can't force values based on lines of other slopes:
there's no requirement for these lines to contain exactly two points
each. In principle this kind of counting could still be useful though.
For example, each diagonal line can contain at most two points, so if
we tally up the number of available positions on each diagonal,
clamped at a maximum of 2 per line, we might be able to determine
early that a position is not completable even if each row and column
individually looks alright.^[There are $2n-1$ diagonal lines
in each direction compared with $n$ orthogonal lines, so a lot more
"room" to fit points in.] But after some experiments, it seems doing
this check is not a time improvement.


## Eliminating Lines

The above handles orthogonal lines, but we also have to update the
known-empty cells caused by lines of all other slopes. I couldn't come
up with an efficient way to do this simultaneously for all points, as
we did for the orthogonal lines^[But maybe there's a better way to do
it, please let me know!]. For each new point, we handle the lines
through all other points one by one.

So let's suppose two points $p = (p_0, p_1)$, $q = (q_0, q_1)$, and
let's say that $p$ is above $q$, so $p_1 < q_1$^[We've already handled
the case where two points are in the same row.]. The first step is to
bring $q$ as close as possible to $p$ on the same line, because we
don't want to accidentally miss the points *between* $p$ and $q$:

```lifeviewer
x = 34, y = 14, rule = LifeHistory
3BD10B6.3BD10B$14B6.14B$4BA9B6.4BA9B$14B6.14B$5BD8B6.5BA8B$14B6.14B$
6BD7B6.6BD7B$14B6.14B$7BA6B6.7BD6B$14B6.14B$8BD5B6.8BD5B$14B6.14B$9BD
4B6.9BD4B$14B6.14B!

#C [[ ZOOM 10 ]]
```

That is, we scale $\Delta = (q_0-p_0, q_1-p_1)$ down by $\gcd(q_0-p_0,
q_1-p_1)$. We can avoid doing this expensive calculation for each line
by precomputing a table of $x / \gcd(x, y)$ for all $0 \leq x,y < n$
and storing it in the device's constant memory, which is pretty quick
to read.

So far the same calculation has been done in all threads. Now, each
thread checks whether its row contains a point on the line^[Again, by
assumption the line is not horizontal, so at most one point in each
row is on the line.]. Not all rows will, because for some rows the
line falls between integer points (as in the example above). If the
row does contain a point, a little algebra gives us which bit needs to
be set. It doesn't seem possible to avoid integer division/modulus
here, unfortunately.

```cpp
unsigned p_quo = p.second / delta.second;
unsigned p_rem = p.second % delta.second;

unsigned row = threadIdx.x & 31;
if (row % delta.second == p_rem) {
  int col = p.first + ((int)(row / delta.second) - p_quo) * delta.first;
  if (col >= 0 && col < 32) 
    state |= 1 << col;
}
// We don't want to eliminate the original points...
if (p.second == row || q.second == row) {
  state = 0;
}
```

There are a couple of ways to speed this up. First, we can avoid the
integer divisions completely in the case that `delta.second` is a
power of two. Rather than writing out special cases by hand, we can
give the compiler some strong encouragement to do it for us:

```cpp
switch (delta.second) {
case 1:  return eliminate_line_inner(p, q, {delta.first, 1});
case 2:  return eliminate_line_inner(p, q, {delta.first, 2});
case 4:  return eliminate_line_inner(p, q, {delta.first, 4});
default: return eliminate_line_inner(p, q, delta);
}
```

Second, we can do some early filtering on the pairs of points that we
need to consider. If $\Delta$ is large enough then additional points
in either direction will be off the edge of the grid, so we can skip
processing that line entirely. This applies so long as there are no
points between, which is again determined by calculating the GCD of
the coordinates of $\Delta$.

```lifeviewer
x = 34, y = 22, rule = LifeHistory
2.D$22.D3$14B6.3BD10B$14B6.14B$14B6.14B$4BA9B6.4BA9B$14B6.14B$14B6.
14B$14B6.5BD8B$14B6.14B$14B6.14B$14B6.6BA7B$6BA7B6.14B$14B6.14B$14B6.
7BD6B$14B6.14B2$28.D2$8.D!

#C [[ ZOOM 8 ]]
```

For this, we precompute a mask giving all "relevant" endpoints for a
line with one point at $(0, 0)$. When processing a new point, we first
use this mask (appropriately shifted) to restrict to the second
endpoints we need to worry about.

In case you're curious, here's how that mask looks for $13 \times 13$:

```lifeviewer
x = 25, y = 25, rule = LifeHistory
AB3ABAB3A3B3ABAB3ABA$BA21BAB$ABABAB3ABA3BAB3ABABABA$A2BA2BA2BA5BA2BA
2BA2BA$ABABABABABA3BABABABABABA$5BA13BA5B$AB3AB6AB6AB3ABA$2BA3B6AB6A
3BA2B$ABABAB6AB6ABABABA$A2BA2B6AB6A2BA2BA$ABABAB6AB6ABABABA$6B6AB6A6B
$25B$6B6AB6A6B$ABABAB6AB6ABABABA$A2BA2B6AB6A2BA2BA$ABABAB6AB6ABABABA$
2BA3B6AB6A3BA2B$AB3AB6AB6AB3ABA$5BA13BA5B$ABABABABABA3BABABABABABA$A
2BA2BA2BA5BA2BA2BA2BA$ABABAB3ABA3BAB3ABABABA$BA21BAB$AB3ABAB3A3B3ABAB
3ABA!

#C [[ ZOOM 8 ]]
```

So only 304/625 of the time do we need to actually process a pair of
points.


## Branching and Vulnerable Cells

The remaining piece of the puzzle is how we choose what to branch on
once we've propagated all the information we can. One observation is
that for the typical cell, placing a point in that cell provides a lot
more information than assuming the cell is empty. If it contains a
point, the lines through that point are likely to rule out a handful
of other cells, whereas if empty, we've learned very little.

But for some unknown cells, learning that cell is empty causes a
*different* cell to contain a point, via the orthogonal forcing
detailed earlier. This happens when a row/column has zero points and
three unknowns, or when a row/column has one point already and two
unknowns. These cells I'm calling "vulnerable", and they're more
promising to branch on.

Here's the best strategy I've been able to come up with, just fiddling
with some different options:

* If there are any vulnerable cells, choose the one closest to the
  centre of the board and branch on whether it contains a point.
* Otherwise, find the row (not column!) with the fewest options for
  where its points can go and branch on each possibility.

I'm not sure why preferring to always branch along the same axis works
much better than, say, always taking the row or column with fewest
unknown cells, but empirically it's the case. This makes me think
there's a lot about the structure of this search problem that I don't
understand.

Again for the curious, here are some typical mid-search
configurations, where the red cells are ones which have been
definitively ruled out. Is there an obvious "tell" for these that
makes clear they're not completable? I don't see one.

```lifeviewer
x = 54, y = 14, rule = LifeHistory
11DADA6.11DADA6.11DADA$5DADA6D6.2DBD4BDB4D6.4DADA7D$B5DB5DAD6.2DBD4BD
2B3D6.4DA3DA5D$3BA2DB7D6.2DBDB2DB6D6.6DB4DBDB$3DA8DAD6.4D3B3DB3D6.D2A
11D$B3DBD2B6D6.2DBD3B3DB3D6.9DBD3B$2DB4D2B4DA6.A11DAD6.3DA2D2BD2BD2B$
A9DA3D6.A11DAD6.DA10DAD$8DADA3D6.6DADA5D6.A2DA10D$3B5DB5D6.DA6DA5D6.
7DBD5B$5DA5DA2D6.3DA7DA2D6.6DB2DBD3B$2B2DB3DB5D6.DA11DA6.5DADB2DBD2B$
6DA2DA4D6.3DA5DA4D6.A7DA5D$D2BDB2DBDA4D6.4DA2DA6D6.2DA2DA8D!

#C [[ ZOOM 8 ]]
```

On my [toy
machine](https://www.nvidia.com/en-us/autonomous-machines/embedded-systems/jetson-orin/nano-super-developer-kit/),
here's how long it takes to enumerate all configurations of a given
size.

| Size | Time (s) |
|------|----------|
| 12   | 13.5     |
| 13   | 65       |
| 14   | 1231     |
| 15   | 15672    |

Extrapolating outwards, redoing the [existing
calculation](http://web.archive.org/web/20131027174807/http://wso.williams.edu/~bchaffin/no_three_in_line/index.htm)
for $18 \times 18$ would take around 9 months. To push ahead to $19
\times 19$, I'm going to need better ideas or a better machine.
    
This is a good place to stop.

