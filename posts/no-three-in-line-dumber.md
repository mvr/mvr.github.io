---
title: No‑Three‑In‑Line, Dumber
published: February 3, 2026
tags: code, cuda
---

<script defer src="/life/lv-plugin.js"></script>

([Previously](/posts/no-three-in-line.html), [previously](/posts/no-three-in-line-quicker.html))

To not bury the lede, we finally have our first new value for the OEIS sequence [A000769](https://oeis.org/A000769):^[And its corollary [A000755](https://oeis.org/A000755).]

$$ a(19) = 32577 $$

This was calculated in 255 GPU-hours using eight RTX 4090s rented from
[Runpod](https://www.runpod.io/).

Not only that! With help from [Thomas Prellberg] and the [GPU cluster]
at Queen Mary University of London, we've been able to find some
solutions with record-breaking sizes. The largest at the time I'm
writing this is 64×64, shown below, with hopefully more on the way.

[Thomas Prellberg]: https://webspace.maths.qmul.ac.uk/t.prellberg/
[GPU cluster]: https://docs.hpc.qmul.ac.uk/

```lifeviewer
x = 64, y = 64, rule = LifeHistory
32BA16BA14B$34BA6BA22B$24BABA37B$16BA17BA29B$38BA13BA11B$7BA33BA22B$
12BA22BA28B$31BA26BA5B$35BA16BA11B$20BA26BA16B$21BABA40B$4BA3BA55B$
49BA7BA6B$42BA3BA17B$A11BA51B$33BA9BA20B$9BA50BA3B$13BA9BA40B$27BA5BA
30B$27BA11BA24B$15BA38BA9B$13BA39BA10B$BA3BA58B$46BA6BA10B$19BA41BA2B
$4BA32BA26B$25BA35BA2B$44B2A18B$6BABA55B$BABA60B$15BA2BA45B$A55BA7B$
7BA55BA$45BA2BA15B$60BABAB$55BABA6B$18B2A44B$2BA35BA25B$26BA32BA4B$2B
A41BA19B$10BA6BA46B$58BA3BAB$10BA39BA13B$9BA38BA15B$24BA11BA27B$30BA
5BA27B$40BA9BA13B$3BA50BA9B$20BA9BA33B$51BA11BA$17BA3BA42B$6BA7BA49B$
55BA3BA4B$40BABA21B$16BA26BA20B$11BA16BA35B$5BA26BA31B$28BA22BA12B$
22BA33BA7B$11BA13BA38B$29BA17BA16B$37BABA24B$22BA6BA34B$14BA16BA32B!

#C [[ HEIGHT 400 ]]
#C [[ ZOOM 5 ]]
```

<!--more-->

Getting these results involved some further improvements over what was
detailed in the previous posts, and I'll go through some of them in
the rest of this post.


## Getting Smarter

Let's return to the step that eliminates the remaining points on a
line that go through two given points. [APG] pointed out that there's
a way to do this simultaneously for all lines with a given slope,
similarly to orthogonal propagation handling all vertical or
horizontal lines simultaneously. For a slope $(x, y)$, the idea is to
count the number of points on those lines by doing a warp reduction
down to the first $y$ lanes and $x$ columns. Like a normal warp
reduction, this can be done in the shape of a binary tree, adding
lanes of distance $y, 2y, 4y, \dots$, until we've covered the full
32-lane warp. At each step, we shift the values by the appropriate
multiple of $x$ to match the slope. Here's an illustration for a 8×8
grid on lines of slope $(1, 2)$. Each arrow represents adding the
current tally from the source square into the tally for the target
square, and this can be done simultaneously for all squares using the
same sorts of shuffles and bit operations from the previous post.

```tikzpicture
\definecolor{terminalfill}{HTML}{D8E5F1}
\definecolor{smallarrow}{HTML}{214F66}
\definecolor{bigarrow}{HTML}{803300}
\def\offa{-0.08}
\def\offb{0.08}
\begin{scope}

\foreach \x in {0,...,7} {
  \foreach \row in {0,1} {
    \fill[terminalfill] (\x,7-\row) rectangle ++(1,1);
  }
}
\foreach \row in {0,...,7} {
  \fill[terminalfill] (0,7-\row) rectangle ++(1,1);
}

\foreach \x in {0,...,8} {
  \draw[gray!70] (\x,0) -- (\x,8);
}
\foreach \y in {0,...,8} {
  \draw[gray!70] (0,\y) -- (8,\y);
}
\draw[very thick] (0,0) rectangle (8,8);

\foreach \x in {1,...,7} {
  \foreach \row in {2,3,6,7} {
    \draw[-stealth, line width=0.6pt, color=smallarrow]
      (\x + 0.5 + \offa,7.5-\row) -- (\x - 0.5 + \offa,9.5-\row);
  }
}
\foreach \x in {1,...,7} {
  \foreach \row in {4,5} {
    \draw[-stealth, line width=0.6pt, color=smallarrow]
      (\x + 0.5 + \offb,7.5-\row) -- (\x - 0.5 + \offb,9.5-\row);
  }
}
\end{scope}

\begin{scope}[xshift=10cm]

\foreach \x in {0,...,7} {
  \foreach \row in {0,1} {
    \fill[terminalfill] (\x,7-\row) rectangle ++(1,1);
  }
}
\foreach \row in {0,...,7} {
  \fill[terminalfill] (0,7-\row) rectangle ++(1,1);
}

\foreach \x in {0,...,8} {
  \draw[gray!70] (\x,0) -- (\x,8);
}
\foreach \y in {0,...,8} {
  \draw[gray!70] (0,\y) -- (8,\y);
}
\draw[very thick] (0,0) rectangle (8,8);

\foreach \x in {2,...,7} {
  \foreach \row in {4,5} {
    \draw[-stealth, line width=1.1pt, color=bigarrow]
      (\x + 0.5 + \offa,7.5-\row) -- (\x - 1.5 + \offa,11.5-\row);
  }
}
\foreach \x in {2,...,7} {
  \foreach \row in {6,7} {
    \draw[-stealth, line width=1.1pt, color=bigarrow]
      (\x + 0.5 + \offb,7.5-\row) -- (\x - 1.5 + \offb,11.5-\row);
  }
}
\end{scope}
```

By the end of all this, the highlighted squares contain the full tally
for the line that contains them. We then reverse the process and
broadcast that value back down to the other squares in the line.

[APG]: https://cp4space.hatsya.com/

The remaining thing to do is determine which slopes need processing in
this way. For this, we look at all points that are new since we last
did this oblique line propagation step. For each one, we shift the
board so that point is at the origin; the positions of all remaining
points represent slopes we need to check for that point. After
accumulating a mask of all such slopes, we pull all points in that
mask towards the origin by dividing by the GCD of their coordinates.
The resulting mask gives exactly the slopes that we need to apply the
above warp-reduction procedure to.

After all this, it ends up slower than just doing each pair of points
individually as in the previous post! There must not be enough pairs
of points with identical slopes for it to be worth it, and the
bookkeeping to collect those slopes also takes some time. So that idea
was a bust, sadly, though I don't think we could have known before
trying it.


## Getting Dumber

So much for trying to be smarter. Here's a much dumber way to
eliminate the line through two points: precompute a giant table with a
line mask for every possible pair of points. This table occupies $n^2
\times n^2 \times 32 \times 32$ bits, which for $n = 19$ comes out to
16.7 megabytes, perfectly reasonable to store in global memory.

Then, when eliminating the line through a pair, we just look up the
mask for that pair, add it to the current known-off set, and we're
done. 

Even here there are some smarter things to try, like a tiny pipeline
that tries to load the next mask from memory while still processing
the previous one. No good! The dumber thing is better. The only little
optimisation that helped was using the `__ldg` intrinsic to load from
global memory, to indicate that the data being loaded is "read-only"
for the lifetime of the kernel.

Using a lookup table ended up being significantly faster than any
clever tricks for computing the lines I've been able to come up with.
Not only that, but it also makes obsolete the "one-hop" optimisation I
described in the previous post; it's no longer worth doing over just
eliminating the lines this braindead way. A little disappointing, but
being able to reach new results smooths over any psychic pain caused.


## Getting Semivulnerable

In the first post I mentioned that preferring to branch on
"vulnerable" cells helps a lot. These were the cells in a row or
column that when assumed to be off force at least one other cell to be
on. So, in that row/column, we have either zero points and three
unknowns, or one point and two unknowns. The point is that branching
on a vulnerable cell makes progress regardless of which option we
take, whereas setting a typical cell to off usually doesn't cause any
other cells to have their values forced.

There's another situation that's nearly as good: zero points and
*four* unknowns; we might call those unknowns "semivulnerable". An
additional off cell in that row/column doesn't immediately force
another cell to be on, but it does lead to the row/column being
vulnerable, and next off branch will set *two* cells to be on. In this
way we make up for the first "wasted" move.

Looking for semivulnerable cells after not finding vulnerable ones
gives a nice speedup. Experimenting further than that and hunting for
zero points and five unknowns is over the hill of diminishing returns.


## Getting Symmetric

The record breaking solution at the top of this post has obvious
rotational symmetry, and that's no coincidence. Hunting for such
symmetric solutions has advantages: we only have to store one
quarter of the grid, and each additional point introduces a large
number of new constraints involving its images under the symmetry.

For the fundamental region of a $2n \times 2n$ board, I'm taking the square $[0, n) \times [0, n)$, stored across the warp like before.

```lifeviewer
x = 6, y = 6, rule = LifeHistory
6B$6B$6B$3B3D$3B3D$3B3D!
```

Working symmetrically ties in nicely with the dumb lookup table we just introduced. Each pair of points we add is now involved in 16 different lines:

```lifeviewer
x = 36, y = 36, rule = LifeHistory
BA4B4.BA4B4.BA4B4.BA4B$2BA3B4.6B4.6B4.BD4B$3BD2B4.4BAB4.2BD3B4.BD4B$
4BDB4.6B4.6B4.BA4B$5BD4.6B4.3BA2B4.BD4B$6B4.6B4.6B4.BD4B5$6B4.6B4.6B
4.6B$2DA2DA4.5BA4.5BA4.5BA$6B4.4BAB4.6B4.3BD2B$6B4.3BD2B4.6B4.BA4B$6B
4.2BD3B4.3BA2B4.6B$6B4.BD4B4.6B4.6B5$6B4.4BDB4.6B4.6B$2BA3B4.4BDB4.D
5B4.6B$6B4.4BAB4.BD4B4.6B$3BD2B4.4BDB4.2BD3B4.BA4B$6B4.4BDB4.3BA2B4.
6B$4BAB4.4BAB4.4BAB4.4BAB5$6B4.6B4.6B4.4BDB$2BA3B4.6B4.6B4.3BD2B$6B4.
4BAB4.6B4.2BD3B$6B4.2BD3B4.6B4.BA4B$A5B4.A5B4.A2DA2D4.A5B$6B4.6B4.6B
4.6B!

#C [[ ZOOM 6 ]]
```

Rather than doing 16 times as much computation per pair, we just include the contributions for all these lines in the lookup table for the two points in the fundamental region.

```lifeviewer
x = 6, y = 6, rule = LifeHistory
6B$6B$6B$3B2DB$3BA2D$4BAB!
```

The piece of code that truly needs to be customised for the
symmetrical case is orthogonal propagation. Specifically, we'd like to
do the counting along rows and columns using the fundamental domain
directly, without materialising the full $2n \times 2n$ grid. This is
easy enough: for each row of the fundamental domain, the missing part
of the row in the full grid is exactly one of the columns of the
fundamental domain, if you imagine rotating the grid anticlockwise
once:

```lifeviewer
x = 6, y = 6, rule = LifeHistory
6B$6B$6B$4BDB$3D3A$4BDB!
```

The only new code necessary is a converter from row counts (as
per-lane integers given by popcount) to a binary counter as described
in the previous post, and this is easily achieved using a couple of
ballots. Combining those counters gives the count for the row of the
full grid, and by symmetry also one of the columns of the full grid,
and we can use the same forcing logic as in the non-symmetric case.

Enumerating these symmetric solutions is much, much faster than for
the non-symmetric ones, even taking into account the doubling of the
grid size that you get for free. We've been able to enumerate all
rotationally symmetric grids up to 52×52, contrasted with the
non-symmetric grids up to 19×19.

Strategies like this of course work for other symmetries of the grid,
just with trickier storage layouts and correspondingly trickier
solutions for the orthogonal counting problem. In particular,
Flammenkamp discovered that hunting for large solutions with *odd*
grid size is best done with "near" symmetry: fourfold rotational
symmetry except for the two main diagonals, which contain two points
rather than the expected four.

With a little more thought, one can devise efficient storage formats
and counting methods for this layout, as well as the various other
possible symmetry groups of configurations on the grid. Details are
perhaps better left to a separate post.


## Summary

All together, we have another large jump in performance over the
previous post. And this is on top of code that I already thought was
pretty good---shows what I know!^[Remember, these timings are on the
tiny machine I have at home, not a beefy rented GPU. A good machine
with a few GPUs is another 250× speedup "for free".]

| Size | Post 1 (s) |   | Post 2 |   | Now   |
|------|------------|---|--------|---|-------|
| 12   | 13.5       | → | 8.2    | → | 5.6   |
| 13   | 65         | → | 39     | → | 18.5  |
| 14   | 1231       | → | 771    | → | 277   |
| 15   | 15672      | → | 7801   | → | 2645  |
| 16   | ?          | → | ?      | → | 26805 |

The nice round factors of 10 in the last three rows is what made me
optimistic to try 19×19. Unluckily, the jump from 18 to 19 was closer
to 15×.
