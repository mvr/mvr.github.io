---
title: Projects
---

## Introduction to Cubical Agda

[David](https://www.davidjaz.com/) and I ran a summer course on
Cubical Agda for the few years we were at NYUAD. We put together some
lecture notes, which are [now available
here](https://cqts.github.io/introduction-to-cubical/).

I like to think of them as a much-expanded and smoothed out version of
[The HoTT Game](https://thehottgameguide.readthedocs.io/) from a few
years ago, where as much as possible is left as exercises.


## `at` -- Effective Algebraic Topology

A [Haskell library](https://github.com/mvr/at) for explicit constructions on simplicial sets. This is a port of [Kenzo](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/) from Common Lisp. More details in [this post](posts/at.html).

```
...
> let x = totalSpace s3 (Wbar kz1) fibration
> putStrLn $ "π₄ S³ is: " ++ show (homology x !! 4)
π₄ S³ is: ℤ/2
```


## Conway's Game of Life Tools

I've written a handful of C++ search tools, most of which use a vastly
expanded fork of the [LifeAPI](https://github.com/mvr/LifeAPI) library
by Michael Simkin. The first two tools in this list were central in
resolving the [Omniperiodicity](https://arxiv.org/abs/2312.02799) of
the Game of Life.

* __[Symmetric CatForce](https://github.com/mvr/CatForce/tree/depth-first)__: A
  catalyst-placing search program. Similar to
  [ptbsearch](https://github.com/ceebo/ptbsearch). Has been used to
  find a [huge](https://conwaylife.com/wiki/P84_honey_farm_hassler)
  [number](https://conwaylife.com/wiki/84P199)
  [of](https://conwaylife.com/wiki/108P59)
  [interesting](https://conwaylife.com/wiki/74P34)
  [oscillators](https://conwaylife.com/wiki/Cribbage).

  Originally based on
  [CatForce](https://github.com/simsim314/CatForce), also originally
  by Micahel Simkin, but very little original code remains.

* __[Barrister](https://github.com/mvr/barrister)__: A cell-by-cell
  searcher for catalysts. Similar to
  [Bellman](https://conwaylife.com/wiki/Bellman) by Mike Playle, but
  uses a more careful analysis of the neighbourhoods available to each
  cell. This, together with being more vectorised, makes it much
  faster. Barrister was used to find the catalyst appearing in the
  [first period-19 oscillator](https://conwaylife.com/wiki/Cribbage),
  and many oscillators and conduits since.
  
  Adam P. Goucher has written a searcher in CUDA based on Barrister
  called [Silk](https://gitlab.com/apgoucher/silk). Silk is much much
  faster still (but by nature somewhat less customisable).

* __[Lightcone](https://github.com/mvr/lightcone)__: A
  catalyst-placing search program that tries much harder to take into
  account "lightspeed considerations" between how different parts of
  the pattern can interact. Details in [this
  post](posts/lightcone.html). Has been used to find a handful of new
  elementary conduits especially "spartan" ones.

* __[Stomp](https://github.com/mvr/LifeAPI/tree/master/tools)__: A
  port of the
  [`transfer.py`](https://conwaylife.com/wiki/Shinjuku#transfer.py)
  script to LifeAPI, for searching for new glider syntheses. Used to
  pick the low-hanging fruit in the project of synthesising all
  [23-bit still
  lifes](https://conwaylife.com/forums/viewtopic.php?f=2&t=7031),
  though human ingenuity (of other people, not me) was still necessary
  for the last couple of hundred.


## `cf` -- Continued Fractions

A [Haskell library](https://github.com/mvr/cf) for Gosper's algorithms
on continued fractions with possibly non-positive terms. Some tricks
from Vuillemin and Lester are used to compute transcendental functions
(e.g. `exp`, `sin` and `cos`) on rational numbers.

The novel idea is that these algorithms apply to continued fractions
whose terms _are also continued fractions_, allowing us to also apply
these transcendental functions to general continued fractions. A
function `cfcf` flattens one of these into an ordinary continued
fraction.

A nice advantage the continued fraction representation is that the
precision of a result does not need to be specified in advance. If
more precision is required, one can just compute more terms in the
continued fraction, without throwing away the work already
done. The downside is that it is very slow!

* Gosper, Ralph W. "Continued fraction arithmetic." HAKMEM Item 101B, MIT Artificial Intelligence Memo 239 (1972).
* Vuillemin, Jean E. "Exact real computer arithmetic with continued fractions." Computers, IEEE Transactions on 39.8 (1990): 1087-1105.
* Lester, David R. "Vuillemin’s exact real arithmetic." Functional Programming, Glasgow 1991. Springer London, 1992. 225-238.


## `cgt` -- Combinatorial Game Theory
A [Haskell library](https://github.com/mvr/cgt) for some combinatorial
game theory, based on algorithms from the [Combinatorial Game
Suite](http://cgsuite.sourceforge.net/) by Aaron Siegel. More details
in [this post](/posts/cgt.html), which describes the basic ideas.


## epeen

A [dumb joke](/epeen/index.html) that translates your Dota 2 MMR (as
of 2016) into to a certain anatomical measurement. Written in Elm to
try the language out, didn't come away a fan.
