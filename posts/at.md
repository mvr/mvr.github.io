---
title: Effective Algebraic Topology in Haskell
published: September 22, 2022
tags: maths, code, retroactive
draft: true
---

Code available on [GitHub](https://github.com/mvr/at).

For a little while I have been working on this port of [Kenzo] from
Common Lisp to Haskell. Kenzo is a collection of algorithms for
explicit constructions on simplicial sets: the `README` in the
repository gives a list of what's implemented and what should be
possible. The algorithms and implementations in Kenzo were created by
Francis Sergeraert, Julio Rubio Garcia, Xavier Dousson, Ana Romero and
many collaborators.

[Kenzo]: https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/

My goal was to implement just enough to compute $\pi_4 S^3$,
and I reached it today. 

```
...
> let x = totalSpace s3 (Wbar kz1) fibration
> putStrLn $ "π₄ S³ is: " ++ show (homology x !! 4)
π₄ S³ is: ℤ/2
```

A fun capability it picked up along the way is calculating the
homology of Eilenberg-MacLane spaces $K(ℤ/m,n)$, for example, here
$K(ℤ/3,2)$.^[This makes my laptop get hot pretty quickly.]

```
> homology (Wbar (WbarDiscrete (Zmod 3)))
[ℤ,0,ℤ/3,0,ℤ/3,0,ℤ/(3^2),ℤ/3,ℤ/3,ℤ/3,ℤ/3 ⊕ ℤ/3,^C
```

I have a lot of ideas for improvements and additional features if
anybody is interested in joining in, so ==please email me if that's
you or you know a student for whom this would make a good project==.
For example, adding the construction of loop spaces and their homology
would be nice, as would homotopy pushouts in general.

On the implementation side, I have not spent any time optimising the
code and there are some obvious places to start, like using a proper
integer matrix library instead of a homemade one. These sorts of
optimisations would also make a good project for an undergraduate,
with not much mathematical background required.

Below, I've copy-pasted the quick introduction to the ideas behind
Kenzo that I give in the `README`.

## Central Concepts of Kenzo

A *simplicial set* $X$ is described in code by some abstract type `a`,
containing whatever data is required to specify $X$, and a type
`GeomSimplex a`, whose elements correspond to non-degenerate
simplices; in Kenzo these are referred to as 'geometric simplices'.
Like Kenzo we also allow a predicate on `GeomSimplex a` specifying
when an element actually describes a geometric simplex and when it is
spurious.

An actual simplex of $X$ is a geometric simplex together with a
'formal degeneracy operator', which is a list of degeneracy operators
in a normal form. Face maps are implemented as functions from
geometric simplices to (possibly degenerate) simplices, and the
extension of these face maps to all simplices is forced by the
simplicial identities.

A simplicial set is *of finite type* if there is a finite number of
geometric simplices for each dimension, and there is a function giving
a list of these simplices for any dimension $n$. It is not required
that there are finitely many geometric simplices overall.

The *normalised chain complex* $N(X)$ of $X$ has each $N(X)_n$ given
by the free abelian group on the set of nondegenerate $n$-simplices of
$X$, with the boundary of a simplex calculated similar to usual (the
alternating sum of face maps), but ignoring any degenerate faces.

If $C(X)$ is the ordinary chain complex of simplicial chains of $X$,
the quotient map $C(X) \to N(X)$ is a quasi-isomorphism, and so if $X$
is of finite type, then the homology of $X$ can be computed via
$N(X)$.

But many unavoidable simplicial sets (like $K(ℤ,n)$ and loop spaces
$ΩX$) are not of finite type, and so we need some other way to
calculate their homology. This is where 'effective homology' comes in.

A *reduction* between chain complexes $C$ and $D$ is a strong
deformation retract of chain complexes. The data of a reduction
unwinds to a triple ($f : C \to D$, $g : D \to C$, $h : C \to C$) where
$f$ and $g$ are degree 0, the homotopy operator $h$ is degree 1, and
certain equations involving these hold.  A *(strong chain)
equivalence* between two chain complexes $C$ and $D$ is a span of
reductions $l : E \to C$ and $r : E \to D$.

An *effective homology structure* on $C$ is an equivalence between $C$
and a chain complex $F$ of finite type.

A *simplicial set with effective homology* is a simplicial set $X$
equipped with an effective homology structure on $N(X)$.

The point of Kenzo is that although constructions on simplicial sets
sometimes do not preserve levelwise finiteness, they *do* extend to
effective homology structures. And so if we begin with a finite
simplicial complex and perform some constructions using it, then we
can often compute the homology of the result even if the actual
simplicial sets are now far too complicated to get a handle on.

