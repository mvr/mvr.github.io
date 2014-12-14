---
title: The Jordan curve theorem
published: December 14, 2014
tags: maths, algebraic-topology, easy
---

Pick up a pen and draw some kind of squiggly loop so that you end where you start and don't cross your own path. The most obvious thing you could say about your picture is that the page has been split into two disjoint regions; the part inside your curve and the part outside. This assertion is known as the _Jordan curve theorem_. Not only is it something that needs to be proven, but a formal proof was only given 100 years ago!

This theorem is brought up every now and then in undergraduate courses to remind us that obvious statements are not always trivial. Unfortunately, they never actually gave us a proof. With the degree behind me, I thought I would find some closure and describe a modern proof that uses the machinery of algebraic topology.

<div class="theorem"> **Theorem**

Let $C$ be the image of an embedding of $S^1$ into $\mathbb{R}^2$. Then $\mathbb{R}^2 \setminus S$ has exactly two components.

</div>
<div class="proof">

It suffices to prove the statement when we instead carve a circle out of $S^2$, as deleting a point from an open set of $S^2$ does not change its connectedness.

Consider the two subsets $A$ and $B$ of $S^2$ given by the closed upper and lower hemispheres of $C$, so that $A \cup B = C$ and $A \cap B \cong S^0$. Looking at the reduced Mayer-Vietoris sequence for $(S^2 \setminus A, S^2 \setminus B)$, the interesting part is:
$$
\cdots \to \tilde{H}_1(S^2 \setminus A) \oplus \tilde{H}_1(S^2 \setminus B) \to \tilde{H}_1(S^2 \setminus S^0) \to \tilde{H}_0(S^2 \setminus C)
\to \tilde{H}_0(S^2 \setminus A) \oplus \tilde{H}_0(S^2 \setminus B) \to \cdots
$$
So
$$
\cdots \to 0 \to \mathbb{Z} \to \tilde{H}_0(S^2 \setminus C) \to 0 \to \cdots
$$
is exact and therefore $\tilde{H}_0(S^2 \setminus C) \cong \mathbb{Z}$, meaning $S^2 \setminus C$ has exactly 2 components.
</div>

An induction argument can be used to prove that any $S^{n-1}$ shaped subset of $S^n$ splits $S^n$ into two components.

You might also note that in the 2-dimensional case, the inside and outside of the curve are homeomorphic to the inside and outside of an ordinary circle drawn in the plane. An analogous statement is not always true in higher dimensions, as shown by the famous 'Alexander horned sphere'.

TODO
