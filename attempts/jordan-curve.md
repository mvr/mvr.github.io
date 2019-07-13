---
title: The Jordan Curve Theorem
published: December 14, 2014
tags: maths, algebraic-topology, easy
---

Pick up a pen and draw any squiggly loop so that you end where you start and don't cross your own path. The most obvious thing you could say about your picture is that the page has been split into two disjoint regions; the part inside your curve and the part outside. This assertion is known as the _Jordan curve theorem_. Not only is it something that needs to be proven, but a truly correct formal proof was only given 100 years ago!

This theorem is brought up every now and then in undergraduate courses to remind students that obvious statements are not always obviously true. Unfortunately, they never ended up giving us a proof. In the hope of finding some closure I describe a short modern proof here.

<div class="theorem"> **Theorem**

Let $C$ be the image of an embedding of $S^1$ into $\mathbb{R}^2$. Then $\mathbb{R}^2 \setminus C$ has exactly two components.

</div>
<div class="proof">

It suffices to prove the statement when we instead carve a circle out of $S^2$, as deleting a point from an open set of $S^2$ does not change its connectedness.

Consider the two subsets $A$ and $B$ of $S^2$ given by the closed upper and lower hemispheres of $C$, so that $A \cup B = C$ and $A \cap B \cong S^0$. We now look at the reduced Mayer-Vietoris sequence for $(S^2 \setminus A, S^2 \setminus B)$. The interesting part is:
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

An inductive argument can be used to prove that any $S^{n-1}$ shaped subset of $S^n$ splits $S^n$ into two components.

In the 2-dimensional case, the inside and outside of the curve are homeomorphic to the inside and outside of an ordinary circle drawn in the plane, a fact proven in the _Jordan-Schönflies theorem_. An analogous statement is not always true in higher dimensions, as shown by the famous 'Alexander horned sphere'. The horned sphere is a peculiar embedding of $S^2$ into $\mathbb{R}^3$ such that the exterior of the sphere is not simply connected. This embedding is much better shown than explained:

<iframe width="560" height="315" src="//www.youtube.com/embed/Pe2mnrLUYFU" frameborder="0" allowfullscreen></iframe>
