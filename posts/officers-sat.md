---
title: Constraining the Period of Officers
published: March 23, 2026
tags: code, sat, officers
---

The values of the impartial game [Officers] are probably periodic
eventually. Can we say anything about what the period could be?^[I'll
be assuming you're up to speed on the previous post.]

[Officers]: /posts/officers.html

<!--more--> 

If we assume there are no further rare values, we can put a
(ludicrous) bound on what it's possible for the eventual period to be.
Let's say that there are $R$ rare values, and the last rare value
occurs at position $k$. After this point, any additional value depends
only on the previous $k+1$ values, adding one because the rules of
Officers require us to remove a coin from the heap, so looking one
further back into previous values. When calculating the $\mathrm{mex}$
in order to get the next value, we can only rule out $R$ common values
each time, so there are only $R+1$ possible values that can occur in
the periodic segment.

All together, we end up with a strange kind of shift-register with
only $(R+1)^{k+1}$ possible states. The actual (conjectured) values
for Officers here are $R = 1584$ and $k = 20627$, so our upper bound
on the period is: $$1585^{20628} \approx 1.6\times 10^{66010}$$ Not
very useful in practice, unfortunately.

On the flip side, there's an easy way to rule out some small periods.
Positions with value 0 are rare (they're the losing positions), and
have a special role. For such a position $G(l)=0$, when computing the
value of a new position $n$ we end up including the value
$G(l)⊕G(n-l-1) = 0⊕G(n-l-1) = G(n-l-1)$ in the $\mathrm{mex}$, and so
$G(n-l-1)$ and $G(n)$ cannot be the same value. This excludes $l+1$ as
a possible period, indeed, any divisor of $l+1$.

So almost for free, this excludes the periods $$1, 2, 3, 5, 7, 9, 11,
13, 15, 21, 25, \dots, 314, 409$$ by looking at all the known losing
positions.


## SAT

Can we say anything else? Is the problem constraining enough that other eventual periods can be ruled out?

The first unknown period is 4, and to search for a solution I decided
to give SAT solving a go. To keep things even simpler, I make another
unjustified assumption; that the values permanently stay below 512.
This holds, at least for the first 14 trillion values, as checked by
Grossman.

The encoding I used is fairly simple: we have $p$ values with 256
possibilities^[Remember that all values are common, so one bit is
knocked out.] to find, with each represented by a one-hot encoding.
Calculating the $\mathrm{mex}$ is done via another 256 variables,
tracking whether each possible value is hit by a rare value/common
value combination. The rare values and their positions are read from a
file, and the XOR calculation for each one is hardcoded directly into
the SAT problem.

And, disappointingly, we quickly find a solution: $$[121, 156, 126,
155]$$ I continued this calculation out to $p = 26$, and for every
case that's not excluded by the presence of a losing position, there's
a feasible pattern of values with that period:

\begin{aligned}
p = 1 &\quad \text{None} \\
p = 2 &\quad \text{None} \\
p = 3 &\quad \text{None} \\
p = 4 &\quad [121, 156, 126, 155] \\
p = 5 &\quad \text{None} \\
p = 6 &\quad [43, 230, 262, 64, 141, 323] \\
p = 7 &\quad \text{None} \\
p = 8 &\quad [32, 75, 362, 134, 65, 42, 296, 134] \\
p = 9 &\quad \text{None} \\
p = 10 &\quad [21, 184, 116, 217, 307, 86, 251, 55, 154, 285]  \\
p = 11 &\quad \text{None} \\
p = 12 &\quad [18, 292, 272, 180, 101, 138, 91, 363, 351, 253, 44, 195]  \\
p = 13 &\quad \text{None} \\
p = 14 &\quad [30, 272, 33, 495, 30, 128, 207, 272, 292, 60, 504, 455, 207, 128]  \\
p = 15 &\quad \text{None} \\
p = 16 &\quad [2, 185, 59, 128, 2, 190, 5, 135, 262, 128, 2, 283, 5, 135, 60, 128]  \\
p = 17 &\quad [19, 55, 236, 169, 114, 423, 157, 216, 269, 328, 335, 266, 90, 426, 24, 485, 488]  \\
p = 18 &\quad [65, 117, 251, 393, 252, 114, 70, 117, 251, 273, 252, 374, 313, 444, 251, 273, 252, 114]  \\
p = 19 &\quad [21, 60, 426, 324, 121, 155, 178, 345, 423, 247, 222, 283, 156, 59, 459, 217, 126, 87, 262]  \\
p = 20 &\quad [44, 211, 422, 323, 49, 290, 127, 211, 402, 157, 98, 211, 445, 344, 127, 313, 49, 211, 393, 157]  \\
p = 21 &\quad \text{None} \\
p = 22 &\quad [3, 195, 328, 325, 100, 302, 98, 322, 135, 195, 129, 386, 352, 38, 230, 32, 224, 162, 267, 71, 5, 65]  \\
p = 23 &\quad [65, 324, 179, 303, 76, 138, 344, 324, 185, 150, 110, 65, 180, 70, 90, 156, 363, 90, 156, 128, 120, 387, 257]  \\
p = 24 &\quad [3, 64, 256, 195, 175, 9, 352, 196, 101, 71, 358, 269, 135, 38, 4, 165, 201, 104, 206, 162, 256, 33, 262, 290] \\
p = 25 &\quad \text{None} \\
p = 26 &\quad [5, 71, 179, 241, 141, 246, 138, 200, 319, 392, 99, 324, 100, 24, 90, 378, 236, 323, 257, 151, 313, 378, 399, 126, 392, 121]
\end{aligned}

These were all found relatively quickly, in under a minute each. The
first unclear case is $p = 27$, which I ran overnight without result;
it would be interesting to know whether it's UNSAT, though again not
especially useful!
