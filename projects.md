<section class="post">
<a href="/">&lt; Home</a>
<h1 class="post-title">Projects</h1>
<div class="post-content">

cf -- Continued Fractions
=========================

A <a href="https://github.com/mvr/cf">Haskell library</a> implementing
Gosper's algorithms on continued fractions with possibly non-positive
terms.  Some tricks from Vuillemin and Lester are used, to allow
computing transcendental functions such as `sin` and `cos` of rational
numbers.

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

cgt -- Combinatorial Game Theory
================================

A [Haskell library](https://github.com/mvr/cgt) for some combinatorial
game theory, based on algorithms from the [Combinatorial Game
Suite](http://cgsuite.sourceforge.net/) by Aaron Siegel.

I have a [literate Haskell post](/posts/cgt.html) describing the basic
ideas.


epeen
=====

A <a href="/epeen/index.html">dumb joke</a> that translates your Dota
2 MMR (as of 2016) into to a certain anatomical measurement. Written
in Elm to try the language out, didn't come away a huge fan.

</div>
</section>
