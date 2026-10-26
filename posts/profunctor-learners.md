---
title: Profunctor Learners
published: October 26, 2026
tags: maths, types
---

[Optics](https://arxiv.org/abs/1809.00738) have a very nice
formulation used in some
[Haskell](https://hackage.haskell.org/package/optics) (and
[PureScript](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/8.0.0)!)
libraries; the so-called "profunctor representation". Learners,
defined a [certain way](https://doi.org/10.4204/eptcs.429.3), look an
awful lot like some kind of two-sided or symmetric optic. Let's see if
we can devise a profunctor representation for them. This post was
prompted by a chat with [Bartosz Milewski](https://bartoszmilewski.com/).
<!--more-->

To recap, the category of
[learners](https://doi.org/10.1109/LICS.2019.8785665) in the sense of
Fong, Spivak and Tuyéras is intended to capture the compositional
structure of supervised learning. In [my
talk](https://doi.org/10.4204/eptcs.429.3) at
[ACT2024](https://oxford24.github.io/) I showed that, if we tweak the
definition slightly, then learners can be presented in a nice
symmetrical form. Specifically, we'll make two changes to the original
definition:

1. Learners will be considered equivalent up to *any* compatible
   function on their parameter sets, rather than only up to
   isomorphism; and,
2. The covariant and contravariant occurrences of each variable will
   be permitted to be different objects.

The full source is [here](/src/ProfunctorLearners.hs), if you'd like it. First some throat clearing.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module ProfunctorLearners where

import Control.Arrow (Kleisli(..), second)
import Control.Monad.Fix (MonadFix, mfix)
import Data.Profunctor (Profunctor(..), lmap, rmap)
```

The "Topos Institute" form of a learner can be implemented verbatim in
Haskell; it's just a tuple of three functions with a particular shape.^[Forgive the joke names, it's a bit of an optics tradition... Or email me better ones!]

```haskell
data Institute b b' a a' =
  forall m. Institute
  { implement :: m -> a -> b,
    update    :: m -> a -> b' -> m,
    request   :: m -> a -> b' -> a' }
```

Through a little coend calculus, this is "behaviourally" equivalent to the following
symmetric form:

```haskell
data School b b' a a' =
  forall m n. School
  { left :: (m, a) -> (n, b),
    right :: (n, b') -> (m, a') }
```

In words:

* `a` and `b` are the forward input and output,
* `b'` and `a'` are the backward input and output,
* `m` is the set of parameters,
* `n` is the information remembered between forwards and backward
  passes.

There's a natural way to depict these, which is what I use in my note: 

```tikzpicture
\tikzset{learner map/.style={draw, circle, inner sep=0pt, minimum size=7mm}}
\node[learner map] (l) at (0,-0.9) {$l$};
\node[learner map] (r) at (0, 0.9) {$r$};

\draw[-stealth] (-2.3,-0.9) -- node[below] {$a$} (l.west);
\draw[-stealth] (l.east) -- node[below] {$b$} (2.3,-0.9);
\draw[-stealth] (2.3,0.9) -- node[above] {$b'$} (r.east);
\draw[-stealth] (r.west) -- node[above] {$a'$} (-2.3,0.9);
\draw[-stealth] (r.220) to[out=220,in=140,looseness=1.2]
  node[left] {$m$} (l.140);
\draw[-stealth] (l.40) to[out=40,in=320,looseness=1.2]
  node[right] {$n$} (r.320);
```

It's easy to convert between these two forms: the trick in the forward
direction is to choose the type `(m, a)` for the value of `n` in the
existential, so we can thread the `m` and `a` arguments through to the
update and request functions.

```haskell
instituteToSchool :: Institute b b' a a' -> School b b' a a'
instituteToSchool (Institute i u r) = School left right
  where left (m, a) = ((m, a), i m a)
        right ((m , a), b') = (u m a b', r m a b')

schoolToInstitute :: School b b' a a' -> Institute b b' a a'
schoolToInstitute (School left right) = Institute i u r
  where i m a = snd (left (m, a))
        u m a b' = fst (right (fst (left (m, a)), b'))
        r m a b' = snd (right (fst (left (m, a)), b'))
```

The main point of my ACT talk was that this symmetric form makes it
very easy to see that learners have additional structure that's
obscured in the non-symmetric form. Specifically, there's an
involution on the category:

```haskell
flipClassroom :: School b b' a a' -> School a' a b' b
flipClassroom (School l r) = School r l
```

and every object is equipped with a cup and cap learner:

```haskell
schoolCup :: School () () a a
schoolCup = School (\((), a) -> (a, ())) (\(a, ()) -> ((), a))

schoolCap :: School b b () ()
schoolCap = flipClassroom schoolCup
```

The analogy in categories of optics (not just lenses!) is that every
object has a cup morphism, but not necessarily a cap
morphism.^[Depending on your conventions, you might have a cap but not
a cup.]


## Profunctor Encoding

This cup morphism is closely related to the profunctor encoding of
optics. What follows might be the most compressed summary of
profunctor optics around; it will only be useful for people who
already have a handle on how the story goes. For a proper exposition
you should have a look at [Profunctor Optics, a Categorical
Update](https://doi.org/10.32408/compositionality-6-1) by Clarke et
al.

We start with the concrete data of an optic that we're trying to
capture:

```haskell
data Appraiser b b' a a' =
  forall n. Appraiser
  { oleft  :: a -> (n, b),
    oright :: (n, b') -> a' }
```

This is like the `School` data-type above but with the incoming state
`m` removed.
For fixed `b` and `b'` this is clearly a profunctor in the remaining
two variables, by composing on either side as appropriate.

```haskell
instance Profunctor (Appraiser b b') where
  dimap f g (Appraiser l r) = Appraiser (lmap f l) (rmap g r)
```

We have the promised cup morphism by stashing the provided `a` into
the `n` component.

```haskell
opticCup :: Appraiser () () a a
opticCup = Appraiser (\a -> (a, ())) (\(a, ()) -> a)
```

Generalising this, we can see that `Appraiser b b'` has additional
structure as a profunctor, specifically, it is a
[`Strong`](https://hackage.haskell.org/package/profunctors/docs/Data-Profunctor-Strong.html)
profunctor.

```haskell
class Profunctor p => Strong p where
  strength :: p a a' -> p (c, a) (c, a')

instance Strong (Appraiser b b') where
  strength (Appraiser l r) = Appraiser l' r'
    where l' ~(c, a) = let (n, b) = l a in ((c, n), b)
          r' ~(~(c, n), b') = let a' = r (n, b') in (c, a')
```

Again, we're stashing this additional data into the `n` component and
extracting it on the other side.^[This `strength` on `Appraiser b b'`
could be factored into two steps; first using that `Appraiser b b'` is
a monoidal profunctor, and then applying that to `opticCup` exactly,
but this fused definition is fine.]

By Yoneda tricks, natural transformations between the evaluation
functors for such profunctors are exactly equivalent to the concrete
data we were trying to capture.

```haskell
type Optic a a' b b' = forall p. Strong p => p b b' -> p a a'

appraise :: Appraiser b b' a a' -> Optic a a' b b'
appraise (Appraiser l r) = dimap l r . strength

unappraise :: Optic a a' b b' -> Appraiser b b' a a'
unappraise o = o (Appraiser (\b -> ((), b)) (\((), b') -> b'))
```

Now we play the same game with learners. Our `School` is already a
profunctor for fixed `b` and `b'`:

```haskell
instance Profunctor (School b b') where
  dimap f g (School l r) = School (lmap (second f) l) (rmap (second g) r)
```

And, because learners have both caps and cups, this profunctor has both strength and a symmetrical "costrength" operation.

```haskell
class Profunctor p => Pedagogical p where
  action :: p a a' -> p (c, a) (c, a')
  reaction :: p (d, a) (d, a') -> p a a'
```

To be a valid instance of `Pedagogical`, we demand that certain laws
hold for these operations. The laws are what you'd expect: for
`action`, the two different ways of constructing $p a a' -> p ((c, d),
a) ((c, d), a')$ are the same, and the two ways of constructing $p a
a' -> p ((), a) ((), a')$ are the same. And flipped versions of these
laws hold for `reaction`. The two are *not* required to be inverses,
however.

Our first example is the `School b b'` profunctor.

```haskell
instance Pedagogical (School b b') where
  action (School l r) = School l' r'
    where l' ~(m, ~(c, a))  = let (n, b)  = l (m, a)  in ((n, c), b)
          r' ~(~(n, c), b') = let (m, a') = r (n, b') in (m , (c, a'))
  reaction (School l r) = School l' r'
    where l' ~(~(m, d), a)  = let (n, b)        = l (m, (d, a)) in ((n, d), b)
          r' ~(~(n, d), b') = let (m, ~(d, a')) = r (n, b')     in ((m, d), a')
```

The definitions for `School` are a mess, but once again we're just
shoving the additional data into the existentially quantified type and
pulling it out on the other side. The tuple patterns are deliberately
lazy, in preparation for some knot-tying later in this post.

By the same Yoneda tricks as for optics, learners are equivalent to natural transformations between evaluation functors:

```haskell
type Learner a a' b b' = forall p. Pedagogical p => p b b' -> p a a'

inculcate :: School b b' a a' -> Learner a a' b b'
inculcate (School l r) = reaction . dimap l r . action

unculcate :: Learner a a' b b' -> School b b' a a'
unculcate o = o (School id id)
```

The learner we are building in `inculcate` is the composite 

```
p b b' -> p (n, b) (n, b') -> p (m, a) (m, a') -> p a a'
```

The cup and cap can be implemented directly:

```haskell
cup :: Learner a a () ()
cup p = dimap (\a -> (a, ())) (\(a, ()) -> a) (action p)

cap :: Learner () () b b
cap p = reaction (dimap (\(b, ()) -> b) (\b -> (b, ())) p)
```

We can also implement involution through a good choice of `Pedagogical` profunctor.

```haskell
newtype Flipped b b' a a' = Flipped
  { unFlipped :: Learner b' b a' a }

instance Profunctor (Flipped b b') where
  dimap l r (Flipped o) = Flipped (o . dimap r l)

instance Pedagogical (Flipped b b') where
  action (Flipped o) = Flipped (o . reaction)
  reaction (Flipped o) = Flipped (o . action)

invol :: Learner a a' b b' -> Learner b' b a' a
invol o = unFlipped (o (Flipped id))
```

This profunctor representation is not so useful unless we actually
have instances of the `Pedagogical` typeclass. For this, we can pilfer
the instances of the more restrictive `ArrowLoop` typeclass, which has
a component with exactly the same shape as `reaction`:

```haskell
instance Pedagogical (->) where
  action f ~(c, a) = (c , f a)
  reaction g a = let (d, a') = g (d, a) in a'

instance MonadFix m => Pedagogical (Kleisli m) where
  action (Kleisli f) = Kleisli $ \ ~(c, a) ->
    do a' <- f a
       return (c, a')
  reaction (Kleisli g) = Kleisli $ \a -> do
    let g' p = g (fst p, a)
    ~(d, a') <- mfix g'
    return a'
```

The `reaction` instance for `(->)` ties a lazy knot, feeding the
output `d` directly in as input: we have to be very careful to avoid
looping infinitely.


## Guessing and Judging

As a first concrete example, let's compose a "judge" who stores a
secret integer and a "guesser" who tries to guess it. We'll work with
lists directly: the guesser sends a stream of guesses to the judge,
and the judge returns a stream of comparisons.

Schematically, here's what will be going on:

```tikzpicture
\tikzset{learner map/.style={draw, circle, inner sep=0pt, minimum size=7mm}}
\node[learner map] (lg) at (0,-0.9) {$$};
\node[learner map] (rg) at (0, 0.9) {$$};
\node[learner map] (lj) at (4.5,-0.9) {$$};
\node[learner map] (rj) at (4.5, 0.9) {$$};
\node at (0,2) {\texttt{guesser}};
\node at (4.5,2) {\texttt{judge}};

% The forward stream travels right; comparisons and the trace travel left.
\draw[-stealth] (-2.5,-0.9) -- node[below] {$\mathtt{Int}$} (lg.west);
\draw[-stealth] (lg.east) -- node[below] {$\mathtt{[Int]}$} (lj.west);
\draw[-stealth] (lj.east) -- node[below] {$()$} (6.9,-0.9);
\draw[-stealth] (6.9,0.9) -- node[above] {$()$} (rj.east);
\draw[-stealth] (rj.west) -- node[above] {$\mathtt{[Ordering]}$} (rg.east);
\draw[-stealth] (rg.west) -- node[above] {$\mathtt{[Int]}$} (-2.5,0.9);

% Each learner's m wire runs down; its saved n wire runs up.
\draw[-stealth] (rg.220) to[out=220,in=140,looseness=1.2]
  node[left] {$\mathtt{[Ordering]}$} (lg.140);
\draw[-stealth] (lg.40) to[out=40,in=320,looseness=1.2]
  node[right] {$\mathtt{[Int]}$} (rg.320);
\draw[-stealth] (rj.220) to[out=220,in=140,looseness=1.2]
  node[left] {$()$} (lj.140);
\draw[-stealth] (lj.40) to[out=40,in=320,looseness=1.2]
  node[right] {$\mathtt{[Ordering]}$} (rj.320);
```

The judge compares each guess in a list with its secret value, and replies with an ordering:

```haskell
judge :: Int -> Learner [Int] [Ordering] () ()
judge secret = inculcate (School l r)
  where
    l ~(_, guesses) = (map (\guess -> compare guess secret) guesses, ())
    r ~(orders, _) = ((), orders)
```

The guesser takes an initial guess, and uses each comparison it
receives to produce the next guess. The residual on the left is a copy of all the orderings it receives from downstream (so that it can be used to produce the next guess), and the residual on the right is the list of all guesses so that the composite can use the stream of all guesses as its reply.

```haskell
guesser :: Learner Int [Int] [Int] [Ordering]
guesser = inculcate (School l r)
  where
    l ~(orders, initial) = (guesses, guesses)
      where
        guesses = initial : zipWith adjust guesses orders
    r ~(guesses, orders) = (orders, guesses)

    adjust guess LT = guess + 1
    adjust guess EQ = guess
    adjust guess GT = guess - 1
```

The payoff of the profunctor encoding is that composition of learners
is just function composition!

```haskell
trial :: Learner Int [Int] () ()
trial = guesser . judge 10
```

To close this off and turn it into an ordinary function, we apply `trial` to the function `id :: () -> ()` to close off the right-hand side of the diagram, implicitly choosing the `Pedagogical (->)` instance. 

```haskell
-- >>> take 12 (trial id 3)
-- [3,4,5,6,7,8,9,10,10,10,10,10]
-- >>> take 8 (trial id 14)
-- [14,13,12,11,10,10,10,10]
```

Ultimately what we have constructed is the following ordinary
knot-tying:

```haskell
guesses = initial : zipWith adjust guesses orders
orders  = map (\guess -> compare guess 10) guesses
```


## Intensional Learners

Let's re-break the symmetry and expose parameter sets again, so that
this encoding might be easier to use in practice, these are what I
called "intensional" learners in the note. The concrete data we're
trying to capture becomes:

```haskell
data ISchool b b' m a a' =
  forall n. ISchool
  { ileft  :: (m, a)  -> (n, b),
    iright :: (n, b') -> (m, a') }
```

All we've done is pull `m` out into a type parameter, rather than existentially quantifying it. We now need "intensional" versions of all the typeclasses we've used:

```haskell
class IProfunctor p where
  idimap :: (a -> b) -> (b' -> a') -> p m b b' -> p m a a'

instance IProfunctor (ISchool b b') where
  idimap f g (ISchool l r) = ISchool (lmap (second f) l) (rmap (second g) r)

newtype Para m a a' = Para (m -> a -> a')

instance IProfunctor Para where
  idimap f g (Para h) = Para $ \m -> dimap f g (h m)
```

And for `Pedagogical`, the `action` remains unchanged but `reaction`
now uses the new type parameter to record the data that was stashed
away. We keep the existing parameters first and append the newly
exposed context, changing `m` to `(m, d)`:

```haskell
class IProfunctor p => IPedagogical p where
  iaction :: p m a a' -> p m (c, a) (c, a')
  ireaction :: p m (d, a) (d, a') -> p (m, d) a a'

instance IPedagogical Para where
  iaction (Para f) = Para $ \m ~(c, a) -> (c , f m a)
  ireaction (Para g) = Para $ \ ~(m, d) a -> snd (g m (d, a))

instance IPedagogical (ISchool b b') where
  iaction (ISchool l r) = ISchool l' r'
    where l' ~(m, ~(c, a))  = let (n, b) = l (m, a) in ((n, c), b)
          r' ~(~(n, c), b') = let (m, a') = r (n, b')  in (m , (c, a'))
  ireaction (ISchool l r) = ISchool l' r'
    where l' ~(~(m, d), a)  = let (n, b)       = l (m, (d, a)) in ((n, d), b)
          r' ~(~(n, d), b') = let (m, ~(d, a')) = r (n, b') in ((m, d), a')
```

And we have a similar profunctor representation.

```haskell
type ILearner a a' b b' m n = forall p. IPedagogical p => p m b b' -> p n a a'

iinculcate :: ISchool b b' n a a' -> ILearner a a' b b' m (m, n)
iinculcate (ISchool l r) = ireaction . idimap l r . iaction

iunculcate :: ILearner a a' b b' n m -> ISchool b b' m a a'
iunculcate o = o (ISchool id id)
```

Each application of `iinculcate` appends its parameters to those
already present. In a composition `f . g . h`, the parameters
from all three learners accumulate.


## Iterating Intensional Learners

The intensional version of learners might be more convenient, in that
we have a direct handle on the parameters that a learner uses. But if
we plan to iterate the learner as in the `guesser` example anyway, it
would be handy to have a way to convert these intensional learners
into regular learners that work iteratively. We'll put this together
in a couple of steps.

The first is a construction on `Pedagogical` profunctors that lifts
them to infinite streams (simulated using lists). This doesn't work
generically over a choice of functor; what we're really using about
the stream constructor is that it preserves products^[(As long as we truly are using the lists as infinite streams!)], i.e., has
`zip` and `unzip` that are inverses.

```haskell
newtype Streamed p a a' = Streamed { unStreamed :: p [a] [a'] }

instance Profunctor p => Profunctor (Streamed p) where
  dimap f g (Streamed p) = Streamed (dimap (fmap f) (fmap g) p)

instance Pedagogical p => Pedagogical (Streamed p) where
  action (Streamed p) = Streamed $
    dimap unzip (\ ~(cs, as') -> zip cs as') (action p)
  reaction (Streamed p) = Streamed $
    reaction (dimap (\ ~(ds, as) -> zip ds as) unzip p)

batch :: Learner a a' b b' -> Learner [a] [a'] [b] [b']
batch o p = unStreamed (o (Streamed p))
```

This is a little more flexible than just applying the learner in parallel: when using the result of `batch`, we might supply *any* stream function `[b] -> [b']` and not just one that acts pointwise.

Next, we need a way to turn any `Pedagogical` profunctor into an intensional `IPedagogical` one. That is, we need a way to capture the state that would ordinarily be tucked away by `reaction` and leave it in the arguments to the profunctor.

```haskell
newtype Captured p m a a' = Captured
  { unCaptured :: p (m, a) (m, a') }

instance Profunctor p => IProfunctor (Captured p) where
  idimap f g (Captured p) = Captured $
    dimap (\ ~(m, a) -> (m, f a))
          (\ ~(m, a') -> (m, g a')) p

instance Pedagogical p => IPedagogical (Captured p) where
  iaction (Captured p) = Captured $
    dimap (\ ~(m, ~(c, a)) -> (c, (m, a)))
          (\ ~(c, ~(m, a')) -> (m, (c, a')))
          (action p)
  ireaction (Captured p) = Captured $
    dimap (\ ~(~(m, d), a) -> (m, (d, a)))
          (\ ~(m, ~(d, a')) -> ((m, d), a')) p

trivialParams :: Profunctor p => p a a' -> Captured p () a a'
trivialParams p = Captured (dimap snd (\a' -> ((), a')) p)
```

Notice that the definition of `ireaction` doesn't actually use `reaction` at all. Given one of these `Captured p m a a'`, we can get back to a plain `p a a'` just by using `reaction`.

Now, we can get the behaviour we want by composing these two constructions.

```haskell
capturedSteps :: ILearner a a' b b' () m
  -> Learner [(m, a)] [(m, a')] [b] [b']
capturedSteps o p = unStreamed (unCaptured (o (trivialParams (Streamed p))))
```

And to get going, we just need to push the initial parameters onto the front of the parameter stream, and then close up the loop!

```haskell
iterateLearner :: m -> ILearner a a' b b' () m
  -> Learner [a] [a'] [b] [b']
iterateLearner m o p = reaction (dimap seed unzip (capturedSteps o p))
  where seed ~(ms, as) = zip (m:ms) as
```


## Gradient Descent

Let's put it all together, with a tiny gradient descent example. Let's start with a "neuron" that just scales its input according to its weight parameter. Both the weight and input are stashed to be used in the backwards pass when updating the weight. The reply `b'` that is handed to a layer  is the derivative of the loss with respect to `b`. Then, for $b=wa$, the parameter gradient is $b'a$, and the input gradient is $wb'$, and we see this directly in the code.

```haskell
layer :: Double -> ILearner Double Double Double Double m (m, Double)
layer rate = iinculcate (ISchool l r)
  where
    l (w, a) = ((w, a), w * a)
    r ((w, a), b') = (w - rate * b' * a, w * b')

inetwork :: ILearner Double Double Double Double () (((), Double), Double)
inetwork = layer 0.05 . layer 0.05
```

Now we initialise the model with some weights and close it up.

```haskell
network :: Learner [Double] [Double] [Double] [Double]
network = iterateLearner (((), 2), 1) inetwork
```

We're yet to actually specify a loss function: the place for this is
the function we apply the learner to that sends outputs to replies.
Here we'll choose the difference between the output and the target; this is the derivative of the half-squared-error.

```haskell
trainingRun :: [Double] -> [Double] -> [Double]
trainingRun ins targets = network judge ins
  where judge results = zipWith (-) results targets
```

Now we can try it out, on repeated inputs of `1`, and target outputs
of `6`. What we see are the ultimate results of the *reply* function,
that is, the input gradients.

```haskell
-- >>> take 5 (trainingRun (repeat 1) (repeat 6))
-- [-8.0,-8.9936,-7.703924129257238,-5.047284566497232,-2.6240549262696793]
```

If we want to see the actual results of the network, there is a trick:
we apply the same learner with a Kleisli morphism for the writer monad
$([Double],)$, which captures the results on the right-hand side!

```haskell
trainingTrace :: [Double] -> [Double] -> ([Double], [Double])
trainingTrace ins targets = runKleisli (network (Kleisli judge)) ins
  where
    judge results = (results, zipWith (-) results targets)

-- >>> let (outs, grads) = trainingTrace (repeat 1) (repeat 6)
-- >>> take 5 outs
-- [2.0,3.08,4.138453280000001,4.988143715505187,5.525063380141243]
-- >>> take 5 grads
-- [-8.0,-8.9936,-7.703924129257238,-5.047284566497232,-2.6240549262696793]
```


## References

```bib
@inproceedings{fst:backprop-as-functor,
  title = {Backprop as functor: a compositional perspective on supervised learning},
  author = {Fong, Brendan and Spivak, David and Tuy\'{e}ras, R\'{e}my},
  doi = {10.1109/LICS.2019.8785665},
  date = {2019},
  isbn = {978-1-7281-3608-0},
  location = {Vancouver, Canada},
  series = {LICS '19},
}

@article{Riley_2025,
   title={Learners are Almost Free Compact Closed},
   volume={429},
   ISSN={2075-2180},
   DOI={10.4204/eptcs.429.3},
   journal={Electronic Proceedings in Theoretical Computer Science},
   publisher={Open Publishing Association},
   author={Riley, Mitchell},
   year={2025},
   month=sep,
   pages={49–62}
}

@article {MR4715033,
    AUTHOR = {Clarke, Bryce and Elkins, Derek and Gibbons, Jeremy and
              Loregian, Fosco and Milewski, Bartosz and Pillmore, Emily and
              Rom\'{a}n, Mario},
     TITLE = {Profunctor optics: a categorical update},
   JOURNAL = {Compositionality},
  FJOURNAL = {Compositionality},
    VOLUME = {6},
      YEAR = {2024},
    NUMBER = {1},
     PAGES = {39},
       DOI = {10.32408/compositionality-6-1},
}
```
