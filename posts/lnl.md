---
title: Linear/Nonlinear Logic and Linear Algebra
published: May 29, 2025
tags: maths, types
---

\newcommand{\Set}{\mathrm{Set}}
\newcommand{\VecR}{\mathrm{Vec}_{\mathbb{R}}}
\newcommand{\lolli}{\multimap}
\newcommand{\yields}{\vdash}
\newcommand{\fin}[1]{\bold{#1}}
\newcommand{\ite}[3]{\mathsf{if}\;{#1}\;\mathsf{then}\;{#2}\;\mathsf{else}\;{#3}}
\newcommand{\ind}[3]{\mathsf{let} \; {#2} = {#1} \; \mathsf{in} \; {#3}}
\newcommand{\case}[5]{\mathsf{case} \; {#1} \; \mathsf{of} \; {#2} \mapsto {#3}; {#4} \mapsto {#5}}

<!-- \newcommand{\unit}{𝟙} -->
\newcommand{\unit}{\mathsf{e}}
\newcommand{\proj}{\mathsf{pr}}
\newcommand{\inl}{\mathsf{inl}}
\newcommand{\inr}{\mathsf{inr}}

\newcommand{\suc}{\mathsf{suc}}

\newcommand{\addprd}{\mathbin{\&}}

\newcommand{\softmax}{\mathrm{softmax}}

\newcommand{\adjF}{\mathsf{F}}
\newcommand{\adjU}{\mathrm{U}}

\let\oldequiv\equiv
\renewcommand{\equiv}{\simeq}
\newcommand{\defeq}{\oldequiv}

\newcommand{\bang}{{!}}

Benton's [Linear/Nonlinear
Logic](https://ncatlab.org/nlab/files/BentonLinearLogic.pdf) has
models in (lax) monoidal adjunctions between monoidal and cartesian
categories.

```tikzcd
\mathcal{C} \ar[r, "F", shift left=1.5] & \mathcal{L} \ar[l, "U", shift left=1.5]
 ```

The tasks performed in GPU kernels often deal with linear and
nonlinear maps between real vector spaces.

```tikzcd
\mathrm{Set} \ar[r, "F", shift left=1.5] & \mathrm{Vec}_{\mathbb{R}} \ar[l, "U", shift left=1.5]
 ```

I want to take this and run with it: let's see what it looks like to
define some mixed linear/nonlinear functions in a linear/nonlinear
type theory. This feels to me so obvious that it must be written down
somewhere already. In the literature there is work that gets close
(and I'll do some comparisons at the end), but none are quite what I
want. Please let me know if I've missed something.

**Edit**: It's been pointed out to me that the type theory I've put
together is almost exactly the [Enriched Effect
Calculus](https://doi.org/10.1093/logcom/exs025), though I still
haven't seen the semantics in ordinary vector spaces discussed
anywhere. <!-- And see my [follow-up](/posts/lnl-ad.html)! -->

<!--more-->


The Setting
-----------

To be totally explicit, what we want to capture is:

* $\Set$ is cartesian closed with finite coproducts, 
* $\VecR$ is symmetric monoidal closed with finite biproducts.
* $\VecR$ is powered and copowered over
  $\Set$, and in particular there is a lax monoidal adjunction given by
  \begin{align*}
  \adjF- &:\defeq - \odot \mathbb{R} \\
  \adjU- &:\defeq \VecR(\mathbb{R}, -)
  \end{align*}
  (and $\adjF$ is automatically also strong monoidal by "doctrinal
  adjunction").

### Judgements

There are different presentations of the logic, some which merge the
$\mathcal{L}$ and $\mathcal{C}$ rules. I want to keep things separated
to avoid (my own) confusion. So we have two term judgements, one
cartesian and one linear.

$$
\Gamma \yields x : X \qquad \qquad
\Gamma \mid \Lambda \yields a :: A
$$

The context of the linear judgement has two "zones", with a term such
as the above interpreted as a morphism $a : \adjF \Gamma \otimes
\Lambda \to A$. We don't need "bunches" in the sense of bunched
logics, because bunching only arises when $\to$ and $\lolli$ exist on
the same mode/category, which is not the case here.

I'll be consistent about using serif and $:$ for nonlinear
definitions, and sans-serif and $::$ for linear definitions.^[Because
linear is bouba and non-linear is kiki.]

### Tensor and Unit Type

Standard rules:

```mathpar
\inferrule*{\Gamma \mid \Lambda \yields a :: A \and \Gamma \mid \Lambda' \yields b :: B}{\Gamma \mid \Lambda, \Lambda' \yields a \otimes b :: A \otimes B} \and
\inferrule*{\Gamma \mid \Lambda, x :: A, y :: B \yields c :: C \\\\ \Gamma \mid \Lambda' \yields t :: A \otimes B}{\Gamma \mid \Lambda, \Lambda' \yields \ind{t}{x \otimes y}{c} :: C} \\\\
\inferrule*{~}{\Gamma \mid \cdot \yields \mathsf{e} :: \mathbb{R}} \and
\inferrule*{\Gamma \mid \Lambda \yields c :: C \\\\ \Gamma \mid \Lambda' \yields r :: \mathbb{R}}{\Gamma \mid \Lambda, \Lambda' \yields \ind{r}{\mathsf{e}}{c} :: C}
```

I will use $ℝ$ to refer to the monoidal unit, although nothing in this
post really requires it to be the real numbers as opposed to an
arbitrary ring. The syntax for the introduction rule $\unit :: ℝ$ is
chosen so that we think of $\unit$ as an "abstract basis vector". It
is tempting to use $1$ here, but $1$ looks more like a constant which
can be dropped into any term in any location, which is certainly not
the case for $\unit$.

In most places, I'm going to write functions that involve these
eliminators using pattern matching notation rather than mentioning the
eliminators explicitly. Sue me!


### Power, Copower and Hom-set

Our first departure from the ordinary presentation of LNL is to
generalise the $\adjF$ and $\adjU$ adjunction to the copower and
"external" hom adjunction

First the copower, a kind of mixed pairing of a nonlinear and linear
type.^[This kind of mixed paring isn't novel, a dependent version of
this was used in the $LNL_D$ theory of Krishnaswami, Pradic and Benton
(cited below).]

```mathpar
\inferrule*{\Gamma \yields n : X \and \Gamma \mid \Lambda \yields a :: A}{\Gamma \mid \Lambda \yields n \odot a :: X \odot A} \and
\inferrule*{\Gamma, x : X \mid \Lambda, a :: A \yields c :: C \\\\ \Gamma \mid \Lambda' \yields p :: X \odot A}{\Gamma \mid \Lambda, \Lambda' \yields \ind{p}{x \odot a}{c} :: C}
```

We will use this type former to define $ℝ^{\fin{n}} :\defeq \fin{n}
\odot ℝ$.^[Though the classical notation $ℝ^{\fin{n}}$ does suggest
using the power here...] Any time we feel the need to say "pick a
basis for $A$", that is a sign we should be using $X \odot ℝ$ instead
(where $X$ can certainly be infinite).

This copower has two different right adjoints, depending on which side
of the $\odot$ we transpose. If we transpose the linear side, we get
the *nonlinear* type of homomorphisms between linear types.^[Distinct
from the linear type $\lolli$ of homomorphisms between linear types.]

```mathpar
\inferrule*{\Gamma \mid x :: A \yields b :: B}{\Gamma \yields \partial x. b : \mathrm{Vec}_{\mathbb{R}}(A, B)} \and
\inferrule*{\Gamma \yields h : \mathrm{Vec}_{\mathbb{R}}(A, B) \\\\ \Gamma \mid \Lambda \yields a :: A}
    {\Gamma \mid \Lambda \yields h(a) :: B}
```

From the above operations we can derive the functors $\adjF$ and
$\adjU$ exactly as suggested earlier: $\adjF :\defeq - \odot
\mathbb{R}$ and $\adjU :\defeq \VecR(\mathbb{R}, -)$. The derived
rules are slightly different to what you see in other presentations of
LNL, but I think it will help to be explicit about what's happening to
the unit type $ℝ$ involved in both of these constructions.

<!-- I'll change the rules slightly compared to other versions of LNL, and -->
<!-- build in a substitution for the unit in the introduction rule. This -->
<!-- avoids a weird context in the conclusion, and I think looks pretty -->
<!-- nice: *of course* you should be allowed to scale the basis vector when -->
<!-- describing an element of $\adjF X$. The $\beta$-rule ends up worse, because -->
<!-- that scaling now has to occur. -->

<!-- ```mathpar -->
<!-- \inferrule*{\Gamma \mid \Lambda \yields \alpha :: \mathbb{R} \and \Gamma \yields n : X} -->
<!--     {\Gamma \mid \Lambda \yields \alpha \mathsf{e}_n :: \adjF X} \and -->
<!-- \inferrule*{\Gamma, x : X \mid \Lambda \yields c :: C \\\\ \Gamma \mid \Lambda' \yields p :: \adjF X}{\Gamma \mid \Lambda, \Lambda' \yields \ind{p}{\mathsf{e}_x}{c} :: C} -->
<!-- ``` -->

<!-- From this we can derive the "underlying set" operation $\adjU $ (also building -->
<!-- in an instance of unit elimination): -->

<!-- ```mathpar -->
<!-- \inferrule*{\Gamma \mid \cdot \yields a :: A} -->
<!--     {\Gamma \yields \partial.a : \adjU A} \and -->
<!-- \inferrule*{\Gamma \yields u : \adjU A \and \Gamma \mid \Lambda \yields r :: \mathbb{R}} -->
<!--     {\Gamma \mid \Lambda \yields u(r) :: A} -->
<!-- ``` -->

For the other right adjoint, if we transpose the nonlinear side of
$\odot$ we get the power $X \pitchfork B$. This is the *linear* type
given by the $X$-fold product of $B$ with itself.^[Where does this
pitchfork notation come from originally?]

```mathpar
\inferrule*{\Gamma, x : X \mid \Lambda \yields b :: B}{\Gamma \mid \Lambda \yields \lambda x. b :: X \pitchfork B} \and
\inferrule*{\Gamma \mid \Lambda \yields p :: X \pitchfork B \and \Gamma \yields n : X}{\Gamma \mid \Lambda \yields p(n) :: B}
```

To summarise the four function types we have now:^[It seems harmless
to have all of these as independent type formers, though you could
define some of them in terms of the others, like $\VecR(A, B) \cong
\adjU(A \lolli B)$.]

| Type Former      | Domain        | Codomain      | Result        |
|------------------|---------------|---------------|---------------|
| $- \to -$        | $\mathcal{C}$ | $\mathcal{C}$ | $\mathcal{C}$ |
| $- \lolli -$     | $\mathcal{L}$ | $\mathcal{L}$ | $\mathcal{L}$ |
| $\VecR(-,-)$     | $\mathcal{L}$ | $\mathcal{L}$ | $\mathcal{C}$ |
| $- \pitchfork -$ | $\mathcal{C}$ | $\mathcal{L}$ | $\mathcal{L}$ |

Accordingly, there are four different ways a variable can be bound:
one for each function type. To help distinguish, I've used $\lambda x$
when binding a nonlinear variable, so for $\to$ and $\pitchfork$, and
$\partial a$ when binding a linear variable, so for $\lolli$ and
$\mathrm{Vec}(-,-)$.


### Boolean

We'll need to be able to eliminate nonlinear inductive types into the
linear term judgement.^[In Mike's terminology this is asking that the
mode morphism from nonlinear types into linear ones is "transparent".
(I think.)] This should be derivable because $\adjF$ is a left
adjoint, but it's convenient to just have it as a rule. For example,
for $\mathrm{Bool}$:

```mathpar
\inferrule*{\Gamma \yields b : \mathrm{Bool} \\\\ 
    \Gamma \mid \Lambda \yields a_t :: A \and 
    \Gamma \mid \Lambda \yields a_f :: A }
    {\Gamma \mid \Lambda \yields \mathsf{if}\;b\;\mathsf{then}\;a_t\;\mathsf{else}\;a_f :: A}
```

This kind of nonlinear interaction is a change in perspective compared
to other applications of LNL. I don't want to think of the nonlinear
world as a "metatheory" for building linear maps (like quantum
circuits, say), but as something that can be jumbled together with the
linear type formers.


### Additive Products

The standard rules for additive products:

```mathpar
\inferrule*{\Gamma \mid \Lambda \yields a :: A \and \Gamma \mid \Lambda \yields b :: B}{\Gamma \mid \Lambda \yields (a \mathbin{\&} b) :: A \mathbin{\&} B} \\
\inferrule*{\Gamma \mid \Lambda \yields p :: A \mathbin{\&} B}{\Gamma \mid \Lambda \yields \proj_1(p) :: A} \and 
\inferrule*{\Gamma \mid \Lambda \yields p :: A \mathbin{\&} B}{\Gamma \mid \Lambda \yields \proj_2(p) :: B}
```

We could also put in additive coproducts, but we're not going to need them.

### Global Additivity

This is the other feature that goes beyond the original LNL logic: we
add in global "additivity" rules for _all_ linear types. This will
turn the products we just put in above into biproducts.^[And note also
that "addition is additive", in that the linear context is not split
into two pieces by the $+$ rule.]

```mathpar
\inferrule*{~}{\Gamma \mid \Lambda \yields 0 :: A} \and
\inferrule*{\Gamma \mid \Lambda \yields a_1 :: A \and \Gamma \mid \Lambda \yields a_2 :: A}{\Gamma \mid \Lambda \yields a_1 + a_2 :: A}
```

Besides being commutative, associative and unital, these global rules
will need to interact sensibly with the other type formers:

\begin{align*}
\ind{0}{\unit}{c} &\defeq 0 \\
\ind{0}{x \otimes y}{c} &\defeq 0 \\
0 &\defeq \partial x. 0 \\
0 &\defeq 0 \addprd 0 \\
~\\
\ind{a + a'}{\unit}{c} &\defeq (\ind{a}{\unit}{c}) + (\ind{a'}{\unit}{c}) \\
\ind{a + a'}{x \otimes y}{c} &\defeq (\ind{a}{x \otimes y}{c}) + (\ind{a'}{x \otimes y}{c}) \\
(\partial x. a) + (\partial x. a') &\defeq \partial x. (a + a') \\
(a \addprd b) + (a' \addprd b') &\defeq (a + a') \addprd (b + b') \\
\end{align*}

I'm thinking of these equations as directed, so we prefer to rewrite
the left sides into the right sides. (I may be missing some...)


Examples
--------

Now let's run the experiment. What does it feel like to write down
some basic definitions in this thing?

### Monoidality and Functoriality

Let's check quickly that we get out what we think we put in. Remembering that $\adjF X$ and $\adjU A$ are shorthand for $X \odot ℝ$ and $\VecR(ℝ, A)$ respectively:

\begin{align*}
&\mathsf{mon}_\adjF :: \adjF X \otimes \adjF Y \lolli \adjF(X \times Y) \\
&\mathsf{mon}_\adjF((x \odot \unit) \otimes (y \odot \unit)) :\defeq (x, y) \odot \unit \\
~\\
&\mathsf{mon}_\adjF^{-1} :: \adjF(X \times Y) \lolli \adjF X \otimes \adjF Y \\
&\mathsf{mon}_\adjF^{-1}((x, y) \odot \unit) :\defeq (x \odot \unit) \otimes (y \odot \unit) \\
~\\
&\mathrm{mon}_\adjU  : \adjU A \times \adjU B \to \adjU (A \otimes B)\\
&\mathrm{mon}_\adjU (a, b)(\unit) :\defeq a(\unit) \otimes b(\unit) \\
\end{align*}

(Or in $\mathrm{mon}_\adjU$ we could not pattern match on the unit and
use the argument on one of the sides.)

It's similarly straightforward to show that all our type formers are
functorial. For purely linear or nonlinear type formers it goes
exactly as normal, so I'll just demonstrate the ones that involve both
linear and nonlinear types.

\begin{align*}
&\mathsf{func}_\odot :: \adjF(X \to Y) \lolli (A \lolli B) \lolli (X \odot A \lolli Y \odot B) \\
&\mathsf{func}_\odot(f \odot \unit)(h)(x \odot a) :\defeq f(x) \odot h(a) \\
~\\
&\mathsf{func}_\pitchfork :: \adjF(Y \to X) \lolli (A \lolli B) \lolli (X \pitchfork A \lolli Y \pitchfork B) \\
&\mathsf{func}_\pitchfork(f \odot \unit)(h)(p)(y) :\defeq h(p(f(y))) \\
~\\
&\mathrm{func}_{\VecR} : \VecR(D, C) \to \VecR(A, B) \to (\VecR(C, A) \to \VecR(D, B)) \\
&\mathrm{func}_{\VecR} (h, g, f)(d) :\defeq g(f(h(d)))
\end{align*}

The definitions for $\odot$ and $\VecR$ specialise to $\adjF$ and
$\adjU$:

\begin{align*}
&\mathsf{func}_\adjF :: \adjF(X \to Y) \lolli (\adjF X \lolli \adjF Y) \\
&\mathsf{func}_\adjF(f \odot \unit)(x \odot r) :\defeq f(x) \odot r \\
~\\
&\mathrm{func}_\adjU  : \VecR(A, B) \to (\adjU A \to \adjU B) \\
&\mathrm{func}_\adjU (h, a)(r) :\defeq h(a(r))
\end{align*}


Interestingly, there's a different kind of functoriality for
$\pitchfork$: nonlinear functoriality on the underlying sets. This is
a genuinely different construction to the above, not just its image
under some bijection, because the provided function could nonlinearly
scramble all the elements of $\adjU A$.^[What a mess!]

\begin{align*}
&\mathrm{func}_\pitchfork : (Y \to X) \to (\adjU A \to \adjU B) \to \adjU (X \pitchfork A) \to \adjU (Y \pitchfork B) \\
&\mathrm{func}_\pitchfork(f, g, u)(r)(y) :\defeq g(\partial r'. u(r')(f(y)))(r)
\end{align*}

I am unable to define analogous function for $\odot$, even though it
feels like it ought to be possible (using additivity maybe).

\begin{align*}
&\mathrm{func}_\odot : (X \to Y) \to (\adjU A \to \adjU B) \to \adjU (X \odot A) \to \adjU (Y \odot B) \\
&\mathrm{func}_\odot(f, g, u) :\defeq {???}
\end{align*}


### Biproducts

Global additivity gives $A \mathbin{\&} B$ the universal property of
the coproduct. Let's just rename the type former to $A \oplus B$ from
this point on (though in linear logic this is usually used for the
additive coproduct).

\begin{align*}
&\inl :: A \lolli A \oplus B \\
&\inl(a) :\defeq a \oplus 0 \\
&\inr :: B \lolli A \oplus B \\
&\inr(b) :\defeq 0 \oplus b 
\end{align*}

Coproduct induction comes from addition:

\begin{align*}
&\mathsf{case} :: (A \lolli C) \oplus (B \lolli C) \lolli (A \oplus B) \lolli C \\ 
&\mathsf{case}(r, s) :\defeq \proj_1(r)(\proj_1(s)) + \proj_2(r)(\proj_2(s))
\end{align*}

Or, if you prefer an inference rule:

```mathpar
\inferrule*{\Gamma \mid \Lambda \yields s :: A \oplus B \\
    \Gamma \mid \Lambda', a :: A \yields l :: C \and 
    \Gamma \mid \Lambda', b :: B \yields r :: C }
    {\Gamma \mid \Lambda, \Lambda' \yields (\case{s}{\inl(a)}{l}{\inr(b)}{r}) :: C}
```


<!-- While we're here, let's check that $\adjF$ and $\adjU $ preserve -->
<!-- coproducts/products.^[This is more about demonstrating the syntax than -->
<!-- blowing your mind.] -->

<!-- \begin{align*} -->
<!-- &\mathsf{coprod} :: \adjF X \oplus \adjF Y \lolli \adjF(X + Y) \\ -->
<!-- &\mathsf{coprod}(\inl(x \odot r)) :\defeq \inl(x) \odot r \\ -->
<!-- &\mathsf{coprod}(\inr(y \odot r)) :\defeq \inr(y) \odot r \\ -->
<!-- ~\\  -->
<!-- &\mathsf{coprod}^{-1} :: \adjF(X + Y) \lolli \adjF X \oplus \adjF Y \\ -->
<!-- &\mathsf{coprod}^{-1}(\inl(x) \odot r) :\defeq \inl(x \odot r) \\ -->
<!-- &\mathsf{coprod}^{-1}(\inr(y) \odot r) :\defeq \inr(y \odot r) \\ -->
<!-- ~\\ -->
<!-- &\mathsf{prod} : \adjU A \times \adjU B \to \adjU (A \oplus B) \\ -->
<!-- &\mathsf{prod}(v, w) :\defeq \partial r. v(r) \oplus w(r) \\ -->
<!-- ~\\ -->
<!-- &\mathsf{prod}^{-1} : \adjU (A \oplus B) \to \adjU A \times \adjU B \\ -->
<!-- &\mathsf{prod}^{-1}(u) :\defeq (\partial r. \proj_1(u(r)), \partial r. \proj_2(u(r)))  -->
<!-- \end{align*} -->


### Arithmetic

Multiplication corresponds to the unitor for $ℝ$.

\begin{align*}
&\mathsf{mult} :: ℝ \otimes A \lolli A \\
&\mathsf{mult}(\unit \otimes a) :\defeq a \\
~\\
&\mathsf{mult} :: ℝ \otimes ℝ \lolli ℝ \\
&\mathsf{mult}(\unit \otimes r) :\defeq r
\end{align*}

In the latter case, we could do unit-induction on both sides or just
keep it one-sided. We can derelict multiplication to a nonlinear map,
using the same idea as $\mathrm{mon}_\adjU$:

\begin{align*}
&\mathrm{mult} : \adjU ℝ \times \adjU ℝ \to \adjU ℝ \\
&\mathrm{mult}(a, b) :\defeq \partial \unit. \mathsf{mult}(a(\unit) \otimes b(\unit))
\end{align*}

Nonlinearly, we have $1 : \adjU ℝ$ defined as the identity
homomorphism $1 :\defeq \partial r. r$.

We certainly have $0 :: ℝ$ linearly because we put zero in as a rule,
from which we can define $\partial r. 0 : \adjU ℝ$. Addition is *not*
bilinear, so the best we can hope for is a map $\adjU ℝ \times \adjU ℝ
\to \adjU ℝ$, which we can define using global additivity.

\begin{align*}
&\mathrm{add} : \adjU ℝ \times \adjU ℝ \to \adjU ℝ \\
&\mathrm{add}(a, b) :\defeq \partial r. a(r) + b(r)
\end{align*}

At this point it would be sensible to check that the ring axioms hold
for $\adjU ℝ$, using the equations we put in for $+$.


### Matmul

Let's suppose we have (nonlinear) finite types $\fin{n}$, which we can
pattern match on and manipulate as normal. Euclidean spaces
$ℝ^\fin{n}$ are defined by $ℝ^\fin{n} :\defeq \fin{n} \odot ℝ$, or in
other words, $\adjF\fin{n}$. We can define the dot product as a linear
map, using case analysis on the _nonlinear_ Boolean value of $i =
j$.

\begin{align*}
&\mathsf{dot} :: ℝ^\fin{n} \otimes ℝ^\fin{n} \lolli ℝ \\
&\mathsf{dot}((i \odot r) \otimes (j \odot s)) :\defeq \ite{i=j}{\mathsf{mult}(r, s)}{0}
\end{align*}

Or, if you want to be edgy:^[In the special case of $X \odot ℝ$, we
could write the term/pattern $(i \odot \unit)$ as $\unit_i$: it really
is like we're pattern-matching on basis vectors. But I'll resist doing
that in this post, to make it easier to remember that it's really
$\odot$.]

\begin{align*}
&\mathsf{dot}((i \odot \unit) \otimes (j \odot \unit)) :\defeq \ite{i=j}{\unit}{0}

\end{align*}

We are usually handed two separate vectors nonlinearly, and like
before we can handle this by dereliction.

\begin{align*}
&\mathrm{dot} : \adjU ℝ^\fin{n} \times \adjU ℝ^\fin{n} \to \adjU ℝ \\
&\mathrm{dot}(u, v) :\defeq \partial \unit. \mathsf{dot}(u(\unit) \otimes v(\unit))
\end{align*}

These definitions generalise easily to matrix multiplication, by
functoriality of $\otimes$. This is the place where the
linear/nonlinear logic really shines, so let's hope there's a lot of
this.

\begin{align*}
&\mathsf{matmul} :: ℝ^\fin{m} \otimes ℝ^\fin{n} \otimes ℝ^\fin{n} \otimes ℝ^\fin{k} \lolli ℝ^\fin{m} \otimes ℝ^\fin{k} \\
&\mathsf{matmul}(v \otimes (i \odot \unit) \otimes (j \odot \unit) \otimes w) :\defeq \ite{i=j}{v \otimes w}{0} \\
\end{align*} 

<!-- ~\\ -->
<!-- &\mathrm{matmul} : \adjU (ℝ^\fin{m} \otimes ℝ^\fin{n}) \times \adjU (ℝ^\fin{n} \otimes ℝ^\fin{k}) \to \adjU (ℝ^\fin{m} \otimes ℝ^\fin{k}) \\ -->
<!-- &\mathrm{matmul}(P, Q) :\defeq \partial \unit. \mathsf{matmul}(P(\unit) \otimes Q(\unit)) -->


Of course, because we're already working in a theory that has linear
functions, we'd much rather use those than explicit matrices wherever
we can.

\begin{align*}
&\mathsf{unexplode}_\lolli :: (ℝ^\fin{n} \otimes A) \lolli (ℝ^\fin{n} \lolli A) \\
&\mathsf{unexplode}_\lolli(v \otimes a)(w) :\defeq \mathsf{mult}(\mathsf{dot}(v \otimes w) \otimes a) \\
\end{align*}

It might come in handy to know that $\mathsf{unexplode}_\lolli$ is a
linear map, but of course we can also define the same thing
nonlinearly:

\begin{align*}
&\mathrm{unexplode}_{\VecR} : \adjU(ℝ^\fin{n} \otimes A) \to \VecR(ℝ^\fin{n}, A) \\
&\mathrm{unexplode}_{\VecR}(t)(w) :\defeq \mathsf{unexplode}_\lolli(t(\unit))(w)
\end{align*}


To define the inverse of this, we'll need to cover iteration.


### Iteration

How do we iterate over a vector of real numbers? Realistically, we
probably want iteration over a vector to be a primitive^[As it is in
[Futhark](https://futhark-lang.org/), for example.], but assuming some
reasonable rules for the finite types $\fin{n}$ we can do it by
induction.^[These examples do need proper nonlinear dependent types to
make sense, but hopefully you get the idea. We are doing dependent
elimination on the $n : \mathbb{N}$ parameter of the finite type
former.]

The linear isomorphism

\begin{align*}
&\mathsf{cons} :: ℝ \oplus ℝ^\fin{n} \lolli ℝ^\fin{n+1} \\
&\mathsf{cons}(\inl(r)) :\defeq \mathsf{zero} \odot r \\
&\mathsf{cons}(\inr(i \odot r)) :\defeq \suc(i) \odot r \\
~\\
&\mathsf{cons}^{-1} :: ℝ^\fin{n+1} \lolli ℝ \oplus ℝ^\fin{n} \\
&\mathsf{cons}^{-1}(\mathsf{zero} \odot r) :\defeq \inl(r) \\
&\mathsf{cons}^{-1}(\suc(i) \odot r) :\defeq \inr(i \odot r) \\
\end{align*}

suggests that we can pattern match on elements of $ℝ^\fin{n+1}$ as $ℝ
\oplus ℝ^\fin{n}$^[I'm cheating here, officially we are accessing
elements of $A \oplus B$ by projection rather than pattern matching,
but the projection syntax is a bit noisy.]. Then, for example:

\begin{align*}
&\mathrm{sum}_\fin{n} : \adjU ℝ^\fin{n} \to \adjU ℝ \\
&\mathrm{sum}_\fin{0}(v) :\defeq \partial r. 0 \\
&\mathrm{sum}_\fin{n+1} (v) :\defeq \mathsf{add}(\partial r. \proj_1(v(r)), \partial r. \mathrm{sum}_\fin{n}(\proj_2(v(r))))
\end{align*} 

and

\begin{align*}
&\mathrm{map}_\fin{n} : (\adjU ℝ \to \adjU ℝ) \to \adjU ℝ^\fin{n} \to \adjU ℝ^\fin{n} \\
&\mathrm{map}_\fin{0}(f, v) :\defeq \partial r. 0 \\
&\mathrm{map}_\fin{n+1}(f, v) :\defeq \partial r. \mathsf{cons}(\proj_1(v(r)) \oplus \mathrm{map}_\fin{n}(f, \partial r'. \proj_2(v(r')))(r))
\end{align*} 

<!-- &\mathrm{map}_{n+1}(f, v) :\defeq \partial r. \case{v(r)}{\inl(s)}{\inl(f(s)(\unit))}{\inr(w)}{\inr(\mathrm{map}(f, w)(\unit))}-->

This kind of induction also lets us give an inverse to
$\mathsf{unexplode}$ above:

\begin{align*}
&\mathsf{explode}_{\fin{n}} :: (ℝ^\fin{n} \lolli A) \lolli (ℝ^\fin{n} \otimes A) \\
&\mathsf{explode}_{\fin{0}}(h) :\defeq 0 \\
&\mathsf{explode}_{\fin{n+1}}(h) :\defeq (\mathsf{zero} \odot \unit) \otimes h(\mathsf{zero} \odot \unit) + (\iota \otimes \mathsf{id}_A)(\mathsf{explode}_{\fin{n}}(h \circ \iota))
\end{align*}
where $\iota :: ℝ^\fin{n} \lolli ℝ^\fin{n+1}$ is the inclusion $\iota(i \odot e) :\defeq \mathsf{suc}(i) \odot e$. Nonlinearly:

\begin{align*}
&\mathrm{explode} : \VecR(ℝ^\fin{n}, A) \to \adjU(ℝ^\fin{n} \otimes A) \\
&\mathrm{explode}(h)(\unit) :\defeq \mathsf{explode}(\partial v. h(v))
\end{align*}

<!-- &\mathsf{explode}_{\fin{n+1}}(h) :\defeq \ind{\mathsf{explode}_{\fin{n}}(h\circ \iota)}{v \otimes a}{?} \\ -->

What does it look like to apply a nonlinear function along an axis?
This can't be done generically, in the sense of defining a map

$$
\mathrm{map}_2 : (\adjU B \to \adjU C) \to \adjU (A \otimes B) \to \adjU (A \otimes C)
$$

for an arbitrary linear type $A$, because the result really does
depend on a choice of basis for $A$. This is easiest to do if we first
take the inputs in unexploded form.^[Using $X \odot ℝ$ makes clear
that this function *does* require us to pick a basis, but *doesn't*
require the vector space to be finite dimensional.]

\begin{align*}
&\mathrm{along} : (\adjU B \to \adjU C) \to \VecR(X \odot ℝ, B) \to \VecR(X \odot ℝ, C) \\
&\mathrm{along}(f, h)(i \odot r) :\defeq f(\partial r'. h(i \odot r'))(r)\\
\end{align*}

Now, if we really want to define $\mathsf{map}_2$ as map on (the
underlying set of) the tensor type, we unexplode the matrix into a
homomorphism, compose, and then re-explode:^[And here, we see that the
final exploding step is the only place we actually need that
$\mathbb{R}^\mathbf{q}$ is finite dimensional.]

\begin{align*}
\adjU (\mathbb{R}^\mathbf{q} \otimes B) \longrightarrow \VecR (\mathbb{R}^\mathbf{q}, B) \longrightarrow \VecR (\mathbb{R}^\mathbf{q}, C) \longrightarrow \adjU (\mathbb{R}^\mathbf{q} \otimes C)
\end{align*}

So:

\begin{align*}
&\mathrm{map}_2 : (\adjU B \to \adjU C) \to \adjU (\mathbb{R}^\mathbf{q} \otimes B) \to \adjU (\mathbb{R}^\mathbf{q} \otimes C) \\
&\mathrm{map}_2(f) :\defeq \mathrm{explode} \circ \mathrm{along}(f) \circ \mathrm{unexplode}
\end{align*}


### Attention

Let's try to apply what we've learned so far to the most famous
linear/nonlinear map of $ℝ$-vector spaces in the world: the attention
mechanism. Here's a string diagram of what we're trying to replicate
in code, where the "caps" represent the $\mathrm{dot}$ map from above,
and the triangular node represents $\softmax$:^[I find these kinds of
"string diagrams" quite confusing, because the interchange law fails
when mixing linear/nonlinear maps and the meaning of a diagram is not
invariant under isotopy. In other words, the relative horizontal
position of the nodes matters; even when the nodes are completely
disconnected from one other. Here, pulling the upper cap past the
$\softmax$ results in a different map.]

![Attention as a "neural circuit diagram", from
<https://arxiv.org/abs/2505.09326> by Abbott et
al.](/diagrams/attention.png)

Of course, we can't fuse this into a single linear map because it
involves the non-linear $\softmax : \adjU ℝ^\fin{n} \to \adjU ℝ$.^[We
could define $\softmax$ ourselves using iteration, if we assume we
have $\mathrm{exp} : \adjU ℝ \to \adjU ℝ$ and $\mathrm{recip} : \adjU
ℝ \to \adjU ℝ$, say.] Taken literally, that diagram is

\begin{align*}
&\mathrm{attention} : \adjU (ℝ^\fin{q} \otimes ℝ^\fin{k}) \times \adjU (ℝ^\fin{x} \otimes ℝ^\fin{k}) \times \adjU (ℝ^\fin{x} \otimes ℝ^\fin{k}) \to \adjU (ℝ^\fin{q} \otimes ℝ^\fin{k})
\end{align*} 

But we're better just using $\VecR$ everywhere we can, and exploding
things later if we really want:

\begin{align*}
&\mathrm{attention} : \VecR(ℝ^\fin{q}, K) \times \VecR(K, X) \times \VecR (X, K) \to \VecR (ℝ^\fin{q}, K) \\
&\mathrm{attention}(Q, K, V) :\defeq V \circ \mathrm{along}(\softmax)(K \circ Q)
\end{align*} 

This is the definition we get just clicking things together. If we
inline all the definitions, it's a bit more direct.

\begin{align*}
&\mathrm{attention} : \VecR(ℝ^\fin{q}, K) \times \VecR (K, X) \times \VecR (X, K) \to \VecR (ℝ^\fin{q}, K) \\
&\mathrm{attention}(Q, K, V)(q \odot \unit) :\defeq V(\softmax(\partial r. K(Q(q \odot r))))
\end{align*} 

This is about as simple as we could hope for! It almost looks like we
haven't done anything at all, but this syntax tracks and enforces the
correct interaction of the linear and nonlinear parts of the
definition. I think this is a good place to stop for now.


Dependent Types
---------------

One probably wants a version of this that has dependent nonlinear
types, and linear types that can depend on those nonlinear types (but
not on other linear types). If nothing else, we need this to actually
justify the way we used the finite types $\fin{n}$. For inspiration
one should probably look at:

```bib
@inproceedings{kpb:lnld,
  title = {Integrating Linear and Dependent Types},
  author = {Krishnaswami, Neelakantan R. and Pradic, Pierre and Benton, Nick},
  booktitle = {POPL},
  date = {2015}, 
  doi = {10.1145/2676726.2676969},
  pages = {17--30},
}

@article{vakar:deplin3,
  title = {Syntax and Semantics of Linear Dependent Types},
  author = {V\'{a}k\'{a}r, Matthjis},
  date = {2014},
  eprint = {1405.0033},
  eprintclass = {cs.AT},
  eprinttype = {arXiv},
}

@article{isaev:itt,
  title = {Indexed type theories},
  author = {Isaev, Valery},
  date = {2021},
  doi = {10.1017/S0960129520000092},
  journaltitle = {Mathematical Structures in Computer Science},
  number = {1},
  pages = {3--63},
  volume = {31},
}
```

Related Work
-------------

There are a couple of similar existing bits of work. Closest is
Jennifer Paykin's thesis and some of the related papers.


```bib
@phdthesis{paykin:thesis,
  title = {Linear/Non-Linear Types For Embedded Domain-Specific Languages},
  author = {Paykin, Jennifer},
  date = {2018},
  doi = {20.500.14332/29702},
  institution = {University of Pennsylvania},
}
```

The thesis demonstrates how to implement LNL as an embedded language
inside a nonlinear host language like Haskell or Coq, and this looks
like it works very well in practice. If I understand right, the focus
is more on using the nonlinear language as a metalanguage for the
linear language, rather than considering the whole system together as
being semantically useful. This is also the case for the Quipper line
of work. Her $\mathtt{Box}$ constructor is like the external hom,
which I here called $\VecR(-,-)$.

Then, there are the CHADs.

```bib
@online{vakar:deplin,
  title = {Syntax and Semantics of Linear Dependent Types},
  author = {V\'{a}k\'{a}r, Matthjis},
  date = {2014},
  eprint = {1405.0033},
  eprintclass = {cs.AT},
  eprinttype = {arXiv},
}

@inproceedings{vakar:reverse-ad,
  title = {Reverse AD at Higher Types: Pure, Principled and Denotationally Correct},
  author = {V\'{a}k\'{a}r, Matthijs},
  booktitle = {Programming Languages and Systems},
  date = {2021},
  isbn = {978-3-030-72019-3},
  location = {Cham},
  pages = {607--634},
  publisher = {Springer International Publishing},
}

@article{vs:chad,
  title = {CHAD: Combinatory Homomorphic Automatic Differentiation},
  author = {V\'{a}k\'{a}r, Matthijs and Smeding, Tom},
  date = {2022-08},
  doi = {10.1145/3527634},
  issn = {0164-0925},
  journaltitle = {ACM Trans. Program. Lang. Syst.},
  location = {New York, NY, USA},
  number = {3},
  publisher = acm,
  volume = {44},
}

@article{MR4640115,
  title = {C{HAD} for expressive total languages},
  author = {Lucatelli Nunes, Fernando and V\'{a}k\'{a}r, Matthijs},
  date = {2023},
  doi = {10.1017/s096012952300018x},
  issn = {0960-1295},
  journaltitle = {Math. Structures Comput. Sci.},
  number = {4-5},
  pages = {311--426},
  volume = {33},
}

@article{sv:efficient-chad,
  title = {Efficient CHAD},
  author = {Smeding, Tom J. and V\'{a}k\'{a}r, Matthijs I. L.},
  date = {2024-01},
  doi = {10.1145/3632878},
  journaltitle = {Proc. ACM Program. Lang.},
  keywords = {automatic differentiation,functional programming,source transformation},
  location = {New York, NY, USA},
  number = {POPL},
  publisher = acm,
  volume = {8},
}

@misc{npv:iterative-chad,
  title = {Unraveling the iterative CHAD},
  author = {Nunes, Fernando Lucatelli and Plotkin, Gordon and V\'{a}k\'{a}r, Matthijs},
  date = {2025},
  eprint = {2505.15002},
  eprintclass = {cs.PL},
  eprinttype = {arXiv},
}
```

This is exciting work, and the part relevant to us is the
linear/nonlinear "target" language for their source transformation.
Unfortunately the linear context zone in this type theory is a
"stoup", meaning that it contains exactly one linear variable.^[If
there's some CHAD work that I missed which covers the properly linear
tensor product, please let me know!] As a result, that language is not
able to express the linear tensor product, which is something I
definitely want to have.

Separately, additivity and the $\lambda$-calculus has been considered
in the following line of work:

```bib
@article{diaz-caro-dowek:linear-linear,
  title = {A linear linear lambda-calculus},
  author = {D\'iaz-Caro, Alejandro and Dowek, Gilles},
  date = {2024},
  doi = {10.1017/S0960129524000197},
  journaltitle = {Mathematical Structures in Computer Science},
  number = {10},
  pages = {1103--1137},
  volume = {34},
}

@misc{dim:ill,
  title = {An Algebraic Extension of Intuitionistic Linear Logic: The $L_!^S$-Calculus and Its Categorical Model},
  author = {D\'iaz-Caro, Alejandro and Ivnisky, Malena and Malherbe, Octavio},
  date = {2025},
  eprint = {2504.12128},
  eprintclass = {cs.LO},
  eprinttype = {arXiv},
}
```

The first paper considers additivity in the context of the ordinary
lambda calculus, and the second considers additivity in the context of
intuitionistic linear logic but with a dual-zone context structure
different to ours. There are only linear types, with the "nonlinear"
part of the context corresponding to linear types that have had the
$\bang$ comonad applied. Accordingly, there are no nonlinear type
formers, like functions/products/coproducts etc.

There is also some work on modelling the lambda calculus directly in
finite dimensional vector spaces over finite fields.

```bib
@article{valiron-zdancewic:finite-vector-spaces,
  title = {Modeling simply-typed lambda calculi in the category of finite vector spaces},
  author = {Valiron, Beno\^it and Zdancewic, Steve},
  date = {2014},
  doi = {10.7561/SACS.2014.2.325},
  url = {https://publications.info.uaic.ro/files/sacs/XXIV2/XXIV2_5.pdf},
  issn = {1843-8121},
  journaltitle = {Sci. Ann. Comput. Sci.},
  number = {2},
  pages = {325--368},
  volume = {24},
}

@misc{pratt1992linear,
  author = {Pratt, Vaughan},
  title = {Re: Linear logic semantics (Barwise)},
  howpublished = {Email to linear@cs.stanford.edu},
  year = {1992},
  month = {February},
  day = {25},
  note = {Email correspondence},
  url = {https://www.seas.upenn.edu/~sweirich/types/archive/1992/msg00047.html}
}
```

Here, the theory being modelled really is the ordinary non-linear
lambda calculus without any linear features, though they do also
consider additivity.

The purpose of both restricting to finite dimensional vector spaces
and finite base fields is so that the comonad $\bang :\defeq \adjF
\adjU$ (in our notation) is strong monoidal: $\bang(A \times B) \cong
\bang A \otimes \bang B$. This is not a problem for us, because our
priority is matching the semantics in $\mathbb{R}$-vector spaces
rather than matching the linear logic rules for $!$.

<!-- https://dl.acm.org/doi/pdf/10.1145/3736112.3736141 -->
