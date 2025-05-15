---
title: Officers on the GPU
published: March 8, 2025
tags: code, cuda, officers
---

Officers is a two-player "take-and-break" game played with heaps of
coins (or if you prefer, piles of beans, or [officers and their
subordinates], or [groups of people who form indivisible couples]^[In
the equivalent game Couples-are-Forever, a move is to choose a heap
and split it into two, with the proviso that you may not split a heap
of size 2. An Officers heap of size $n$ is equivalent to a
Couples-are-Forever heap of size $n+1$.], or ...). The players
alternate turns, and a turn consists of removing a coin from a heap
and leaving the remainder in either one or two heaps. In particular,
taking away a lone coin is not a valid move. The winner is the last
player who can make a move.

We can solve a game of this kind by calculating the *Grundy value* for
each position, and in this post I'm going to discuss my attempt at
calculating these values for Officers as fast as possible.

<!--more-->

Officers is the only unsolved "single-digit octal game",^[Achim
Flammenkamp has the most up-to-date [status page], to the best of my
knowledge.] and like all finite octal games, its values are
conjectured to be eventually periodic. J. P. Grossman has calculated
140 trillion values, with no signs of periodicity yet! In the first
few sections I'll recapitulate some of the strategies Grossman used,
before moving to the world of CUDA.

I'm a newcomer to GPU programming, so I would appreciate any comments
or corrections. This will be embarrassingly basic for anyone who knows
what they're doing.

[officers and their subordinates]: https://www.routledge.com/Winning-Ways-for-Your-Mathematical-Plays-Volume-1/Berlekamp-Conway-Guy/p/book/9781568811307
[groups of people who form indivisible couples]: https://doi.org/10.2307/2589561
[status page]: https://wwwhomes.uni-bielefeld.de/achim/octal.html


Sprague–Grundy Theory
---------------------

Here's a very compressed outline of Sprague-Grundy theory, just what
we need to work on Officers.^[See the references for more details.]

The *Grundy value* of an Officers position is the equivalent size of
heap in the game of *Nim*. This this value must exist thanks to the
Sprague–Grundy theorem, which states that every "impartial game" (both
players have the same options from any position) under the "normal
play convention" (a player who can't make a move loses) is equivalent
to a one-heap game of Nim. The winning strategy in Nim is easy, so the
strategy for Officers becomes to convert the position to Nim,
determine the winning move, and convert back.

Nim is also played with heaps of coins, but the rules are simpler. In
Nim, a turn consists of choosing a pile and removing any number of
coins, possibly all of them. For a single pile, the strategy for the
first player is obvious: take the entire pile and you win. For two
equal piles, the second player can win: replicate whatever move is
made on one pile to the other, and eventually the first player will
run out of options.

To get at the general strategy, suppose we have a "false" Nim heap
where all the options are ordinary Nim heaps, say, a "heap" where we
can move to one of 0, 1, 4 or 5. This behaves like a heap of size 2,
but where we also have the option to increase the heap to 4 or 5. But!
These latter two moves cannot be part of a winning strategy, because
the opponent can always make a reversing move back to the (true) Nim
heap of size 2, and thereby force you to make a move to 0 or 1. And so
our false Nim heap is equivalent to the heap of size 2, the *minimal
excluded value* of all of its options. (Sanity check: for a true Nim
heap of size $n$, indeed $n$ is the first excluded option.)

To determine the value $x ⊕ y$ of Nim games with two piles of size $x$
and $y$, we apply this rule inductively. From $1 ⊕ 2$ we can move to
any of $\{0 ⊕ 2, 1 ⊕ 1, 1 ⊕ 0\}$, which have known values $\{2, 0,
1\}$. The minimum excluded value is 3, so $2 ⊕ 1 = 3$. Continuing a
little further,^[Computing these MEXes also reveals the winning
strategy for each sum: move to a position with value 0. So for these
sums, $1 ⊕ 3 \rightsquigarrow 1 ⊕ 1$ and $2 ⊕ 3 \rightsquigarrow 2 ⊕
2$.]

<div class="align-bug">
\begin{align*}
1 ⊕ 3 &= \mathrm{mex}(0⊕3, 1⊕2, 1⊕1, 1⊕0) &&= \mathrm{mex}(3, 3, 0, 1) &&= 2 \\
2 ⊕ 3 &= \mathrm{mex}(1⊕3, 0⊕3, 2⊕2, 2⊕1, 2⊕0) &&= \mathrm{mex}(2, 3, 0, 3, 2) &&= 1
\end{align*}
</div>

In general, one can show that the Nim sum $x ⊕ y$ is binary XOR, or if
you like, vector addition over $\mathbb{F}_2$.

Now how do we apply this to Officers? We consider each Officer heap as
a false Nim heap and identify the minimal excluded value.

From Officers heaps of size 0 and 1 there are no moves, so $G(0) =
G(1) = 0$. From position 2, there is a unique move to 1, so $G(2) =
\mathrm{mex}(G(1)) = \mathrm{mex}(0) = 1$. From position 3, we now
have a choice after removing a coin: whether or not to split the
result. The two options are therefore $G(2)$ or $G(1) ⊕ G(1)$, which
evaluate to $1$ or $0$, so $G(3) = 2$. Generally,

$$ G(x) = \mathrm{mex}(G(i) ⊕ G(x - i - 1) \mid 0 \leq i \leq x-1) $$

with $G(0)=0$ as a base case. The first few values are:

$$ 0, 0, 1, 2, 0, 1, 2, 3, 1, 2, 3, 4, 0, 3, 4, 2, 1, 3, 2, 1, \dots$$

This sequence appears on the OEIS as
[A046695](https://oeis.org/A046695). It quickly climbs out of the
single digits and then bounces around, mostly in the range $[0, 256]$.


Calculating Naively
-------------------

The above formula is easily turned into a naive, $O(n^2)$ algorithm
for computing values one by one:

```cpp
int main() {
  std::vector<int> values;
  values.push_back(0); // G(0) = 0 base case
  values.push_back(0); // G(1) = 0 base case
  
  int pos = 2;
  while(true) {
    std::unordered_set<int> seen;

    for (int i = 0; i < pos; i++) {
      seen.insert(values[i] ^ values[pos - i - 1]);
    }

    int mex = 0;
    while (seen.count(mex)) mex++;

    values.push_back(mex);
    std::cout << "G(" << pos << ") = " << mex << std::endl;

    pos++;
  }
}
```

There are a couple of basic improvements we can make. Firstly, we only
need to run through around half the loop, because past the midpoint we
are computing the XOR of the same values but swapped.

Secondly, all known values (into the trillions) are less than 512, so
we are better off using a `std::array<bool, 512>` or similar to record
the values that we are applying MEX to. We are willing to take the
gamble that we will never calculate values for long enough to see them
reach 512.

The result is an algorithm that calculates values at around 11K values
per second. (And which slows down as time passes.)


The Rare-Value Algorithm
------------------------

Empirically, some values appear far more often than others. This is
underselling it: there are a handful of values that occur a finite
number of times and then never seem to appear again.^[Notably the
value 0 is one of these, so there are only finitely many (known)
positions that are losing for the first player. These are positions 0,
1, 4, 12, 20, 30, 46, 72, 98, 124, 150, 176, 314 and 408.] There are
exactly $1584$ (known) positions that have "rare" values, the final
one occurring at $G(20627)=277$.

The sequence of rare values (not positions) begins

$$ 0, 1, 6, 7, 10, 11, 12, 13, 16, 17, 22, 23, 26, 27, 28, 29, \dots $$

The pattern here is not so obvious. It turns out that a rare value is
one that has an even number of set bits after throwing out the 0th and
4th bits.

```cpp
bool is_rare(uint16_t x) {
  uint16_t mask = 0b10001;
  return (__builtin_popcount(x & ~mask) % 2) == 0;
}
```

Many octal games have an analogous property that divides rare values
from common ones. There is no rhyme or reason to which bits end up the
relevant ones for this property, they seem to emerge from the chaos of
the initial handful of values. But once a pattern like this is
established it is almost impossible to dislodge, for the following
reason. Because the condition is based on parity, it is not hard to
check the rules:

<div class="align-bug">
\begin{align*}
&\text{common} &&\oplus \,\text{common} &&= \text{rare} \\
&\text{rare} &&\oplus \,\text{rare} &&= \text{rare} \\
&\text{rare} &&\oplus \,\text{common} &&= \text{common}
\end{align*}
</div>

Now consider the collection $\{G(i) ⊕ G(x - i - 1) \mid 0 \leq i \leq
x-1\}$ that we are taking the MEX of to determine $G(x)$.
As $x$ becomes large, the values $G(i) ⊕ G(x - i - 1)$ will be
dominated by those of the form $(\text{common} \oplus \text{common})$,
i.e. rare values, because there are vastly more common values than
rare ones. The first missing value is therefore likely to be common,
because only members of the smaller collection of sums $(\text{rare}
\oplus \text{common})$ are capable of landing on common values.

Knowing that values are likely to be common suggests the following
approach. First, calculate all $(\text{rare} \oplus \text{common})$
sums, and thereby determine the first missing *common* value. This is
overwhelmingly likely to be the correct value in the end, but to
verify this we'll have to check sums not of this form. Here's the key:
we only have to rule out the rare values *below* this missing common
value, and as soon as we've done this we can stop. Usually, the rare
possibilities can be eliminated after checking a small number of
additional sums. No further sums can possibly eliminate that first
missing common value.

Here's how this might look.

```cpp
int main() {
  std::vector<uint16_t> values;
  values.push_back(0); // G(0) = 0 base case
  values.push_back(0); // G(1) = 0 base case

  std::vector<std::pair<uint64_t, uint16_t>> rares;
  rares.push_back({0, 0});
  rares.push_back({1, 0});
  
  uint64_t pos = 2;
  while (true) {
    std::array<bool, 512> seen{};

    // First calculate the sums that result in common values:
    for (auto [rare_pos, rare_value] : rares) {
      uint16_t option = rare_value ^ values[pos - 1 - rare_pos];
      seen[option] = true;
    }

    // Determine the first missing common value:
    uint16_t mex_common = 0;
    while (is_rare(mex_common) || seen[mex_common]) {
      mex_common++;
    }

    // Count how many rare values below that have to be excluded:
    uint16_t missing_rare = 0;
    for (int i = 0; i < mex_common; i++) {
      if (is_rare(i) && !seen[i])
        missing_rare++;
    }

    // Calculate all sums until those rare values are covered:
    for (int i = 0; i < pos; i++) {
      uint16_t option = values[i] ^ values[pos - 1 - i];
      if (option < mex_common && !seen[option]) {
        seen[option] = true;
        missing_rare--;

        if (missing_rare == 0)
          break;
      }
    }

    if (missing_rare == 0) {
      // If we got them all, the result is indeed common:
      
      values.push_back(mex_common);

      std::cout << "G(" << pos << ") = " << mex_common << std::endl;
    } else {
      // Otherwise, we have found a new rare value:
      
      // Calculate the true mex:
      uint16_t mex_rare = 0;
      while (seen[mex_rare])
        mex_rare++;

      values.push_back(mex_rare);
      rares.push_back({pos, mex_rare});

      std::cout << "G(" << pos << ") = " << mex_rare 
                << " New rare!" << std::endl;
    }

    pos++;
  }
}
```

This is a big improvement: we are now generating values at a rate of
50K/s. For now, we're going to put aside the verification step and
focus on calculating the speculative common values as fast as
possible. There is a good parallelisable algorithm for verifying these
common values^[You can do the verification for each value completely
independently, under the assumption that the values that came before
it were correct. Only rarely (i.e. seemingly never) is this assumption
violated, at which point you can redo the later ones that relied on
that incorrect value.], so if our goal is push the calculation further
than Grossman then getting these candidate values as fast as possible
is the whole ballgame.


Values as Bytes
---------------

Grossman observes that, because a) all known values are less than 512
and b) common values obey a parity constraint, we can compress common
values into 8 bits instead of 9. This is a big win: we can store each
value in one byte rather than two. He does this by chopping off the
highest bit and reconstructing it when necessary. Importantly, this
doesn't affect the calculation of the Nim sum: 

$$\mathrm{compress}(x \oplus y) = \mathrm{compress}(x) \oplus \mathrm{compress}(y) $$

Here we can make a minor improvement. The downside of removing the
highest bit is that this doesn't preserve order. The difference has to
be accounted for when calculating the MEX, to avoid picking an
incorrect "smallest value".

Instead, we can cut out the 1st bit and shift all higher bits down by
one. This does preserve order, and so we only ever have to think about
compression when we want to present the output.

```cpp
uint8_t compress(uint16_t x) {
  return ((x >> 1) & ~0b1) | (x & 0b1);
}

uint16_t uncompress(uint8_t x) {
  auto pop = __popc(x & 0b11110110);
  uint16_t bit1 = (pop % 2 == 0) ? 0b10 : 0;
  return ((x & 0b11111110) << 1) | bit1 | (x & 0b1);
}
```

(Or if you like, that line could be `bit1 = (~pop & 1) << 1`.)


Calculating a Block of Values
-----------------------------

The above few sections have just been recapitulating previous work,
now it's time to do something GPU specific.

In CUDA, operations are performed in "blocks" of "threads", with a
block executed collectively on a "streaming multiprocessor".^[I am
going to stop with the scare quotes now.] For hardware reasons, it is
best for a block to have some multiple of 32 threads; a typical number
might be 256. Our plan is to have each thread compute one additional
Officers value. Let's focus on a single block to keep things simple
for now.

Suppose we already know all values at positions smaller than $n$ and
we are now trying to simultaneously compute the values for the range
$[n, n+B)$, where $B$ is the number of threads in our block. Our goal
is to run the first loop of the above algorithm simultaneously over
the block. So that all threads can make progress at the same time, we
want the computation to be as uniform as possible across those
threads. For each rare position $p$, all threads will calculate $G(p)
\oplus G(x - 1 - p)$ for that rare value^[These rare values and their
locations can be stored in `__constant__` memory, luckily it is fast
to broadcast a single value from constant memory to all threads in a
block.], and record the result as ruled out for the MEX. In other
words, at each step we are using a $B$ sized window over the previous
values, whose location is determined by the position of each rare
value. We'll run through the rare positions largest to smallest, so
that the range of previous values we're inspecting moves from to
oldest to newest, with the window starting on the range $[n-M, n-M+B)$
and ending on the range $[n, n+B)$, where $M=20627$ is the position of
the last known rare value.

Of course, we have to be careful: the values in the range $[n, n+B)$
aren't actually known yet, they're what we're calculating right now.
The computation naturally splits into two phases:^[These diagrams may
not be very helpful, but it was satisfying to get TiKZ working on this
page.]

1. **Backwards:** For all threads simultaneously, compute the sums
   involving a rare value and an already-known common value. So for
   the rare value at position $p$, we are reading the window:
   
   ```tikzpicture
\tikzstyle{Interval} = [
  draw, anchor=west,
  inner sep=0,
  outer sep=0,
  minimum height=1cm,
  text=black,
  very thick
]
\node[Interval, minimum width=10cm] (first) {};
\node[Interval, minimum width=4cm, right=0cm of first, dashed] (second) {};
\node[Interval, minimum width=4cm, anchor=center, fill=lightgray] (prev) at (first.center) {};
\draw[-stealth, thick] (second.center) to (prev.center);
\node[below=0.8cm of first.south east, anchor=base] {$n$};
\node[below=0.8cm of first.south west, anchor=base] {$n - M$};
\node[below=0.8cm of second.south east, anchor=base] {$n + B$};
\node[below=0.8cm of prev.south west, anchor=base] {$n - p$};
\node[below=0.8cm of prev.south east, anchor=base] {$n - p + B$};
   ```

   If for a thread the relevant previous value is still unknown, do
   nothing. This happens when the rare value has position $p \in [0,
   B]$ and the current thread $x$ has $x > p$, because then $n + x -
   p - 1 \geq n$ is an unknown position. And so the window is shorter:

   ```tikzpicture
\tikzstyle{Interval} = [
  draw, anchor=west,
  inner sep=0,
  outer sep=0,
  minimum height=1cm,
  text=black,
  very thick
]
\node[Interval, minimum width=10cm] (first) {};
\node[Interval, minimum width=4cm, right=0cm of first, dashed] (second) {};
\node[Interval, minimum width=2cm, anchor=east, fill=lightgray] (prev) at (first.east) {};
\draw[-stealth, thick] (second.center) to (prev.center);
\node[below=0.8cm of first.south east, anchor=base] {$n$};
\node[below=0.8cm of first.south west, anchor=base] {$n - M$};
\node[below=0.8cm of second.south east, anchor=base] {$n + B$};
\node[below=0.8cm of prev.south west, anchor=base] {$n - p$};
   ```

2. **Forwards:** One thread $x \in [0, B)$ at a time, calculate the
   MEX in thread $x$ then announce that value forwards to all the
   threads $> x$ to cover all the cases missed by phase 1.

   ```tikzpicture
\tikzstyle{Interval} = [
  draw, anchor=west,
  inner sep=0,
  outer sep=0,
  minimum height=1cm,
  text=black,
  very thick
]
\node[Interval, minimum width=10cm] (first) {};
\node[Interval, minimum width=2cm, right=0cm of first] (second) {};
\node[Interval, minimum width=2cm, right=0cm of second, dashed] (prev) at (second.east) {};
\draw[-stealth, thick] (prev.center) to (second.east);
\node[below=0.8cm of first.south east, anchor=base] {$n$};
\node[below=0.8cm of first.south west, anchor=base] {$n - M$};
\node[below=0.8cm of prev.south east, anchor=base] {$n + B$};
\node[below=0.8cm of prev.south west, anchor=base] {$n + x$};
   ```

Here's how this looks in code.

```cpp
// The position and value of all (known) rare values
constexpr unsigned RARE_VALUE_COUNT = 1584;
__constant__ uint16_t rare_positions[RARE_VALUE_COUNT];
__constant__ uint8_t rare_values[RARE_VALUE_COUNT];

// The first BLOCK_SIZE values, with common values marked by 0xFF
constexpr unsigned BLOCK_SIZE = 256;
__constant__ uint32_t low_rare[BLOCK_SIZE];

// Initialised with the first SHARED_BUFFER_SIZE positions
__shared__ uint8_t buffer[SHARED_BUFFER_SIZE];

uint32_t mex_array[256 / 32] = {};
  
// The Backward phase:
for (unsigned rare_idx = 0; rare_idx < RARE_VALUE_COUNT; rare_idx++)
{
  uint16_t rare_pos = rare_positions[rare_idx];
  uint8_t rare_value = rare_values[rare_idx];

  if (threadIdx.x < rare_pos+1) {
    uint8_t prev = buffer[(position - 1 - rare_pos) % SHARED_BUFFER_SIZE];
    uint8_t option = prev ^ rare_value;
    set_bit(mex_array, option);
  }
}

// The Forward phase:
for (unsigned done_idx = 0; done_idx < BLOCK_SIZE; done_idx++) {
  // If it's our turn, calculate the MEX
  __shared__ uint8_t final_value;
  if (threadIdx.x == done_idx) {
    final_value = lowest_unset(mex_array);
    buffer[position % SHARED_BUFFER_SIZE] = final_value;
  }
  __syncthreads();

  // Communicate this value to threads computing later positions
  if (threadIdx.x > done_idx) {
    uint8_t rare_value = low_rare[threadIdx.x - done_idx - 1];
    if (rare_value != 0xFF)
      set_bit(mex_array, final_value ^ rare_value);
  }
  __syncthreads();
}
```

With 256 threads this goes at about 470K/s, which is a nice
improvement.


Tracking the MEX in a Thread
----------------------------

Above we used some functions `set_bit` and `lowest_unset`, how do we
implement those operations efficiently? Because each value fits into a
byte, we will have each thread keep track of the values it has seen by
storing a 256-bit mask, and setting single bits as we go.

```cpp
void set_bit(uint32_t array[8], const unsigned i) {
  __builtin_assume(i < (1u<<8));
  array[i/32] |= 1u << (i % 32);
}
```

Because the index `array[i/32]` is not a compile-time constant,
there's a risk that this `array` will end up in local memory rather
than in registers, if the compiler can't figure out this can be
written as an unrolled loop.^[Despite the name, local memory is
actually a thread-specific piece of *global* memory. It's where, for
example, spilled registers get stored.] It seems quite inconsistent,
sometimes the compiler can do this and sometimes not. Just in case,
we'll do the optimisation by hand.

We can manually write the operation as a loop:

```cpp
#pragma unroll 8
for (unsigned j = 0; j < 8; j++) {
  if (j == i/32) {
    array[j] |= 1u << (i % 32);
  }
}
```

This compiles to a straight-line program, using predicated
instructions to operate on the correct entry of the array.

Here's a alternative trick which seems to come out very slightly ahead.

```cpp
#pragma unroll 8
for (unsigned j = 0; j < 8; j++) {
  array[j] |= __funnelshift_lc(0, 1, i - (32 * j));
}
```

We use a "clamped funnel shift" to produce the correct mask for each
array entry directly. For this operation, shifting by more than 32 is
clamped to a shift of exactly 32 rather than wrapping around back to
zero. So, for an array entry `j` that is too small, the 0 value is
fully shifted into the result. And when `j` is too large, the shift
distance underflows and again the 0 is fully shifted in.

At the end of the main loop, the MEX can be computed from the mask by
using the `__ffs` ("find first set") intrinsic to pick out the first 0
bit.

```cpp
int lowest_unset(uint32_t array[8]) {
  for (unsigned i = 0; i < 256/32; i++) {
    if (array[i] != 0xFFFFFFFF) {
      int bit_pos = __ffs(~array[i]) - 1; // __ffs returns 1-based position
      return i * 32 + bit_pos;
    }
  }
  __builtin_unreachable();
}
```


Multiple Blocks
---------------

Of course, we don't want to restrict ourselves to only one block: the
GPU has several multiprocessors that can run blocks simultaneously. We
might have $K$ blocks going at once, so that block 0 is responsible
for computing positions $[n, n+B)$, block 1 is responsible for $[n+B,
n+2B)$, and so on. 

We have to modify the backwards phase, because a block may catch up to
values currently being computed by earlier blocks and have to wait for
that to complete. On a step with rare position $p$, the latest
position in the current block, i.e. $n + B - 1$, will need to read
from the buffer at position $(n + B - 1) - p - 1$. We can be certain
other blocks aren't working on that position as long as it is strictly
less than $n - B(K-1)$. A little rearrangement gives the condition $p
< B K - 1$.

We are safe again once $p < B-1$: all the relevant values are going to be
computed local to the current block in the forward phase.

To make this work, we need to synchronise the blocks somehow. Here I
am going to simply store a value in global memory that records the
overall progress, i.e., up to what position the values are known. Each
thread will also know up to what position its block's shared buffer is
correct, and if a still-unknown value is needed, we'll have the block
spin until that value is available.

The backward phase now looks like:

```cpp
// The Backward phase:
for (unsigned rare_idx = 0; rare_idx < RARE_VALUE_COUNT; rare_idx++)
{
  uint16_t rare_pos = rare_positions[rare_idx];
  uint8_t rare_value = rare_values[rare_idx];

  // Test whether we are in the danger zone:
  if (rare_pos < BLOCK_SIZE * BLOCK_COUNT - 1 && rare_pos > BLOCK_SIZE) {

    // Until we have the value we need:
    while (buffer_correct <=
           block_offset + (BLOCK_SIZE - 1) - 1 - rare_pos) {

      // Wait for the next block to be available:
      if (threadIdx.x == 0) {
        while (*global_progress == buffer_correct) {
        }
      }
      __syncthreads();
  
      // And copy that block of values in:
      buffer[(buffer_correct + threadIdx.x) % SHARED_BUFFER_SIZE] =
        global_buffer[(buffer_correct + threadIdx.x) % GLOBAL_BUFFER_SIZE];
      buffer_correct += BLOCK_SIZE;
    }
    __syncthreads();
  }

  // Now proceed as before:
  if (threadIdx.x < rare_pos+1) {
    uint8_t prev = buffer[(position - 1 - rare_pos) % SHARED_BUFFER_SIZE];
    uint8_t option = prev ^ rare_value;
    set_bit_8(mex_array, option);
  }
}
```

With 8 blocks of size 256, this does 942K/s. Getting better!

Synchronising Properly
----------------------

This code works on my machine and with these particular parameters,
but it possibly shouldn't. The CUDA programming model makes no
guarantees about when blocks get run in relation to each other, so the
strategy I've used here of synchronising via a global variable could
deadlock if for some reason one of the blocks never gets scheduled. 

As far as I can tell there are three official methods for
synchronising across different blocks, even those that are part of the
same kernel:

* Wait for all blocks to run to completion then launch a new
  computation.
* Use `cuda::barrier<cuda::thread_scope_device>`.
* Use the `grid.sync()` method in the Cooperative Groups library.

It would be nice to implement things "properly" and use one of these
methods. The one that fits best is to use `cuda::barrier`.
Unfortunately, this ends up significantly slower than the cheating
method I'm using now, so I'll keep that in my back pocket until
deadlocks are an actual issue. I haven't tracked down exactly why this
is.


The State of Play
-----------------

Everything described above is available at
<https://github.com/mvr/officers>. At the time of writing it
calculates values at 1M/s on my ([dinky little]) machine. This is
pathetic: Grossman's code gets 5M/s on 8 (CPU) threads, and I'm only
getting 1M/s on 4096 (GPU) threads. We should be able to do better.

[dinky little]: https://www.nvidia.com/en-us/autonomous-machines/embedded-systems/jetson-orin/nano-super-developer-kit/

As observed by Grossman, the speed is ultimately limited by how
quickly each block can get through its critical segment, that is, the
part where it computes the MEX. While this is happening, all other
blocks are likely waiting for it to finish. This goes some way (but
not all the way!) to explaining the speed difference: Grossman was
using a 2.66GHz CPU whereas my machine's GPU runs at 900MHz. The
`__ffs` intrinsic used for calculating the MEX is also [more
expensive] than you would expect, with a SM not even able to calculate
an entire warp's worth per cycle.

[more expensive]: https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#arithmetic-instructions

We'll need a better strategy. My idea is to do even more speculation,
allowing each block to compute a MEX for before it's seen all the
relevant values and then correct mistakes later if necessary. This
will complicate things, because any failed prediction could in
principle invalidate all 1584 later values that depend on it. The hope
is that only rarely will this kind of speculation cause a cascade of
corrections to be made, but whether this is the case will have to be
determined experimentally. The corrections could possibly be done on
the CPU, but then we have to hope that they're infrequent enough that
the CPU doesn't become a limiting factor.

A better idea, and what I plan to do next, is to just go all-in on the
awesome power of the GPU and use an algorithm that looks for the fixed
point of this speculation process. On a typical block, the first round
of speculation will yield a handful of incorrect results, because it
was missing some of the relevant previous values. The second round of
speculation will be more correct but still have some errors, and so
on, until there are no more corrections to be made. Key is that each
round no longer has to wait for the block immediately before it to do
its speculation, unlocking a lot more parallelism. Again, it will have
to be determined empirically how many rounds of speculation each block
needs on average before all mistakes are fixed. At *worst* this would
be as fast as the single-block version, because the earliest block
never has to actually do any speculation.

PS. My personal suspicion is that Officers is periodic but with a
pre-period so long that humans will never live to find out what it is.
This is more about the journey than the destination.


References
----------

* Berlekamp, Elwyn R.; Conway, John H.; Guy, Richard K. [Winning Ways for Your Mathematical Plays](https://www.routledge.com/Winning-Ways-for-Your-Mathematical-Plays-Volume-1/Berlekamp-Conway-Guy/p/book/9781568811307). Vol. 1. Second edition. A K Peters, Ltd., Natick, MA, 2001. xx+276 pp. ISBN: 1-56881-130-6
* Caines, Ian; Gates, Carrie; Guy, Richard K.; Nowakowski, Richard J. [Periods in Taking and Splitting Games](https://doi.org/10.2307/2589561). Amer. Math. Monthly 106 (1999), no. 4, 359--361.
* Flammenkamp, A. [Sprague-Grundy Values of Octal-Games](https://wwwhomes.uni-bielefeld.de/achim/octal.html). 2021
* Gangolli, A.; Plambeck, T. [A Note on Periodicity in Some Octal Games](https://link.springer.com/content/pdf/10.1007/BF01254294.pdf). Internat. J. Game Theory 18 (1989), no. 3, 311--320 
* Grossman, J. P. [Searching for periodicity in Officers](https://library.slmath.org/books/Book70/files/1016.pdf). Games of No Chance 5, 373--385, Math. Sci. Res. Inst. Publ., 70, Cambridge Univ. Press, Cambridge, 2019. 
