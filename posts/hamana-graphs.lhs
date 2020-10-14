---
title: Hamana's Graph GADT
published: February 10, 2018
tags: note-to-self, code
---

Hamana describes a neat way of encoding general graphs as a GADT in
[Initial Algebra Semantics for Cyclic Sharing Tree
Structures](https://lmcs.episciences.org/1060), a nice trick that I
hadn't seen before. The idea is that the `Graph` data type will have a
`Ptr` constructor that allows us to add an edge reference other parts
of the structure. The `Graph` type will be indexed by a 'context' that
specifies what `Ptr`s are valid.

Elements of `Graph` won't correspond to general graphs exactly. We are
actually describing graphs that are "rooted, connected, directed and
edge-ordered with each node having out-degree 2". It is possible to
choose different roots and orderings for the same graph, and these
choices will lead to different representations as elements of
`Graph`. The problem of determining whether two elements of `Graph`
have the same underlying actual graph seems like it would be difficult
in general.


> {-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE GADTs #-}

> module Graphs where

First, let us define a type describing the possible shapes of our data
structure. We will be using this at the type-level via DataKinds. Our
`Graph`s all have underlying binary trees.

> data TreeShape where
>   LeafShape    :: TreeShape
>   BinShape     :: TreeShape -> TreeShape -> TreeShape

There are also special leaves that are `Ptr`s to other locations.

>   PtrShape     :: TreeShape

And finally, we will need a way of blocking off a section of a graph
so that a `Ptr` can't refer to it.

>   VoidShape    :: TreeShape

This won't occur as the shape of an actual graph.

The next piece is an indexed type that, given a `TreeShape`, picks
out the location of a node in that shape. These will be what we use to
specify where in a tree a `Ptr` is pointing.

> data TreePosition (shape :: TreeShape) where
>   LeafPos  :: TreePosition LeafShape
>   BinPos   :: TreePosition (BinShape s t)
>   LeftPos  :: TreePosition s -> TreePosition (BinShape s s')
>   RightPos :: TreePosition s' -> TreePosition (BinShape s s')

There is, of course, no constructor for `TreePosition VoidShape`. We
also don't allow `TreePosition PtrShape`, which means that `Ptr`s
will be required to point to genuine nodes in the tree, and not other
`Ptr`s. This avoids there being multiple ways of specifying the same
tree via chains of pointers, rather than having each `Ptr` target the
eventual node at the end of the chain.

We'll need to be able to index type-level lists, so let's get that out
of the way:

> data Elem :: k -> [k] -> * where 
>   Here :: Elem s (s : c)
>   There :: Elem s c -> Elem s (t : c)

Now for the actual graph type. The type is indexed both by the `shape`
of the underlying tree, and a `context`, a list of `TreeShape`s. As we
build a tree, this list is collecting the shapes of all the subtrees
used as a left branch of an earlier binary node.

Consider the following tree:

                 a
                / \
               /   \
              a     b 
             / \   / \
            a   a b   *
                   \
                    b

If we are currently defining the subtree to be placed at `*`, then the
`context` will consist of the two shapes

                          a
             b           / \
            / \         /   \
      [    b   0  ,    a     0   ]
            \         / \   
             b       a   a 

In the subtree to be placed at `*`, we may have pointers to `a`s or `b`s,
but not the `0`s.

> data Graph (context :: [TreeShape]) (shape :: TreeShape) where
>   Leaf :: Graph c LeafShape

Leaves are easy, we can have a leaf in any context.

>   Ptr :: Elem s context -> TreePosition s -> Graph context PtrShape

For the pointers, we pick a shape from the context and then a position
in that shape.

>   Bin :: Graph (BinShape VoidShape VoidShape : c) s 
>       -> Graph (BinShape s VoidShape : c) t 
>       -> Graph c (BinShape s t)

The interesting case is a binary node. In the left and right branches,
we push new trees onto the context c. When defining the left branch,
we only have access to one additional node over what is already in
`c`: the binary node we are currently defining. In the right branch,
we have access to the entire left branch: note that the shape `s` of
the left branch is what is pushed onto the context. This is why this
type has to be indexed over the shape of the resulting graph, not just
the context.

And that's it! We can add labels to the nodes in the graph if we like,
by adding a parameter to the `Leaf` and `Bin` constructors.

Graphs of shape `s` then correspond to elements of `Graph '[] s`. If
we don't care about having the shape as part of the type, we can
existentially quantify it.

> data AnyGraph = forall s. AnyGraph (Graph '[] s)

Here are some simple graphs:

> graph1 :: AnyGraph
> graph1 = AnyGraph $ Bin (Bin Leaf Leaf) (Bin (Ptr (There $ Here) (LeftPos $ LeftPos $ LeafPos)) Leaf)

Corresponding to

         *
        / \
       /   \
      *     *
     / \   / \
    *   * /   * 
     \   /
      ---

We can modify what kinds of graphs are representable by changing the
way the `context` is extended.  Note that we can currently have cycles
in the graph:

> graph2 :: AnyGraph
> graph2 = AnyGraph $ Bin Leaf (Ptr Here BinPos)

corresponds to

        --- 
       /   \
      *    /
     / \  /
    *   --

It's not hard to change this though: 

> data DAG (context :: [TreeShape]) (shape :: TreeShape) where
>   DAGLeaf :: a -> DAG c LeafShape
>   DAGPtr :: Elem s context -> TreePosition s -> DAG context PtrShape
>   DAGBin :: DAG c s -> DAG (s : c) t -> DAG c (BinShape s t)

The only difference is in the `DAGBin` constructor. Here, when
constructing the left branch, we are no longer permitted to reference
the current binary node. The same is true when constructing the right
branch, but we still give it access to the left branch. The result is
that every `Ptr` points to something to the left, other than a direct
ancestor of the current node. That is enough to rule out any cycles.

We have no need for `VoidShape` here, which is nice!
