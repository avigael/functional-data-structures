# Funtional Programming Data Structures
This project is written in Haskell. To run this project please [install](https://www.haskell.org/downloads/#minimal "Haskell Installation") Haskell on your machine.

In the functional programming style, we usually avoid mutating or updating memory cells, preferring instead pure operations. Accordingly, data structures in functional languages look rather different from the data structures you might be used to.

## AppendList
> Concatenating lists with `(++)` is an expensive operation. It is linear in the length of the first argument, which must be "unravelled" and then added back on one at a time. To improve this, let's consider AppendLists. These use higher-order functions so that appending becomes a constant-time operation. The price is that the other list operations are much less efficient.

**Compilation instructions:**
To run this file with cabal, execute the following command:
```
$ cabal v2-run appendlist
```

## ListContext
> A context takes a plain datastructure (a list, a tree, etc.) and adds an extra bit of information describing a position within the datastructure. This position behaves much like a cursor: we can move the cursor, lookup the value at the cursor, or update/delete the value at the cursor.

**Compilation instructions:**
To run this file with cabal, execute the following command:
```
$ cabal v2-run listcontext
```

## TreeContext
Just like in ListContext, we want to represent a position in a datastructure as the context around the position, and the data at the position. Both of these pieces are more complicated for trees. Let's start with representing contexts in a tree. We will represent contexts recursively: a context of a node is either nothing, if the node is root of the tree, or it is the context of its parent node, the parent node's data, and two lists describing the node's siblings (child nodes of the same parent): the sibling trees before the current node, and sibling trees after the current node.

**Compilation instructions:**
To run this file with cabal, execute the following command:
```
$ cabal v2-run treecontext
```
*Project created for CS538: Theory and Design of Programming Languages (Spring 2020)*