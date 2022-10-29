module Syntax_Practice where


{-
When defining a grammar in ASCII text, it is customary to mark terminal symbols with double quotes and use unquoted words for nonterminal symbols. E.g., here is the syntax of exercise 2(b) in ASCII format.

    cond ::= "T" | "not" cond | "(" cond ")"
    stms ::= "skip" | "while" cond "do" "{" stmt "}" | stmt ";" stmt

Using this notation, here is a grammar for the concrete syntax of a language for defining unlabeled directed graphs. Note that 'node' is a nonterminal that stands for natural numbers. Note also the difference between "nodes", which represents a terminal symbol, and the unquoted nodes, which is a nonterminal representing a list of nodes.

    node  ::= 1 | 2 | 3 | 4 | ...
    nodes ::= node | node "," nodes
    edges ::= node "->" nodes | node "->" nodes ";" edges
    graph ::= "nodes" nodes "edges" edges

Here is an example sentence/program in the above syntax to define a particular graph.

    nodes 1,2,3
    edges 1 -> 2,3; 2 -> 1

Define the abstract syntax for the graph language using Haskell (data) types.  Specifically, the nonterminal graph should be represented by the Haskell type Graph. Also use the following type for representing node IDs.

    type Node = Int

Then represent the shown example graph as a value g of type Graph.
-}

type Node = Int

type Graph = ([Node],[(Node,[Node])])

g :: Graph
g = ([1..3],[(1,[2,3]),(2,[1])])



{-
Here is a grammar for defining unlabeled rose trees (also called multiway trees).

    tree ::= "tree" branch
    branch ::= node "{" child* "}"
    child ::= node | branch

Define the abstract syntax for the tree language using Haskell (data) types. Then define the following tree as a Haskell value.

    tree 1{2 3{4 5} 6{7}}
-}

-- Version 1 (distinguish between leaves and branches):
--
data Child = L Node | B Tree1     deriving Show
data Tree1 = Branch1 Node [Child] deriving Show

t1 :: Tree1
t1 = Branch1 1 [L 2,B (Branch1 3 [L 4,L 5]),B (Branch1 6 [L 7])]

-- Version 2 (represent a leaf as a node with empty list of children):
--
data Tree = Branch Node [Tree] deriving Show

leaf n = Branch n []
leaves = map leaf

t :: Tree
t = Branch 1 [leaf 2,Branch 3 (leaves [4,5]),Branch 6 [leaf 7]]
