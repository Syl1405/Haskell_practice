module Syntax where

--
-- Exercise 1
--

-- (a)
{-
We need three data types to represent the nonterminals cmd, mode, and
pos. Since the nonterminals pars and vals define lists of names and
nums, respectively, we can use Haskell lists to represent those.

Instead of "Sequ Cmd Cmd", we could also use "Sequ [Cmd]".
-}

data Cmd = Pen Mode
          | MoveTo Pos Pos
          | Def  String [String] Cmd
          | Call String [Int]
          | Sequ Cmd Cmd

data Mode = Up | Down

data Pos = I Int | Var String

{-
One could also represent the lists using additional data types for the
nonterminals pars and vals.

data Pars = Par String Pars | NoPars
data Vals = Val Int Vals | NoVals

In that case the definition of Cmd has to be changed as follows.

data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def  String Pars Cmd
         | Call String Vals
         | Sequ Cmd Cmd

However, the first definition is definitely better for two reasons:
First, it saves the definition of additional data types, and second,
it allows the reuse of list operations when manipulating parameter and
value lists in abstract syntax trees.
-}

-- (b)
{-
The macro definition in concrete Mini Logo syntax looks as follows.

  def vector (x1,y1,x2,y2) pen up; moveto (x1,y1); pen down; moveto (x2,y2)

The abstract syntax is represented as a value of the data type Cmd.
-}

vector :: Cmd
vector = Def "vector" ["x1","y1","x2","y2"]
          (Sequ (Pen Up)
                (Sequ (MoveTo (Var "x1") (Var "y1"))
                      (Sequ (Pen Down)
                            (MoveTo (Var "x2") (Var "y2")) )))

{-
A representation that uses the explicit data type representation for
the nonterminals pars and vals looks as follows.

vector :: Cmd
vector = Def "vector"
         (Par "x1" (Par "y1" (Par "x2" (Par "y2" NoPar))))
         (Sequ (Pen Up)
               (Sequ (MoveTo (Var "x1") (Var "y1"))
                     (Sequ (Pen Down)
                           (MoveTo (Var "x2") (Var "y2")) )))

Writing nested applications of Sequ (and Par, etc.) can get quite
tedious. Here are two alternatives that show how to simplify writing
syntax trees.

First, any binary function or constructor can be written using infix
notation by enclosing it in backquotes. E.g.  instead of div 7 3 we
can write 7 `div` 3. Since Sequ is a binary constructor, we can use
this notation to write a sequence of Cmds in a way that is closer to
the concrete syntax.

vector :: Cmd
vector = Def "vector" ["x1","y1","x2","y2"]
          (Pen Up                       `Sequ`
           MoveTo (Var "x1") (Var "y1") `Sequ`
           Pen Down                     `Sequ`
           MoveTo (Var "x2") (Var "y2"))

A representation that uses the explicit data type representation for
the nonterminals pars and vals looks as follows.

vector :: Cmd
vector = Def "vector" ("x1" `Par` "y1" `Par` "x2" `Par` "y2" `Par` NoPar)
          (Pen Up                       `Sequ`
           MoveTo (Var "x1") (Var "y1") `Sequ`
           Pen Down                     `Sequ`
           MoveTo (Var "x2") (Var "y2"))

Another alternative is to put all commands into a Haskell list and
use a function to convert this list into a tree built from Sequ
constructors.

vector :: Cmd
vector = Def "vector" ["x1","y1","x2","y2"]
          (foldr1 Sequ [Pen Up,  MoveTo (Var "x1") (Var "y1"),
                        Pen Down,MoveTo (Var "x2") (Var "y2")])

The Haskell function foldr1 takes a binary operation (the constructor
Sequ in this case) and a list and combines all the elements of the list
with this operation.
-}


-- (c)
{-
Here is a solution that creates the command sequences for the topmost
step and connects it (using Sequ) to the command sequence for the
remaining stair.
-}
steps :: Int -> Cmd
steps 1 = foldr1 Sequ [Pen Up,  MoveTo (I 0) (I 0),
                       Pen Down,MoveTo (I 0) (I 1),
                                MoveTo (I 1) (I 1)]
steps n = Sequ (steps (n-1))
               (foldr1 Sequ [MoveTo (I (n-1)) (I n),
                             MoveTo (I n)     (I n)])


--
-- Exercise 2
--

-- (a)
type NT   = String
type Term = String

type Grammar = [Prod]
type Prod    = (NT,[RHS])
type RHS     = [Symbol]
data Symbol  = N NT | T Term
               deriving Show


-- (b)
cond :: Prod
cond = ("cond",[[T "T"],
                [T "not",N "cond"],
                [T "(",N "cond",T ")"]])

stmt :: Prod
stmt = ("stmt",[[T "skip"],
                [T "while",N "cond",T "do", T "{", N "stmt", T "}"],
                [N "stmt",T ";",N "stmt"]])

imp :: Grammar
imp = [cond,stmt]


-- (c)
nonterminals :: Grammar -> [NT]
-- nonterminals g = [nt | (nt,_) <- g]
nonterminals = map fst

terminals :: Grammar -> [Term]
terminals g = [s | (_,rhss) <- g, rhs <- rhss, T s <- rhs ]
