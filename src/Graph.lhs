$Id: Graph.lhs,v 1.4 2007/02/06 12:42:43 dvd Exp $

> module Graph (visualize, testsGraph) where

Graph representation of the syndrome network (via Graphviz).

> import Synet
> import qualified Test

State will keep current context to draw edges.

> import Control.Monad.State

Class Dot defines a single method, draw, responsible for drawing
a dot code fragment corresponding to the instance.

> class Dot s a where
>     draw :: a -> State s String

The state holds scope name and prefix.

> data Scope = Scope { name :: String, pfx :: String, links :: [String] }
> sePfx = "section."
> syPfx = ""
> exPfx = "explanation."

Network constituents are Dot instances. Each of the entities gets
its own shape.

> instance Dot Scope Network where
>     draw (Network ses sys exs) =
>         do scope <- get
>            put scope {pfx=sePfx}
>            gses <- concatMapM draw ses
>            put scope {pfx=exPfx}
>            gexs <- concatMapM draw exs
>            put scope {pfx=syPfx}
>            gsys <- concatMapM draw sys
>            return ("digraph \"SYRENE: network graph\" {rankdir=\"RL\";\n\
>                    \  size=\"8,10\"\n\
>                    \  edge [arrowhead=\"dot\", arrowtail=\"odot\"];\n\n"
>                    ++(declare ses "diamond" sePfx)
>                    ++(declare exs "box" exPfx)
>                    ++(declare sys "ellipse" syPfx)
>                    ++gses
>                    ++gexs
>                    ++gsys
>                    ++"}\n")
>       where declare ents shape pfx =
>                 "  node [shape=\""++shape++"\"];\n"
>                 ++(concatMap decl ents)
>                 ++"\n"
>                 where decl (Entity name label _) =
>                           "  \""++pfx++name++"\" [label=\""++name++"\"];\n"

> instance Dot Scope a => Dot Scope (Entity a) where
>     draw (Entity from _ children) =
>         do scope <- get
>            put scope {name=from, links = []}
>            concatMapM draw children

Links are drawn from entities to syndromes used in and entities
reference from them.

> drawLink to = 
>     do scope <- get
>        if elem to (links scope)
>           then return ""
>           else do put scope {links=to:links scope}
>                   return ("  \""++(pfx scope)++(name scope)
>                           ++"\"->\""++to++"\";\n")

> instance Dot Scope a => Dot Scope (Content a) where
>     draw (Sequence children) = concatMapM draw children
>     draw (Expansion name ses) = liftM2 (++) (drawLink (syPfx++name))
>                                             (concatMapM draw ses)
>
> instance Dot Scope a => Dot Scope (SelectEntry a) where
>     draw (SelectEntry _ children) = concatMapM draw children

Syndromes can be used in both left- and right-hand side of a syndrome expansion.

> instance Dot Scope YEntry where
>     draw (Weight w) = draw w
>     draw (Syndrome name) = drawLink (syPfx++name)
>
> instance Dot Scope Value where
>     draw _ = return ""
>
> instance Dot Scope SEntry where
>     draw (Section name) = drawLink (sePfx++name)
>     draw _ = return ""
>
> instance Dot Scope XEntry where
>     draw (Explanation name) = drawLink (exPfx++name)
>     draw _ = return ""

> visualize network = evalState (draw network) (Scope "" "" [])

= Testing =

> testsGraph = Test.runSuite []