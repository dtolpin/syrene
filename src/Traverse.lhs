$Id: Traverse.lhs,v 1.6 2007/04/19 09:06:46 dvd Exp $

> module Traverse (WalkContext(..),
>                  cxtName,
>                  cxtSname,
>                  cxtQname,
>                  Walk(..),
>                  Proc(proc),
>                  process ) where

Synet traversal; a generic machinery to process a network.

> import Synet
> import Control.Monad.State

> import qualified Test

Walk encompasses common actions for all traversals, Proc represents
specifics of traversal for a particular purpose. The walk context
holds section and question names.

> data WalkContext = WalkContext {sctName :: Maybe Name,
>                                 qstName :: Maybe Name,
>                                 expName :: Name }
>                    deriving Show
> cxtName name = case name of
>                "*" -> Nothing
>                _ -> Just name

For processing, section and question names are used.

> cxtSname cxt = qname (sctName cxt) Nothing
> cxtQname cxt = qname (sctName cxt) (qstName cxt)

Walk takes a node and returns a stateful action corresponding to
the node.

> class Proc n a => Walk n a where
>     walk :: n -> State WalkContext a
>     walk = idlewalk

The default behavior of walk is to treat a node
atomically and delegate to proc.

> idlewalk n = do cxt <- get
>                 return $ proc n cxt []

Proc takes a node (n), a walk context (WalkContext) and a list
of lists of actions ([[a]]) and returns an action (b) for the node.
The outer list corresponds to multiple slots in a data constructor;
the inner list -- to the list of items in the data tree.

> class Proc n a where
>     proc :: n -> WalkContext -> [[a]] -> a

Walk instances are defined here and shared by all traversals.
They define the context for processing of every node and relations
between nodes.

Proc instances are defined elsewhere and describe the essense of every
particular application.

A network encompasses three branches of entities: sections, syndromes and
explanations.

> instance (Walk (Entity SEntry) a,
>           Walk (Entity YEntry) a,
>           Walk (Entity XEntry) a,
>           Proc Network a)         => Walk Network a where
>     walk n@(Network ses sys exs) =
>         do cxt <- get
>            pses <- mapM walk ses
>            psys <- mapM walk sys
>            pexs <- mapM walk exs
>            return $ proc n cxt [pses,psys,pexs]

An entity introduces a section name context, the section name serves as the
question name prefix.

> instance (Walk (Content b) a,
>           Proc (Entity b) a)  => Walk (Entity b) a where
>     walk e@(Entity name label children) =
>         do cxt <- get
>            put (cxt {sctName = cxtName name, qstName = Nothing})
>            pchis <- mapM walk children
>            put cxt
>            return $ proc e cxt [pchis]

Content consists of sections (which are always included) and expansions
(inclusion is controlled by guards).

> instance (Walk b a,
>           Walk (SelectEntry b) a,
>           Proc (Content b) a) => Walk (Content b) a where
>     walk seq@(Sequence children) =
>         do cxt <- get
>            pchis <- mapM walk children
>            return $ proc seq cxt [pchis]
>     walk exp@(Expansion name ses) =
>         do cxt <- get
>            put (cxt {expName = name})
>            pses <- mapM walk ses
>            return $ proc exp cxt [pses]

Expansion children are selection entires, each entry is a sequence of guards
(under disjunction) followed by a sequence of entries.

> instance (Walk SelectGuard a,
>           Walk b a,
>           Proc (SelectEntry b) a) => Walk (SelectEntry b) a where
>     walk se@(SelectEntry guards children) =
>         do cxt <- get
>            pguards <- mapM walk guards
>            pchis <- mapM walk children
>            return $ proc se cxt [pguards,pchis]

> instance Proc SelectGuard a => Walk SelectGuard a where

> instance (Proc SEntry a,
>           Proc Answer a) => Walk SEntry a where
>     walk q@(Question name) = do cxt <- get
>                                 put (cxt {qstName = cxtName name})
>                                 return $ proc q cxt []

In tables, the row name is added to the section name:

>     walk t@(Table rows cols) =
>         do cxt <- get
>            prows <- mapM makerow rows
>            return $ proc t cxt prows
>         where makerow (name,label) =
>                   do cxt <- get
>                      put (cxt {sctName =
>                                cxtName (cxtQname
>                                         (cxt {qstName = cxtName name})),
>                                qstName = Nothing})
>                      pcells <- mapM (walk . fst) cols
>                      put cxt
>                      return pcells

Answers are just unwrapped.

>     walk a@(Answer a') = do cxt <- get
>                             pans <- walk a'
>                             return $ proc a cxt [[pans]]

The rest is delegated to proc.

>     walk x = idlewalk x

> instance Proc Answer a => Walk Answer a where

Checkbox name becomes the question name.

>     walk a@(Checkbox name) =
>         do cxt <- get
>            return $ proc a (cxt {qstName = cxtName name}) []

The rest is delegated to rpoc.

>     walk x = idlewalk x

Syndrome and explanation entries are processed atomicatlly.

> instance Proc YEntry a => Walk YEntry a where
> instance Proc XEntry a => Walk XEntry a where

To process network, walk over it in the context of its processor.

> process network = evalState (walk network) (WalkContext Nothing Nothing "")

