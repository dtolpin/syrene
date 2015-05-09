$Id: Gene.lhs,v 1.4 2008/08/11 10:30:00 dvd Exp $

Text generation experiments for the Natural Language Processing Course.

> module Gene ( ThemaTree(..)
>             , thematree
>             , themastripped
>             , themamixed
>             ) where
>
> import qualified Test
> import Synet
> import Compute
> import HTML

> import Debug.Trace

Thematic tree is a structured explanation, halfway between the network
and the explanation text.

> data ThemaTree = ThemaExpl String [ThemaTree] | ThemaText String | ThemaRef String String 
 
> -- thematree :: Network -> Name -> Configuration -> ThemaTree
> thematree (Network _ _ explanations) root configuration =
>     let explatrees :: [(Name,[ThemaTree])]
>         explatrees = (map binding explanations)
>         binding :: Entity XEntry -> (Name, [ThemaTree])
>         binding (Entity name label ces) =
>             (name, concatMap (concatMap (map expand) . (entries configuration)) ces)
>         expand :: XEntry -> ThemaTree
>         expand (XText (Plain text)) = ThemaText text
>         expand (XText (Value name)) =
>             ThemaRef name $ maybe (missing "sy" name "") showval (lookup name configuration)
>         expand (Explanation name) = 
>             ThemaExpl name $ maybe (missing "ex" name []) id (lookup name explatrees)
>     in expand (Explanation root)

HTML module is somewhat abused here to serialize ThemaTree to HTML. (show . thematext)
returns a well-formed serialized XML thematic tree.

> themaxml :: (String -> [Html]) -> ThemaTree -> [Html]
> themaxml text2html (ThemaExpl name entries) =
>     [HtElem name [] $ concatMap (themaxml text2html) entries]
> themaxml _ (ThemaRef name value) = [HtElem "ref" [("name", name)] [HtText value]]
> themaxml text2html (ThemaText text) = text2html text

> themastripped = themaxml $ \text -> []
> themamixed = themaxml $ \text -> [HtText text]
