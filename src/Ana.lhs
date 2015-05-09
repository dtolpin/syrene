$Id: Ana.lhs,v 1.15 2007/05/04 07:33:23 dvd Exp $ 

> module Ana ( domains
>            , expoques
>            , utility
>            , confidence
>            , penalty
>            , testsAna ) where

Analytics on top of synet. 

> import Synet
> import Compute
> import Traverse

> import Control.Monad.State
> import Data.Monoid

> import qualified Test

To every question - an evidence variable - corresponds a set
of possible answers - its domain. Answer (Number 0) corresponds to
no answer, and is not included into explicit value lists.

> type Domain = [Value]

Checkboxes are tri-valued: unknown, false, true.

> checkboxDomain = [Number 1,Number 2]

Freeform questions are mapped to 0 for a missing answer, 1 otherwise.
Since freeform questions are not related to their enabling radio
or check buttons, it makes sense to ignore them.

> ignoreFreeform = True
> freeformDomain = if ignoreFreeform then [Number 0] else [Number 1]

Domains for all evidence variables are collected into an association list.

> type Domains = [(Name,Domain)]

To collect domains, the network must be traversed. The domains are collected 
in the state.

> type DomainState = State Domains ()

> domains net = once $ 
>     execState 
>     (process net :: DomainState)
>     []
>   where once (x@(k,v):xs) = x:once (filter ((/= k) . fst) xs)
>         once [] = []

The only non-void handler is for answers.

> instance Proc Answer DomainState where
>     proc (Radio value) wcxt _ = 
>         do domains <- get
>            let name = cxtQname wcxt
>            let domain = maybe [] id $ lookup name domains
>            put ((name,value:domain):domains)

>     proc (Checkbox name) wcxt _ =
>         do domains <- get
>            put ((cxtQname wcxt,checkboxDomain):domains)

>     proc Freeform wcxt _ =
>         do domains <- get
>            put $ (cxtQname wcxt,freeformDomain):domains

The rest is just skipped, but child actions must be run explicitly.

> instance Proc a DomainState where
>     proc _ _ = sequence_ . map sequence_

= Exposed Questions =

At any point in an interview, conditional parts of sections (expansions)
are collapsed or expanded, and questions they contain are hidden or
exposed.

For every section, referred by its name, there is an expander which, given
the configuration, returns names of exposed questions in the section.

> type Expanders = [(Name,ExpQ)]
> newtype ExpQ = ExpQ [Configuration -> Expanders -> [Name]]
> instance Monoid ExpQ where
>     mempty = ExpQ []
>     mappend (ExpQ a) (ExpQ b) = ExpQ (a++b)

A section may refer to other sections, thus expanding it is performed in the
environment of all section expanders.

> expose cfg env (ExpQ qs) = (concatMap (\f -> f cfg env) qs)

Applied to the root section and then recoursively to sections referred from it,
expansion returns the list of all exposed questions in the questionnaire.

> expoques net cfg =
>     once $ 
>     let Network ses sys exs = net
>         env = map (\se@(Entity name _ _) -> (name,process se)) ses
>     in expose cfg env
>            (case lookup "*" env of
>               Just root -> root
>               Nothing -> missing "se" "*" ExpQ [\_ _ -> []])
>     where once (x:xs) = x:once (filter (/=x) xs)
>           once _ = []

SelectEntry controls which question names are included.

> instance Proc (SelectEntry a) ExpQ where
>     proc (SelectEntry guards entries) wcxt [_,eqs] =
>         ExpQ [\cfg env ->
>                   case lookup (expName wcxt) cfg of
>                     Just v -> if any (allows v) guards 
>                               then concatMap (expose cfg env) eqs
>                               else []
>                     Nothing -> missing "sy" (expName wcxt) []]
>         

Answer holds question name.

> instance Proc Answer ExpQ where
>     proc _ wcxt _ = ExpQ [\_ _ -> [cxtQname wcxt]]

Section reference redirects to the section.

> instance Proc SEntry ExpQ where
>     proc (Section name) _ _ = 
>         ExpQ [\cfg env -> case lookup name env of
>                             Just eq -> expose cfg env eq
>                             Nothing -> missing "se" name []]

Others just concatenate their children:

>     proc _ _ eqss = eqconcat2 eqss
> instance Proc a ExpQ where
>     proc _ _ eqss = eqconcat2 eqss

where

> eqconcat2 :: [[ExpQ]] -> ExpQ
> eqconcat2 = mconcat . map mconcat 

= Metrics =

Utility measures how far the process of answering has advanced in its way
to a sufficient conclusion. It is computed as the sum of all syndrome values.

> utility (Network _ sys _) cfg = 
>     let Number n = sum $
>                    map (\ (Entity name _ _) ->
>                         let Just v = lookup name cfg
>                         in v)
>                        sys
>     in n

Confidence measures confidence of the choice in the configuration.
For a single guard it is the distance from the nearest theshold.

Branches which are always allowed have zero -- neutral -- confidence.

> cfd_guard v SelectAlways = 0

Point guards are non-positive.

> cfd_guard v (SelectEQ x) | v < x =  v-x 
>                          | otherwise = x-v

Range and threshold guards are positive when the likely state is
symptom-forming.

> cfd_guard v (SelectGE x) = v-x
> cfd_guard v (SelectLE x) = x-v
> cfd_guard v (SelectBetween x y) | v < h = v-x
>                                 | otherwise = y-v
>     where h = liftToNumber1 (/2) (x+y)

For a selection entry, the confidence is computed for the most clearly
passed guard. 

> cfd_entry value (SelectEntry guards _) =
>     abs $ maximum $ map (cfd_guard value) guards

Entities consist of constant and conditional parts, and conditional parts
affect confidence.

> cfd_entity cfg (Entity _ _ cs) =
>     sum $ map (cfd_content cfg) cs
> cfd_content _ (Sequence _) = 0
> cfd_content cfg (Expansion name es) = 
>     let value = syval name cfg
>     in sum $ map (cfd_entry value) es

> confidence (Network ses sys exs) cfg = cfd ses + cfd sys + cfd exs
>     where cfd entities = sum $ map (cfd_entity cfg) entities

When a threshold in an unlikely state, the confidence is penalized.

> penalty value se = 2*cfd_entry value se


= Testing =

> testsAna = Test.runSuite []
