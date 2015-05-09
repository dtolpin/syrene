$Id: Acti.lhs,v 1.8 2007/05/03 22:17:56 dvd Exp $

> module Acti (recommendations) where

= Active Dialogue =

> import Synet 
> import Compute
> import Ana
> import Traverse

> import List (sortBy, (\\))
> import Data.Monoid
> import Control.Monad.State

> import qualified Test
> import Debug.Trace

== Overview ==

In the active dialogue, SYRENE recommends answering questions that have
the greatest utility at the current state.

When answering a question exposes additional questions,
the newly exposed questions are included into utility computation.

Two measures of the utility increase can be computed: peak and mean.
The peak increase attracts attention to infrequent important cases;
the mean one points to the most efficient dialogue scenario on average.

== Referring to questions ==

Since a question does not have a name or location and consists of one or more
answers belonging to the same section, the recommendation is given with regard
to a section that holds the question, not to a particular question in it.

A mapping from questions to holding sections is built by traversing the
network.

> newtype QueLocs = QueLocs [(Name,Entity SEntry)]
> type QueLocState = State (Entity SEntry) QueLocs
> instance Monoid QueLocs where
>     mempty = QueLocs []
>     mappend (QueLocs a) (QueLocs b) = QueLocs (a++b)

Sections bind the section name.

> instance Proc (Entity SEntry) QueLocState where
>     proc se wcxt ps = put se >> qlconcat ps

Answers provide association between questions and sections.

> instance Proc Answer QueLocState where
>     proc _ wcxt _ = 
>         do se <- get
>            return $ QueLocs [(cxtQname wcxt,se)]

All other nodes just concatenate their children.

> instance Proc a QueLocState where
>     proc _ _ = qlconcat
>
> qlconcat :: [[QueLocState]] -> QueLocState
> qlconcat = liftM (mconcat . map mconcat) . sequence . map sequence

Only labeled sections are recommended; labelless are in-place expansions
and do not participate. 

> qst2sct net@(Network ses _ _) qname =
>     let QueLocs qls = evalState (process net::QueLocState) (Entity "" "" [])
>         Just se = lookup qname qls
>     in se

== Building list of recommendations ==

Recommendations is a list of questions-utility pairs ordered by
utility.

> recommendations :: Network -> Configuration -> [(Section,Float)]
> recommendations net cfg0 = 
>     let ds = (domains net)
>         evidence cfg = filter (\(n,_) -> elem n $ map fst ds) cfg

Moves at every point of conversation are answers to unanswered questions.

>         moves :: Configuration -> [(Name,[Value])]
>         moves cfg = 
>             let exposed = expoques net cfg
>             in filter (\ (n,_) -> let Just v = lookup n cfg
>                                   in elem n exposed && v==0) ds

Utility is averaged on answers and on uncovered questions (or should
the latter be sum?)

>         avg :: [Float] -> Float
>         avg [] = 0
>         avg vs = sum vs / (fromIntegral . length) vs

Moved utility is called recursively when it uncovers new questions.

>         sumChildren = False

>         utl cfg m@(q,as) ms =
>             let evd = evidence cfg
>             in avg $
>                map (\a -> 
>                         let cfg' = configuration net ((q,a):evd)
>                             ms' = moves cfg'
>                             dms = ms' \\ ms
>                         in (utility net cfg')
>                                + ((if sumChildren then sum
>                                    else avg)
>                                  $ map (\m -> utl cfg' m ms') dms))
>                    as

And finally, the recommendations are computed for moves in the
current configuration. For convenience, the utility is relative
to the current one.

>         u0 = utility net cfg0 
>         ms0 = moves cfg0
>         labeled (Entity _ label _,_) = length label > 0
>         uniq [] = []
>         uniq (r:rs) = r:(uniq $ 
>                          let (Entity name _ _,_) = r
>                          in filter (\ (Entity name' _ _,_) ->
>                                     name'/=name) rs)
>         q2s = qst2sct net
>     in uniq
>        $ sortBy (\(_,a) (_,b) -> compare b a)
>        $ filter labeled
>        $ filter ((>0) . snd)
>        $ map (\m@(q,_) -> (q2s q,utl cfg0 m ms0 - u0))
>        $ ms0

> testsActi = Test.runSuite []

