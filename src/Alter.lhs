$Id: Alter.lhs,v 1.2 2007/05/04 07:33:22 dvd Exp $

> module Alter (alternatives, explanations) where

Alternative explanations.

> import Synet
> import Compute
> import Ana

> import List (sort,sortBy,nubBy)

Alternatives is a list of alternative compound hypotheses represented
as pairs (configuration,confidence). Alternative hypotheses are obtained
by assuming unlikely states for every threshold.

> variations cfg (Entity syname _ cs) = 
>     nubBy (\(v,_) (v',_) -> v'==v) $ sort $ concatMap vars cs
>     where vars (Sequence _) = []
>           vars (Expansion n es) = 
>               let value = syval n cfg
>               in map (var value) es

If the likely state is 'allowed', then the guarded values are subtracted in
a variation, otherwise added.

>           var v g@(SelectEntry gs vs) = 
>               ((if any (allows v) gs then (-)
>                 else (+))
>                   (syval syname cfg)
>                   (sum $ map (\e ->
>                                   case e of 
>                                     Weight v -> v
>                                     Syndrome n -> syval n cfg)
>                               vs),
>                penalty v g)

It is probably possible to vary on thresholds in all three kinds of entities,
but I have reservations about it. Currently, only thresholds internal to syndromes
are varied.
    
> alternatives net@(Network _ sys _) cfg0 =
>     let evd = filter (\(n,_) -> elem n $ map fst $ domains net) cfg0
>         alt n (v,p) = let cfg = configuration net ((n,v):evd)
>                         in (cfg,confidence net cfg - p)
>         cfd0 = confidence net cfg0
>     in takeWhile (\ (_, cfd) -> cfd >= cfd0) 
>        $ sortBy (\(_,ca) (_,cb) -> compare cb ca) 
>        $ concatMap 
>              (\sy@(Entity name _ _) -> map (alt name) $ variations cfg0 sy)
>              sys

Distinct explanations with their confidences.

> explanations net cfg0 = 
>     let cfd0 = confidence net cfg0
>     in nubBy (\ (txt,_) (txt',_) -> txt==txt') 
>        $ (explanation net "*" cfg0, 0)
>          : map (\ (cfg, cfd) -> ( explanation net "*" cfg, cfd-cfd0))
>               (alternatives net cfg0)
