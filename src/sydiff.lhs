$Id: sydiff.lhs,v 1.1 2007/04/25 11:51:19 dvd Exp $

> module Main where

A command-line utility that applies to versions of a network to a list of cases
and prints cases that yield different explanations for the two versions.

> import System
> import IO
> import Control.Monad.Error

> import CGI
> import Synet
> import Parse
> import Form
> import Compute

> main :: IO ()
> main = do oldname:newname:casenames <- getArgs
>           cases <- liftIO (mapM (\c -> file_query c "") casenames)
>           oex <- explainer oldname
>           nex <- explainer newname
>           sequence_
>             $ map (putStrLn . fst)
>             $ filter snd
>             $ zipWith (\n c -> (n,oex c/=nex c)) casenames cases
>        `catch` \err -> liftIO (hPrint stderr err) 
>     where explainer netname = 
>               do nsrc <- liftIO (readFile netname)
>                  network <- liftparse (network netname nsrc)
>                  return (explanation network "*"
>                          . configuration network
>                          . evidence network
>                          . parameters)
>           liftparse = either (fail . show) return


