$Id: thematree.lhs,v 1.2 2008/08/11 10:30:00 dvd Exp $

> module Main where

A command-line utility to generate a thematic subtree for the case. The arguments
are network, case, explanation root.


> import System
> import IO
> import Control.Monad.Error

> import CGI
> import Synet
> import Parse
> import Form
> import Compute
> import Gene

> main :: IO ()
> main = do netname:casename:root:[] <- getArgs
>           netsrc <- liftIO (readFile netname)
>           net <- liftparse $ network netname netsrc
>           cas <- liftIO (file_query casename "")
>           liftIO $ ( putStrLn
>                      . show 
>                      . head
>                      . themastripped
>                      . thematree net root 
>                      . configuration net
>                      . evidence net
>                      . parameters) cas
>        `catch` \err -> liftIO (hPrint stderr err) 
>     where liftparse = either (fail . show) return


