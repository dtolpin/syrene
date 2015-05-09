$Id: idx.lhs,v 1.1 2007/01/16 09:22:39 dvd Exp $

> module Main where

An executable wrapper around Index. It generates a list of cases
along with explanations. Hopefully, GHC is fast enough for the browser
not to timeout.

> import Index
> import System
> import Control.Monad.Error

> main =
>    do [title,bURL,sURL,csv,net] <- liftIO getArgs
>       res <- index title bURL sURL csv net
>       liftIO (putStr ("Content-Type: text/html; charset=utf-8\n"
>                       ++ "Expires: now\n"
>                       ++ "\n"
>                       ++ res))
>    `catch`
>        \err -> liftIO (putStr
>                        ("Content-Type: text/plain; charset=utf-8\n"
>                         ++ "Expires: now\n"
>                         ++ "\n"
>                         ++ (show err)))