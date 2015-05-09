$Id: qrf.lhs,v 1.5 2007/04/22 14:00:23 dvd Exp $

> module Main where

Questionnaire and database. A version generating html on the fly
from the questionnaire.

Path info points to questionnaire,
then interview number: qre.cgi/anna/502

   anna/ contains 
      1) form.html (and other support files in the future). 
      2) numerically named data files in the form of CGI query strings
         (anna/502 is for case #502). 

> import Parse
> import Form

CGI script writes updated data file when -submit-= is in the query and
returns the form with data filled in.

The script works via CGI, and collected data are stored as a raw
query string in a file.

> import CGI
> import System
> import IO
> import Directory(doesFileExist)
> import Monad

The main function builds a query and, based on it, updates or loads collected
data and serves the form to the user.

> main :: IO ()
> main = do [base_path] <- getArgs
>           qry <- http_query
>           main' base_path qry

> main' base_path qry =
>     if null (parval qry "-submit-")
>     then do qry <- load qry
>             serve qry
>     else do save qry
>             serve qry
>             log qry
>     where
>       casepath qry  = base_path++"data"++(pathinfo qry)
>       netpath qry = base_path++"net"
>                     ++reverse
>                        (tail (dropWhile (/='/') (reverse (pathinfo qry))))
>                     ++ ".synet"

Load query data from the database.

>       load qry = do let path_info = pathinfo qry
>                         case_path = (casepath qry)
>                     case_exists <- doesFileExist case_path
>                     if not case_exists
>                        then (openFile case_path WriteMode >>= hClose)
>                        else return ()
>                     file_query case_path path_info

Save query data in the persistent store.

>       save qry = writeFile (casepath qry) (querystring qry)

Serve the form to the user,

>       serve qry =
>           do let path_info = pathinfo qry
>                  net_path = netpath qry
>              net <- readFile net_path
>              putStr
>               (case network net_path net of
>                  Right network ->  
>                      "Content-Type: text/html; charset=utf-8\n"
>                      ++ "Expires: now\n"
>                      ++ "\n"
>                      ++ (serialize network (parameters qry))
>                  Left e ->
>                      "Content-Type: text/plain; charset=utf-8\n"
>                      ++ "Expires: now\n"
>                      ++ "\n"
>                      ++ (show e))

And log essential information.

>       log qry = hPutStr stderr
>                       $ "syrene:"++(pathinfo qry)
>                         ++" utility="++(head $ parval qry "-utility-")
>                         ++"\n"


