$Id: qre.lhs,v 1.11 2006/12/30 22:09:05 dvd Exp $

> module Main where

Questionnaire and database. 

Path info points to questionnaire,
then interview number: qre.cgi/anna/502

   anna/ contains 
      1) form.html (and other support files in the future). 
      2) numerically named data files in the form of CGI query strings
         (anna/502 is for case #502). 

CGI script writes updated data file when -submit-= is in the query and
returns the form with data filled in.

The form is read as XHTML. HaXml is used to parse and update.

> import XML 
> import Text.XML.HaXml
> import Text.XML.HaXml.Xml2Haskell (str2attr, attr2str)

The script works via CGI, and collected data are stored as a raw
query string in a file.

> import CGI
> import System
> import IO
> import Directory(doesFileExist)
> import Monad

A query can be built from CGI HTTP environment:

> http_query = query (hGetContents stdin) getEnv

... or restored from a file:

> file_query file_path path_info = query (h >>= hGetContents) getenv
>     where h = openFile file_path ReadMode
>           getenv "CONTENT_LENGTH" = h >>= (liftM show . hFileSize)
>           getenv "REQUEST_METHOD" = return "POST"
>           getenv "PATH_INFO" = return path_info

A questionnaire is populated with query data when served to the user.

> populate :: String -> String -> Query -> String
> populate sysid src qry = transform populator sysid src
>     where populator =
>               ((elm `with` attr "name") `guards`

Named spans represent value references. Value becomes the only text
child of the span.

>                (tag "span" ?>
>                    (\e@(CElem (Elem n as cs)) ->
>                     [CElem (Elem n as
>                             ((find "name" (literal . fstval qry)) e))])
>
>                 :> tag "input" `guards`

Radio buttons and checkboxes are checked when their value is in the query.

>                    (attrval ("type",str2attr "checkbox") 
>                             |>| attrval ("type",str2attr "radio") ?>
>                        find "name"
>                         (\name ->
>                          (find "value"
>                           (\value -> if elem value (parval qry name)
>                                      then setAttr ("checked",
>                                                    str2attr "checked")
>                                      else remAttr "checked")))

Text and hidden fields keep their values in their value attributes.

>                     :> attrval ("type",str2attr "text") ?>
>                        find "name" 
>                          (\name ->
>                           setAttr ("value",str2attr (fstval qry name)))

Hiddens are used for numeric checkboxes, they are always non-empty.

>                     :> attrval ("type",str2attr "hidden") ?>
>                        find "name" 
>                          (\name ->
>                           case parval qry name of
>                             v:_ -> setAttr ("value",str2attr v)
>                             _ -> keep)
>                     :> none)))
>
>               |>| chip populator

Parameters are often scalar, there is at most one value; no value
is empty string.

>           fstval qry = head . (++[""]) . parval qry

Setting and removing an attribute on an element is common
for checkboxes radio buttons, text and hidden fields.

>           setAttr nv@(name,value) e@(CElem (Elem n as cs)) =
>               [CElem (Elem n
>                       (nv:filter (not.(==name).fst) as)
>                       cs)]
>           remAttr name e@(CElem (Elem n as cs)) =
>               [CElem (Elem n
>                       (filter (not.(==name).fst) as)
>                       cs)]

The main function builds a query and, based on it, updates or loads collected
data and serves the form to the user.

> main :: IO ()
> main = do base_path:_ <- getArgs
>           main' base_path

> main' base_path =
>     do qry <- http_query
>        if null (parval qry "-submit-")
>           then do qry <- load qry
>                   serve qry
>           else do save qry
>                   serve qry
>     where

Load query data from the database.

>       load qry = do let path_info = pathinfo qry
>                         case_path = base_path ++ path_info
>                     case_exists <- doesFileExist case_path
>                     if not case_exists
>                        then (openFile case_path WriteMode >>= hClose)
>                        else return ()
>                     file_query case_path path_info

Save query data in the persistent store.

>       save qry = writeFile (base_path ++ (pathinfo qry))
>                            (querystring qry)

Serve the form to the user,

>       serve qry = do let path_info = pathinfo qry
>                          form_path = base_path
>                                      ++ reverse (dropWhile (/='/')
>                                                  (reverse path_info))
>                                      ++ "/form.html"
>                      form <- readFile form_path
>                      putStr ("Content-Type: text/html; charset=utf-8\n"
>                              ++ "Expires: now\n"
>                              ++ "\n"
>                              ++ (populate path_info form qry))
