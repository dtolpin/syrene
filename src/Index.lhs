$Id: Index.lhs,v 1.3 2007/04/06 13:44:02 dvd Exp $

> module Index (index,testsIndex) where

Index composer for a Syrene module.

> import CSV
> import Synet
> import Parse
> import Compute
> import Form
> import HTML
> import CGI
> import Test

> import Monad
> import Control.Monad.Error
> import IO
> import Directory

Every case in the list contains links to bugzilla and syrene records
and displays the case's explanation. 

> index title bURL sURL csvfname netfname =
>     do sc <- liftIO (readFile csvfname)
>        sn <- liftIO (readFile netfname)
>        cases <- liftparse (parseCSV sc)
>        network <- liftparse (network netfname sn)
>        caserows <- liftIO (sequence (map (markup network) (tail cases)))
>        return (show
>                (HtElem "html" [("xmlns","http://www.w3.org/1999/xhtml")]
>                  [HtElem "head" []
>                    [HtElem "title" []
>                      [HtText ("SYRENE: "++title)],
>                     HtElem "link" [("rel","stylesheet"),
>                                    ("type","text/css"),
>                                    ("href","index.css")]
>                      [],
>                     HtElem "script" [("type","text/javascript"),
>                                      ("src","index.js")]
>                      []],
>                   HtElem "body" []
>                    [HtElem "div" [("class","explanation"),
>                                   ("id","explanation")] [],
>                     HtElem "table" []
>                      ([HtElem "tr" []
>                        [HtElem "th" []
>                          [HtText "Case#"],
>                         HtElem "th" []
>                          [HtText "Case Title"],
>                         HtElem "th" []
>                          [HtText "Bugzilla"],
>                         HtElem "th" []
>                          [HtText "Syrene"]]]
>                       ++caserows)]]))

>   where

Since instance Error String is defined and instance Error ParseError is not,
ParseError is flattened to string.

>     liftparse = either (fail . show) return

Explanations are embedded into rows.

>     markup network (id:severity:priority:platform:
>                     assignee:status:resolution:desc:_) =
>       do explanation <- explain_case network (case_dir++id)
>          return (HtElem "tr" []
>                   [HtElem "td" []
>                     [HtText id,
>                       HtElem "block" [("class","explatext")]
>                        [HtText explanation]],
>                     HtElem "td" []
>                      [HtText desc],
>                     HtElem "td" []
>                      [HtElem "a" [("href",bURL++id)]
>                        [HtText "B"]],
>                     HtElem "td" []
>                      [HtElem "a" [("href",sURL++id)]
>                         [HtText "S"]]])
>     case_dir = reverse (dropWhile (/='/') (reverse csvfname))

Evidences are retrieved from queries stored in files, the file name 
is the case number; the directory is the same as for the case list
in CSV. If a case is not yet entered, than the explanation is empty.

> explain_case network@(Network ses sys exs) case_path = 
>     do entered <- doesFileExist case_path
>        if entered 
>           then liftM (explanation network "*"
>                       . configuration network
>                       . evidence network
>                       . parameters)
>                      (file_query case_path "")
>           else return ""


> testsIndex = runSuite []