$Id: Sysh.lhs,v 1.35 2007/04/19 09:06:46 dvd Exp $

Commands for Manipulations with Syndrome Networks

> module Sysh where
> 
> import Test (runSuites,testsTest,Results(..))
> import HTML
> import Synet (testsSynet,Network(..))
> import Parse
> import Compute
> import Ana
> import Form
> import Index
> import Graph
> import CGI
> import CSV (testsCSV)

> import Control.Monad.Error

= Embedded Tests for All Modules =

> test = runSuites [testsTest,
>                   testsHTML,
>                   testsSynet,
>                   testsCompute,
>                   testsParse,
>                   testsAna,
>                   testsForm,
>                   testsIndex,
>                   testsGraph,
>                   testsCGI,
>                   testsCSV]

= Network Syntax Check =

> check :: String->IO ()
> check filename = do contents <- readFile filename
>                     putStrLn (case network filename contents of
>                               Right _ -> filename++": OK"
>                               Left e -> (show e))

= Network Serialization =

> writenet :: (Network->String)->String->String->IO ()
> writenet serialize ifilename ofilename =
>     do contents <-readFile ifilename
>        case network ifilename contents of
>          Right network -> writeFile ofilename (serialize network)
>          Left e -> putStrLn (show e)

= HTML Generation =

> makeform :: String->String->IO ()
> makeform = writenet ((flip serialize) [])

= Visualisation =

> makegraph :: String->String->IO ()
> makegraph = writenet visualize

And partial graphs:

> data Subgraph = Sections | Syndromes | Explanations deriving Eq
> makesubgraph parts ifilename ofilename =
>     do contents <-readFile ifilename
>        case network ifilename contents of
>          Right (Network ses sys exs) ->
>            writeFile ofilename
>             (visualize (Network
>                         (if elem Sections parts then ses else [])
>                         (if elem Syndromes parts then sys else [])
>                         (if elem Explanations parts then exs else [])))
>          Left e -> putStrLn (show e)


List of cases from Bugzilla

> makeindex title bURL sURL csv net html =
>    do res <- index title bURL sURL csv net
>       liftIO (writeFile html res)
>    `catch` (liftIO . print)

Index shortcuts for current projects:

> idxanna = makeindex "XEP support cases"
>                     "http://bugzilla.renderx.com:8000/show_bug.cgi?id="
>                     "qrf.cgi/anna/"

= Evidence domain =

> makedomains filename =
>     do contents <- readFile filename
>        putStrLn (case network filename contents of
>                  Right n -> show (domains n)
>                  Left e -> show e)

> makexpoques filename = 
>     do contents <- readFile filename
>        putStrLn (case network filename contents of
>                    Right n -> show (expoques n
>                                     $ configuration n
>                                     $ evidence n [])
>                    Left e -> show e)


> makexxx filename =
>     do contents <- readFile filename
>        putStrLn (case network filename contents of
>                    Right n -> show (configuration n (evidence n []))
>                    Left e -> show e)


