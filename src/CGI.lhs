$Id: CGI.lhs,v 1.7 2007/01/14 21:29:59 dvd Exp $

> module CGI( pathinfo
>           , querystring
>           , parameters
>           , Query
>           , query
>           , http_query
>           , file_query
>           , parval 
>           , parseQS
>           , testsCGI ) where

Module for simple CGIs. Does not handle multipart forms.

parseQS is the query string parser. It is exported for applications
which store and parse queries outside of CGI context.

> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec ((<|>))
> import Char(chr,digitToInt,toUpper)
> import List(sort)

> import qualified Test
> import System.IO.Unsafe

Monad, System and IO libraries are required for query instantiation.

> import Monad
> import IO
> import System

A query provides pathinfo -- URL path part after the CGI path, and
parameters -- key-values pairs.

> data Query = Query { pathinfo :: String
>                    , querystring :: String
>                    , parameters :: [(String,[String])] } deriving (Show, Eq)

Utility function to retrieve parameter values.

> parval qry name = case lookup name (parameters qry) of
>                     Just x -> x
>                     Nothing -> []

Query constructor. Queries operate in IO Monad. The constructor is
parameterized by input stream and environment accessor to facilitate
testing and retargetting to FastCGI or queries stored in the file system.

> query :: IO String -> (String -> IO String) -> IO Query
> query input getenv =
>     do request_method <- getenv "REQUEST_METHOD"
>        path_info <- getenv "PATH_INFO"
>        query_string <- case (map toUpper request_method) of
>                          "GET" -> getenv "QUERY_STRING"
>                          "POST" ->
>                              do content_length <- getenv "CONTENT_LENGTH"
>                                 query_string <- input
>                                 return (take (read content_length :: Int)
>                                         query_string)
>        return (Query { pathinfo = path_info
>                      , querystring = query_string
>                      , parameters = (parseQS query_string) })

Frequent sources for a query are CGI HTTP environment:

> http_query = query (hGetContents stdin) getEnv

... or a file in which query parameters where stored:

> file_query file_path path_info = query (h >>= hGetContents) getenv
>     where h = openFile file_path ReadMode
>           getenv "CONTENT_LENGTH" = h >>= (liftM show . hFileSize)
>           getenv "REQUEST_METHOD" = return "POST"
>           getenv "PATH_INFO" = return path_info

Tests for both query methods (GET and POST).

> testQueryGet = Test.resultEQ
>                (unsafePerformIO
>                 (query (return "")
>                        (\name ->
>                             return (case name of
>                                       "PATH_INFO" -> pi
>                                       "QUERY_STRING" -> qs
>                                       "REQUEST_METHOD" -> "GET"))))
>                (Query { pathinfo = pi
>                       , querystring = qs
>                       , parameters = (parseQS qs) })
>                "CGI.query GET"
>     where pi = "/a/b/c"
>           qs = "name=abc&value=d"

> testQueryPost = Test.resultEQ
>                (unsafePerformIO
>                 (query (return qs)
>                        (\name ->
>                             return (case name of
>                                       "PATH_INFO" -> pi
>                                       "CONTENT_LENGTH" -> show (length qs)
>                                       "REQUEST_METHOD" -> "POST"))))
>                (Query { pathinfo = pi
>                       , querystring = qs
>                       , parameters = (parseQS qs) })
>                "CGI.query POST"
>  where pi = "/a/b/c"
>        qs = "name=abc&value=d"


Query string parser parses and groups parameter values.

> parseQS s = let Right pairs = P.parse p_query s s
>             in (List.sort . group) pairs

> testParse = Test.resultEQ (parseQS "a=b&a=d+e&bar=%20")
>               [("a",["b","d e"]),("bar",[" "])]
>               "CGI.parse"

Grammar for the query string (name=value&...)

> p_query = P.sepBy p_param (P.char '&')
>
> p_param = do name <- p_name
>              P.char '='
>              value <- p_value
>              return (name,value)
>
> p_name = P.many p_qrychar
> p_value = P.many p_qrychar

Names and values  must be urldecoded.

> p_qrychar = P.noneOf "&=+%"
>         <|> (P.char '+' >> return ' ')
>         <|> do P.char '%'
>                n <- P.hexDigit
>                m <- P.hexDigit
>                return (chr ((digitToInt n)*16+(digitToInt m)))

Group an association list by keys. Used by the query string parser
to group parameter values into value lists.

> group :: Eq a => [(a,b)] -> [(a,[b])]
> group xs = group' xs []
>     where group' [] ys = ys
>           group' ((k,v):xs) ys
>                      = group' (filter (\(k',_)->k'/=k) xs)
>                        ((k,v:(map snd (filter (\(k',_)->k'==k) xs))):ys)

> testGroup = Test.resultEQ (group [(1,"a"),(2,"b"),(1,"c"),(1,"d"),(2,"e")])
>               [(2,["b","e"]),(1,["a","c","d"])]
>               "CGI.group"

> testsCGI = Test.runSuite [ testQueryGet
>                          , testQueryPost
>                          , testParse
>                          , testGroup]