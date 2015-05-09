$Id: HTML.lhs,v 1.5 2007/04/19 12:35:17 dvd Exp $

> module HTML ( Html(..)
>             , serializer 
>             , testsHTML ) where

= HTML serialization =

Reader and State monads are used for HTML indenting.

> import Monad
> import Control.Monad.Reader
> import Control.Monad.State

> import qualified Test

HTML tree contains elements and text. Attributes are represented as name-value
tuples in an array.

> data Html = HtElem String [(String,String)] [Html]
>           | HtText String
>           | HtRaw String


Html is shown as serialized HTML markup.

> instance Show Html where
>     show elem = runReader (serializer elem) 0

Nested block-level elements are indented.
 
> serializer :: Html -> Reader Int String
> serializer (HtText cdata) = return (escapeCdata cdata)
> serializer (HtRaw cdata) = return cdata
> serializer (HtElem name atts children) =
>     do indent <- ask
>        head <- return
>                ((if inline name
>                  then ""
>                  else "\n"++(take indent (repeat ' ')))
>                 ++"<"++name++(concatMap serializeAttr atts))
>        tail <- case name of
>                _ | (collapse name) && (null children)
>                      -> return "/>"
>                  | (script name) 
>                      -> return 
>                         (">"
>                          ++(let cdata = (concatMap serializeRaw children)
>                             in case cdata of 
>                                  "" -> ""
>                                  _ -> "<!--\n"++cdata++" // -->")
>                          ++"</"++name++">")
>                  | otherwise
>                      -> local (1+)
>                         (do elems <- sequence (map serializer children)
>                             return (">"++(concat elems)++"</"++name++">"))
>        return (head++tail)
>     where inline = (flip elem) ["span", "a", "input"]
>           collapse = (flip elem) ["br", "hr", "link", "input", "option"]
>           script = (flip elem) ["script", "style"]
>           serializeAttr (name,value) =
>               " "++name++"=\""++(escapeAttr value)++"\""
>           serializeRaw (HtText s) = s

Text in character data and attributes is escaped according to similar
but different rules: apostrophe and quotes are escaped in attributes only.

> escapeCdata s = escapeText "<&>" s
> escapeAttr s = escapeText "<&>\"\'" s
> escapeText _ "" = ""
> escapeText esc (c:s)
>     | elem c esc = (escapeChar c)++(escapeText esc s)
>     | otherwise = c:escapeText esc s
>     where escapeChar '<' = "&lt;"
>           escapeChar '&' = "&amp;"
>           escapeChar '>' = "&gt;"
>           escapeChar '\"' = "&quot;"
>           escapeChar '\'' = "&apos;"

= Testing =

> testsHTML = Test.runSuite []