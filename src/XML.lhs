$Id: XML.lhs,v 1.5 2006/12/29 00:18:15 dvd Exp $

> module XML (transform) where

XML and XHTML are used for questionnaire (as well as possibly
for other things). 

> import Text.XML.HaXml
> import Text.XML.HaXml.Pretty (document)
> import Text.PrettyPrint.HughesPJ (renderStyle,style,mode,Mode(..))

A common action is to read XML, transform it, and write to another file.
It just fails if the transformation result is not well-formed.

> transform transformation sysid content = 
>     (renderStyle (style {mode = OneLineMode})
>      . document . applytrans) (xmlParse sysid content)
>     where applytrans d = let Document p s e m = d
>                          in Document p s 
>                                 (let CElem e' = (head . transformation
>                                                  . CElem) e
>                                  in e') m
