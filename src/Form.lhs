$Id: Form.lhs,v 1.45 2007/05/04 09:28:29 dvd Exp $

> module Form( serialize
>            , evidence
>            , testsForm ) where

= HTML Questionnaire Instance Generator =

A network is internally represented as Synet.Network, consisting
of sections, syndromes and explanations. The questionnaire is derived
from sections, while syndromes and explanations are used to compute
the explanation text accompanying data input.

> import Synet
> import Compute
> import HTML
> import Traverse
> import Ana
> import Acti
> import Alter

> import Debug.Trace

State monad is used for markup generation: section and question names
are in the state.

> import List (nub)
> import Control.Monad.State

> import qualified Test

= Network Representation =

As a side-effect of HTML generation, a full evidence is built.

> data Context = Context {params :: [(String,[String])],
>                         evd :: Configuration}
>                deriving Show
> type FormState = State Context [Html]

Lists of HTML instructions are concatenated for sequences of entities.

> htconcat :: [FormState] -> FormState
> htconcat = liftM concat . sequence

The network corresponds to an HTML page.

> instance Proc Network FormState where
>     proc n@(Network _ sys exs) _ [pses,psys,_] =
>         do htses <- htconcat pses
>            cxt <- get
>            let cfg = configuration n (evd cxt)
>                u = utility n cfg
>            return [HtElem "html" [("xmlns","http://www.w3.org/1999/xhtml")]
>                     [HtElem "head" []
>                       [HtElem "title" []
>                         [HtText (networkTitle n)],
>                        HtElem "link" [("rel","stylesheet"),
>                                       ("type","text/css"),
>                                       ("href","../../form.css")]
>                         [],
>                        HtElem "script" [("type","text/javascript")]
>                         [HtText ("var syndromes = {"
>                                ++(unwordsWith (map enscribe sys) ",")
>                                ++"};")],
>                        HtElem "script" [("type","text/javascript"),
>                                         ("src","../../form.js")]
>                         []],
>                      HtElem "body" []
>                       [HtElem "div" [("class","loading-banner"),
>                                      ("id","loading-banner")]
>                         [HtText "LOADING THE QUESTIONNAIRE... \ 
>                                 \If you see this message for more than \
>                                 \a few seconds, than your browser is \
>                                 \incompatible with JavaScript and/or CSS \
>                                 \used in this page. Browsers known to \
>                                 \work are Firefox and Opera. Microsoft \
>                                 \Internet Explorer 6 and below, as well \
>                                 \as Apple Safari do not work with the page. \
>                                 \Sorry for that."],
>                        HtElem "div" [("class","explanation"),
>                                      ("id","explanation")]
>                         [HtText $ explanation n "*" cfg],
>                        HtElem "div" [("class","explalinks")]
>                          $ map (\(i,(txt,cfd)) ->
>                                    HtElem "a" [("class","explalink"),
>                                                ("href","#")]
>                                     [HtElem "block" [("class","explatext")]
>                                       [HtText txt],
>                                      HtText (show i++" ("++showval cfd++")")])
>                          $ take 3
>                          $ zip [0..]
>                          $ explanations n cfg,
>                        case take 3 $ recommendations n cfg of
>                           [] -> HtElem "div" [("class","collapsed-recommendation"),
>                                               ("id","collapsed-recommendation")]
>                                   [HtElem "span" [("class","utility")]
>                                     [HtText (show $ utility n cfg)]]
>                           rs -> HtElem "div" [("class","recommendation"),
>                                               ("id","recommendation")]
>                                 $ [HtElem "span" [("class","utility")]
>                                     [HtText (show u)]]
>                                 ++ map (\ (Entity name label _,u) ->
>                                        HtElem "p" []
>                                         [HtElem "a" [("href","#"++sectionId name)]
>                                           [HtText label],
>                                           HtRaw "&nbsp;",
>                                           HtElem "span" [("class","score")]
>                                            [HtText (show u)]]) rs,
>                        HtElem "form" [("method","POST")]

Utility is logged. To avoid mounting monads one on top of another, utility
assigned to an HTML form parameter and retrieved on next submit.

>                         [HtElem "input" [("type","hidden"),
>                                          ("name","-utility-"),
>                                          ("value",show u)]
>                                 [],
>                          HtElem "input" [("type","submit"),
>                                          ("name","-submit-")] [],
>                          HtElem "div" [("class","section-reference"),
>                                        ("id","se--soil")]
>                            [HtElem "span" [("class","ref-id")]
>                              [HtText (sectionId "*")]],
>                            HtElem "div" [("class","sections")]
>                              htses]]]]

Network title is the label of the root section, if any; the root section
is named '*'.

> networkTitle (Network ses _ _)
>     = concat [label|Entity name label _ <- ses,name=="*"]

Sections are given identifiers by their names. The identifier of the root
section is a special case because '*' is not a valid identifier character.

> sectionId name = case name of 
>                  "*" -> "se--root"
>                  _ -> "se-"++name

= Syndrome Computation = 

Syndromes are expressed in JavaScript. It is probably possible to generalize
the walker to use Traverse, but that will mean more code without any benefit.

> class Computation a where
>     enscribe :: a -> String

Syndromes is a collection of name->computation pairs. Computation is function
that gets value(name) function as its argument (to retrieve control values).

> instance Computation a => Computation (Entity a) where
>     enscribe (Entity name _ children) =
>         "\""++name++"\":function(value) {var sum = 0, ctl = undefined; "
>         ++(concatMap enscribe children)
>         ++" return sum;}"

Sequences of values are added together.

> sumChildren children = concatMap (\x -> "sum+= "++(enscribe x)++";") children
> unwordsWith [] _ = []
> unwordsWith words sep = foldr1 (\x y -> x++sep++y) words
>
> instance Computation a => Computation (Content a) where
>     enscribe (Sequence children) = sumChildren children

Before every guarded input, ctl is assigned the control variable
value.

>     enscribe (Expansion name ses) =
>         " ctl = value(\""++name++"\"); "++(concatMap enscribe ses)

Addition of optional inputs is controlled by guards.

> instance Computation a => Computation (SelectEntry a) where
>     enscribe (SelectEntry guards children) =
>         "if("++(unwordsWith (map enscribe guards) "||")++") {"
>         ++(sumChildren children)
>         ++"}"

> instance Computation SelectGuard where
>     enscribe SelectAlways = "true"
>     enscribe (SelectEQ x) = "ctl=="++(enscribe x)
>     enscribe (SelectLE x) = "ctl<="++(enscribe x)
>     enscribe (SelectGE x) = "ctl>="++(enscribe x)
>     enscribe (SelectBetween x y) = (enscribe x)++"<=ctl&&ctl<="++(enscribe y)

Weights are either values or syndrome value references.

> instance Computation YEntry where
>     enscribe (Weight value) = enscribe value
>     enscribe (Syndrome name) = show ("value(\""++name++"\")")

And finally, numbers are just numbers, and strings are compared by their
lengths.

> instance Computation Value where
>     enscribe (Number n) = show n
>     enscribe (String s) = show (length s)

= Questionnaire =

The only entity for which HTML is emitted is 'section'. Section with a
title stands out, without one is spliced.

> instance Proc (Entity a) FormState where
>     proc n@(Entity name label children) wcxt [pchs] =
>         do htchs <- htconcat pchs
>            return [HtElem "div" [("id",sectionId name),
>                                  ("class",case label of
>                                             "" -> "simple-section"
>                                             _ -> "section")]
>                    ((case label of
>                        "" -> []
>                        _ -> [HtElem "p" [("class","section-title")]
>                               [HtText label]])
>                     ++ htchs)]

Section lines contain multiple entries, and used in sequences (constant parts)
and expansions (conditional parts of an entity).

> sectionLine pchs = do htchs <- htconcat pchs
>                       return [HtElem "div" [("class","section-line")]
>                               htchs]

Sequences are constant parts.

> instance Proc (Content a) FormState where
>     proc (Sequence _) _ [pchs] = sectionLine pchs

Expansions depend on their control variables in span[@class='ref-name']/text().
JavaScript takes care of expanding and collapsing the divisions. Children are
selection entries.

>     proc (Expansion name _) _ [pchs] = 
>         do htchs <- htconcat pchs
>            return [HtElem "div" [("class","expansion")]
>                     ([HtElem "span" [("class","ref-name")]
>                       [HtText name]]
>                     ++htchs)]
>

Selection entries inclusion is controlled by guards. Entry content
is a section line.

> instance Proc (SelectEntry a) FormState where
>     proc (SelectEntry guards children) _ [pgrds,pchs] =
>         do htgrds <- htconcat pgrds
>            htline <- sectionLine pchs
>            return [HtElem "div" [("class","selection")]
>                     (htgrds++htline)]

SelectGuard is translated to a string representation easy to parse
and interpret in JavaScript.

> instance Proc SelectGuard FormState where
>     proc s _ _ = return [HtElem "span" [("class","guard")]
>                           [HtText 
>                            (case s of
>                               SelectAlways -> "**"
>                               SelectEQ x -> "eq"++(showval x)
>                               SelectLE x -> "le"++(showval x)
>                               SelectGE x -> "ge"++(showval x)
>                               SelectBetween x y ->
>                                   "in"++(showval x)++" "++(showval y))]]

To retrieve parameter value, a function like maybe is needed, except
that empty list and a list of single empty string are nothing,
and the first value from the list is retrieved otherwise.

> maybe1st n f x = case x of 
>                    Nothing -> n
>                    Just [] -> n
>                    Just [""] -> n -- for compatibility with earlier versions
>                    Just x -> (f . head) x

Section lines consist of atomic elements, corresponding to passive text,
value references, entry fields and section references.

> instance Proc SEntry FormState where
>     proc (SText (Plain text)) _ _ = return [HtText text]
>     proc (SText (Value name)) _ _ =
>         do cxt <- get
>            return [HtElem "span" [("class", "value"), ("name",name)]
>                     (maybe1st [] ((:[]) . HtText)
>                      (lookup name (params cxt)))]

Question just updates the current question name, does not generate HTML.

>     proc (Question _) _ _ = return []

Answer is what the actual answer is.

>     proc (Answer _) _ [[pans]] = pans

In tables prows are passed to proc, but rows and cols used
to generate row and column headers.

>     proc (Table rows cols) _ prows =
>         do htrows <- sequence $ map sequence prows
>            return [HtElem "table" [("class","grid")]
>                     ([HtElem "tr" []
>                        ([HtElem "th" [] []]
>                         ++(concatMap makecolhead cols))]
>                      ++(concat
>                         (zipWith makerow
>                          (map makerowhead rows)
>                          htrows)))]
>         where makecolhead (_,label) =
>                   [HtElem "th" [("class","colhead")]
>                     [HtText label]]
>               makerowhead (_,label) =
>                   [HtElem "th" [("class","rowhead")]
>                     [HtText label]] 
>               makecell htans = [HtElem "td" [] htans]
>               makerow head body = 
>                   [HtElem "tr" [] 
>                    (concat (head:map makecell body))]

>     proc (Section name) _ _ = 
>         return [HtElem "div" [("class","section-reference")]
>                  [HtElem "span" [("class","ref-id")]
>                    [HtText (sectionId name)]]]

Answers use walk context for question name. 

> instance Proc Answer FormState where

Checkboxes in HTML have two states; in questionnaires there are three:
  - unknown (0);
  - yes (2);
  - no (1).
This is emulated by a hidden input field paired with the checkbox.

>     proc (Checkbox name) wcxt _ = 
>         do cxt <- get
>            let qname = cxtQname wcxt
>            let value' = maybe1st "0" id (lookup qname (params cxt))
>            put cxt {evd = (qname,Number (read value'::Float))
>                                :evd cxt}
>            return [HtElem "input"
>                     (let attrs = [("type","checkbox"),
>                                   ("name",qname),
>                                   ("value","2")]
>                      in if Number (read value'::Float)==trueNumber
>                         then ("checked","checked"):attrs
>                         else attrs)
>                     [],
>                    HtElem "input"
>                       [("type","hidden"),
>                        ("class","hidcheck"),
>                        ("name",qname),
>                        ("value",value')]
>                     []]

Radio buttons emit 0 if none is selected.

>     proc (Radio value) wcxt _ = 
>         do cxt <- get
>            let qname = cxtQname wcxt
>                value' = maybe1st "0" id (lookup qname (params cxt))
>            put cxt {evd = (qname,Number (read value'::Float))
>                                :evd cxt}
>            return [HtElem "input"
>                       (let attrs = [("type","radio"),
>                                     ("name",qname),
>                                     ("value",showval value)]
>                        in if value==Number (read value'::Float)
>                           then ("checked","checked"):attrs
>                           else attrs)
>                     []]

Freeform entries emit empty string if no input.

>     proc Freeform wcxt _ = 
>         do cxt <- get
>            let qname = cxtQname wcxt
>                value' = maybe1st "" id (lookup qname (params cxt))
>            put cxt {evd = (qname,String value'):evd cxt}
>            return [HtElem "input"
>                       [("type","text"),
>                        ("name",qname),
>                        ("value",value')]
>                     []]


Symptoms and explanations do not generate HTML.

> instance Proc YEntry FormState where
>     proc _ _ _ = return []

> instance Proc XEntry FormState where
>     proc _ _ _ = return [] 

= Wrappers =

Get the full evidence alone, for batch processing.

> evidence network params =
>     evd (execState (process network :: FormState)
>          (Context params []))

Generate an HTML page, filled with evd values.

> serialize network params =
>     "<?xml version=\"1.0\"?>"
>     ++"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\
>       \ \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
>     ++show (head (evalState (process network :: FormState)
>                   (Context params [])))

= Tests =

> testsForm = Test.runSuite []
