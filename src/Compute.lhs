$Id: Compute.lhs,v 1.27 2008/08/03 11:59:09 dvd Exp $

Syndrome network runtime in Haskell: compute syndromes and
generate explanations.

> module Compute ( Configuration
>                , syval
>                , entries
>                , configuration
>                , explanation
>                , testsCompute ) where
>
> import qualified Test
> import Synet

> import Debug.Trace

To run selectors, compile their instructions into Haskell code.

> select ses v = map sel ses
>     where sel (SelectEntry guards entities) = 
>               if any (allows v) guards then entities
>               else []

Syndrome values are stored in configuration and computed lazily.

> syval name cfg = maybe (missing "sy" name (Number 0)) id (lookup name cfg)

Entries are retrieved from expansions and spliced into a list.

> type Configuration = [(Name,Value)]
> entries cfg (Sequence es) = [es]
> entries cfg (Expansion name selector) =
>     select selector (syval name cfg)

Questionnaire entries are formatted one by one to produce interactive
questionnaire; syndromes and explanations are computed as scalar values.
Explanations depend on values of all syndromes, stored in an list
of bindings -- the configuration.

> configuration :: Network -> Configuration -> Configuration
> configuration (Network _ syndromes _) evidence =
>   let configuration = (map binding . filter free) syndromes
>                       ++ evidence
>       binding (Entity name _ ces) =
>           (name, foldr (+) (Number 0)
>                    (map weight 
>                     (concatMap (concat . entries configuration) ces)))

Let's allow pinned syndromes buy adding them to the evidence and
excluding from the computation.

>       free (Entity name _ _) = case lookup name evidence of
>                                  Nothing -> True
>                                  Just _ -> False

Weight is either a constant or a variable's (syndrome or evidence variable)
value.

>       weight (Weight w) = w
>       weight (Syndrome name) = syval name configuration
>   in configuration

Explanation is a text string, merged from subexplanations.

> explanation :: Network -> Name -> Configuration -> String
> explanation (Network _ _ explanations) root configuration =
>     let explatexts = (map binding explanations)
>         binding (Entity name label ces) =
>             (name,
>              let body = foldr joinends ""
>                         (concatMap
>                           (map (concatMap expand)
>                            . entries configuration)
>                          ces)

When there is a non-empty label, then the entity is formatted as a block,
separated from preceding and following content (as well as the body
from the title) by empty lines.

>                in case label of
>                     "" -> body
>                     _ -> foldr joinends ""
>                                ["\n\n= ",label," =\n\n",body,"\n\n"])
>         expand (XText (Plain text)) = text
>         expand (XText (Value name)) =
>             maybe (missing "sy" name "") showval (lookup name configuration)
>         expand (Explanation name) =
>             maybe (missing "ex" name "") id (lookup name explatexts)
>     in reflow (expand (Explanation root)) measure
>   where measure = 65536 -- no reflowing due to lack of UTF support

joinends joins explanation fragments, taking care of punctuation.
On joints, latter punctuation replaces former one, no spaces between
text and punctuation, otherwise a single space. When punctuation
is followed by '<<' on a joint, '<<' deletes the punctuation. In any
case, << in the beginning of a line disappears.

> joinends hs ts = niojends (reverse hs) ts
>     where niojends "" ts = ts
>           niojends hs "" = (reverse hs)
>           niojends hs@(h:hs') ('<':'<':ts')
>               | punctu h = (reverse hs')++ts'
>               | otherwise = (reverse hs)++ts'
>           niojends hs@(h:hs') ts@(t:ts')
>               | punctu h && punctu t = (reverse hs')++ts
>               | (h=='\n') && (t=='\n') = (reverse hs')++ts'
>               | punctu t = (reverse hs)++ts
>               | otherwise = (reverse hs)++(' ':ts)
>           punctu c = elem c ",;.:?!"

After the text is composed, it is reflowed on spaces to
fit within the line length limit. 

> reflow s measure = reflow' [] s 0
>     where reflow' h [] _ = reverse h
>           reflow' hs@('\n':_) (' ':ts) n = reflow' hs ts n
>           reflow' hs@(' ':_) (' ':ts) n = reflow' hs ts n
>           reflow' h (' ':ts) n | n >= measure = reflow' ('\n':h) ts 0
>                                | otherwise = reflow' (' ':h) ts (n+1)
>           reflow' h ('\n':ts) n = reflow' ('\n':h) ts 0
>           reflow' h (t:ts) n = reflow' (t:h) ts (n+1)

= Tests =

Selector interpreter is used by both configuration (syndromes) and explanation
(text generation). 

> _test_sel =
>     [SelectEntry [SelectEQ (-5), SelectLE 1]
>                      [Weight (Number 1), Weight (Number 2)],
>      SelectEntry [SelectBetween (Number 1) (Number 3), SelectGE (Number 5)]
>                      [Weight (Number 4)]]
> testSelectorEQ = Test.resultEQ (concat $ select _test_sel (-5))
>                      [Weight (Number 1),Weight (Number 2)]
>                  "Compute.select equal"
> testSelectorLE = Test.resultEQ (concat $ select _test_sel 0,
>                                 concat $ select _test_sel 1)
>                      ([Weight (Number 1),Weight (Number 2)],
>                       [Weight (Number 1),Weight (Number 2),Weight (Number 4)])
>                  "Compute.select less-or-equal, between"
> testSelectorBetween = Test.resultEQ (concat $ select _test_sel 2,
>                                      concat $ select _test_sel 3)
>                          ([Weight (Number 4)],[Weight (Number 4)])
>                       "Compute.select between"
> testSelectorGE = Test.resultEQ (concat $ select _test_sel 5,
>                                 concat $ select _test_sel 6)
>                       ([Weight (Number 4)],[Weight (Number 4)])
>                  "Compute.select greater-or-equal"

Tests for configuration: empty, fixed syndrome value, syndrome expansion.

> testConfigurationEmpty = Test.resultEQ (configuration (Network [] [] []) []) []
>                          "Compute.configuration []"
> testConfigurationFixed = Test.resultEQ (configuration
>                                         (Network [] [Entity "x" "" [Sequence [Weight (Number 1)]]] [])
>                                         [])
>                                        [("x",Number 1)]
>                              "Compute.configuration [x->1]"
> testConfigurationExpansion = Test.resultEQ
>                                   (configuration
>                                    (Network [] [Entity "x" "" [Expansion "y" _test_sel]] [])
>                                    [("y",Number 0)])
>                                   [("x",Number 3),("y",Number 0)]
>                              "Compute.configuration simple"

Tests on explanation: value substitution, explanation reference.

> testExplanationText = Test.resultEQ (explanation
>                                      (Network [] []
>                                       [Entity "*" ""
>                                        [Sequence [XText (Plain "x = "),
>                                                   XText (Value "x")]]])
>                                      "*"
>                                      [("x",Number 1)])
>                                     "x = 1.0"
>                            "Compute.explanation @"
> testExplanationReference = Test.resultEQ (explanation
>                                           (Network [] []
>                                            [Entity "*" ""
>                                             [Sequence [Explanation "a"]],
>                                             Entity "a" ""
>                                             [Sequence [XText (Plain "x")]]])
>                                           "*"
>                                           [])
>                                           "x"
>                            "Compute.explanation ^"
> testExplanationMultiline = Test.resultEQ (explanation
>                                           (Network [] []
>                                            [Entity "*" "Label"
>                                             [Sequence [XText (Plain "x")],
>                                              Sequence [XText (Plain "y.")],
>                                              Sequence [XText (Plain ", z.")]]])
>                                           "*"
>                                           [])
>                                           "\n\n= Label =\n\nx y, z. \n\n"
>                            "Compute.explanation multiline"

> testsCompute = Test.runSuite [testSelectorEQ,
>                               testSelectorLE,
>                               testSelectorBetween,
>                               testSelectorGE,
>                               testConfigurationEmpty,
>                               testConfigurationFixed,
>                               testConfigurationExpansion,
>                               testExplanationText,
>                               testExplanationReference,
>                               testExplanationMultiline]