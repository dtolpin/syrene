$Id: Parse.lhs,v 1.23 2007/02/21 11:55:55 dvd Exp $

> module Parse (network,testsParse) where
>
> import Synet

> import Char (isSpace)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec ((<|>), (<?>))

> import qualified Test

Synet (Syrene Network) parser. Reads a questionnaire/syndrome network
specification and builds a network representation as defined in Synet. 

Syntax

Example:

| section * Syntax samples
|   ^section1,section2
|   ^section3
| 
| section section2 Heading
|    /symptom1/ Choose one of the following:
|      <1> yellow
|      <2> round
|      <3> sweet
| 
|    +symptom1
|      2 -> ^section5
|      1,3 -> ^section6
| 
| syndrome sy
|    -10
|    +symptom1 1-4 -> 1
|              5-7 -> 2
|    +symptom2 1 -> 2
|    +symptom4 1-* -> 3
| 
| explanation *
|    Case Description and Hypothesis
|    +sy 0-1 -> explanation for the @sy value range
|        2-5 -> ^another
|    More text here
| | explanation another
|     some text on a particular case 
|     taken into a separate section

Specification

A network consists of questionnaire sections, syndromes and explanations, each
represented by a separated entity.

> p_network = do newlines;
>                ent [] [] []
>     where ent ses sys exs =
>                do se <- ( p_section <|> p_table )
>                   ent (se:ses) sys exs
>            <|> do sy <- p_syndrome
>                   ent ses (sy:sys) exs
>            <|> do ex <- p_explanation
>                   ent ses sys (ex:exs)
>            <|> do P.eof
>                   return (Network ses sys exs)
>                
> p_section = p_entity "section" p_section_body
> p_syndrome = p_entity "syndrome" p_syndrome_body
> p_explanation = p_entity "explanation" p_explanation_body

= Entity Structure =

Each entity begins with a line starting in the first position with a keyword.
Other lines in the section are empty or indented. Line part from '#' till
the line end is a comment and ignored.

> network name = P.runParser p_network () name 
>                . concatMap ((++ "\n") . takeWhile (/= '#'))
>                . lines

> p_entity kw p_body =
>     do (name,label) <- p_heading kw
>        contents <-
>            P.many (do spaces1
>                       content <- 
>                           (   do name <- p_expansion
>                                  P.try (do newlines1
>                                            spaces1)
>                                   <|> spaces
>                                  selentries <-
>                                      sepBy (do guards <- p_selection
>                                                entries <- p_body
>                                                return (SelectEntry guards
>                                                        entries))
>                                           (do newlines1
>                                               spaces1)
>                                  return (Expansion name selentries)
>                           <|> do entries <- p_body
>                                  return (Sequence entries))
>                       (newlines1 <?> "123")
>                       return content)
>        return (Entity name label contents)
>
> p_heading kw =
>     do keyword kw
>        spaces1
>        name <- tok_name
>        label <- (    do spaces1
>                         tok_rest
>                  <|> return "")
>        newlines1
>        return (name,label)
>     where keyword kw = P.try (P.string kw) <?> kw

= Expansions =

An expansion start with control variable:

> p_expansion =
>     do P.char '+'
>        spaces
>        tok_qname

Guards are comma-separated lists of points and ranges.

> p_selection =
>     do guards <- sepBy1 p_guard (tok_sep ',')
>        spaces
>        P.string "->"
>        spaces
>        return guards
>     <?> "selection"
>
> testSelection = Test.resultEQ (P.parse p_selection "" "-1, 2, 5-6 ->")
>                               (Right [SelectEQ (-1),
>                                       SelectEQ 2,
>                                       SelectBetween 5 6])
>                 "Parse.selection"

> p_guard = P.try (do lower <- p_bound
>                     tok_sep '-'
>                     upper <- p_bound
>                     return (inBounds lower upper))
>       <|> do bound <- p_bound
>              return (inBounds bound bound)
>       <?> "guard"

Either bound can be missing, in which case it is replaced by '*'.

> p_bound = do n <- tok_number
>              return (Just n)
>       <|> do P.char '*'
>              return Nothing
>       <?> "bound"

= Table Formatting =
 
Table is a specialized section that defines a tabular question. Table
is specified by its columns and rows.

> p_table =
>     do (name,label) <- p_heading "table"
>        columns <- P.many1 (P.try column)
>        separator
>        rows <- P.many1 row
>        return (Entity name label
>                [Sequence [Table rows columns]])
>     where separator = do spaces1
>                          P.string "-+-"
>                          newlines1
>           column = do spaces1 
>                       answer <- p_answerform
>                       spaces
>                       label <- tok_rest
>                       newlines1
>                       return (answer,label)
>           row = do spaces1
>                    name <- p_quemark
>                    spaces
>                    label <- tok_rest
>                    newlines1
>                    return (name,label)

= Body Parts =

Section is questions.

> p_section_body = p_sectrefs
>              <|> do words <- P.many1 (p_question
>                                       <|> p_answer
>                                       <|> p_svalref
>                                       <|> p_splain)
>                     return words

Syndrome is a sum of weights.

> p_syndrome_body = p_syndrefs 
>               <|> do spaces
>                      ns <- sepBy1 tok_number (tok_sep ',')
>                      spaces
>                      return (map Weight ns)

Explanation is text intermixed with value references.

> p_explanation_body = p_explrefs
>                  <|> do words <- P.many1 (p_xvalref <|> p_xplain)
>                         return words

Entity references expand into entity bodies (in sections and explanations).
References is a list extending to the end of line.

> p_entrefs = do P.char '^'
>                spaces
>                sepBy1 tok_name (tok_sep ',')
> p_sectrefs = do refs <- p_entrefs
>                 return (map Section refs)
> p_syndrefs = do refs <- p_entrefs
>                 return (map Syndrome refs)
> p_explrefs = do refs <- p_entrefs
>                 return (map Explanation refs)

Question names are set by question marks -- names in slashes, or specified
expliticly for checkboxes. The first question name is the section name;
the section name is substituted for '*'.

> p_quemark = do tok_lbr '/'
>                name <- tok_name
>                tok_rbr '/'
>                return name
> p_question = do name <- p_quemark
>                 return (Question name)

Answers mark points of data input fields.

> p_answerform = (p_checkbox <|> p_radio <|> p_freeform)
> p_answer = do answer <- p_answerform
>               return (Answer answer)
> p_checkbox = do tok_lbr '['
>                 name <- tok_name
>                 tok_rbr ']'
>                 return (Checkbox name)
> p_radio = do tok_lbr '<'
>              value <- tok_number
>              tok_rbr '>'
>              return (Radio value)
> p_freeform = do P.skipMany1 (P.char '_')
>                 return Freeform

Variable values are substituted for value references. This is more
important in explanations than in the questionaire.

> p_valref = do P.char '@'
>               spaces
>               name <- tok_qname
>               return (Value name)
> p_svalref = do vr <- p_valref
>                return (SText vr)
> p_xvalref = do vr <- p_valref
>                return (XText vr)

The rest is text.

> p_splain = do s <- tok_splain
>               return (SText (Plain s))
> p_xplain = do s <- tok_xplain
>               return (XText (Plain s))

= Fixes for Parsec =

A variant of sepBy not failing on following same as separator.

> sepBy p sep = sepBy1 p sep <|> return []
> sepBy1 p sep  = do x <- p
>                    xs <- P.many (P.try (sep >> p))
>                    return (x:xs)
>
> testSepBy = Test.resultEQ (P.parse (do sepBy (P.char '1') (P.char '.')
>                                        P.char '.'
>                                        P.eof)
>                                 "" "1.1.")
>                           (Right ())
>             "Parse.sepBy"

= Low-Level Parsing Primitives: spaces, literals, numbers, punctuation =

Characters can be escaped in plain-text areas by prefixing them with '\'.
Line ends are normalized by 'src' above and are always '\n'.

> space = do P.satisfy (\c -> isSpace c && (c/='\n'))
>            return ()
> spaces = P.skipMany space
> spaces1 = P.skipMany1 space
>
> newline = P.try (do spaces
>                     P.char '\n'
>                     return ())
>           <?> "end of line"
> newlines = P.skipMany newline
> newlines1 = P.skipMany1 newline
>
> eschar = P.try (do P.char '\\'
>                    (    do P.char '\n'
>                            return ' '
>                     <|> do P.char 'n'
>                            return '\n'
>                     <|> P.anyChar))
>          <?> "escaped character"

Sections, evidence variables, syndromes and explanations are referred
by their names. Star (*) instead of name refers to an implicit value:
  - in entity headings, it denotes a root entity;
  - in questions, it gives the section name to the question.

> tok_name = do c <- (P.letter <|> P.char '_')
>               cs <- P.many (P.alphaNum <|> P.char '_' <|> P.char '-')
>               return (c:cs)
>            <|> P.string "*"
>            <?> "name"

Sections and question names are unqualified names. Variable names are
composed by joining section and question names by dots.

> tok_qname = do pfx:sfxs <- P.sepBy1 tok_name (tok_sep '.')
>                return (foldl (\a b -> a++('.':b)) pfx sfxs)
>             <?> "qualified name"

In entity headings, labels extend to the line end.

> tok_rest = P.many (P.noneOf "\\\n" <|> eschar)

In section bodies, the text may contain variables, checkboxes, choices,
freeform fields and value references.

> tok_splain = P.many1 (P.noneOf "\\@<[_/\n" <|> eschar)

In explanations, only value references interrupt the text.

> tok_xplain = P.many1 (P.noneOf "\\@\n" <|> eschar)

A number includes an optional sign, and either the entire or
the fractional part is optional, not both. read is used to
convert to float, and Haskell does not allow either '+' or
empty entire part in floats -- workarounds are provided.

> tok_number = P.try (do sign <- (P.char '-' <|> P.char '+' <|> return '+')
>                        digits <- (do entire <- P.many1 P.digit
>                                      fractional <- do point <- P.char '.'
>                                                       digits <- P.many P.digit
>                                                       return (point:digits)
>                                                <|> return []
>                                      return (entire++(case fractional of
>                                                       "." -> ".0"
>                                                       _ -> fractional))
>                               <|> do entire <- P.many P.digit
>                                      fractional <- do point <- P.char '.'
>                                                       digits <- P.many1 P.digit
>                                                       return (point:digits)
>                                      return ((case entire of
>                                               "" -> "0"
>                                               _ -> entire)++fractional))
>                        return (Number (read (case sign of
>                                              '+' -> digits
>                                              _ -> sign:digits) :: Float)))
>              <?> "number"
>
> testNumber = Test.resultEQ
>              (P.parse (P.sepBy tok_number (P.char ',')) ""
>               "1,2.,.3,4.5,+6,-7")
>              (Right (map Number [1,2,0.3,4.5,6,-7]))
>              "Parse.number"

Common weak tokens used in embedded structures such as guards and answer forms.

 * separator surrounded by spaces (guards and reference lists):

> tok_sep sep = P.try (do spaces
>                         P.char sep
>                         spaces
>                         return ())

 * square and angle brackets (guards, radio buttons and checkboxes):

> tok_lbr lbr = P.try (do P.char lbr
>                         spaces
>                         return ())
> tok_rbr rbr = P.try (do spaces
>                         P.char rbr
>                         return ())


= Test Suite =

Tests for small parts are interspersed with grammar rules; below are tests
for parsing of kinds of entities.

> testSection = Test.resultEQ (P.parse p_network ""
>                              "section name1 Heading\\\n\
>                              \continued\n\
>                              \  /symptom1/ Choose one:\n\
>                              \    <1> one\n\
>                              \    <2> two\n\
>                              \  +symptom1\n\
>                              \  1-2 -> ^s2\n")
>                             (Right
>                              (Network
>                               [Entity "name1" "Heading continued"
>                                [Sequence
>                                 [Question "symptom1",
>                                  SText (Plain " Choose one:")],
>                                 Sequence [Answer (Radio 1),
>                                           SText (Plain " one")],
>                                 Sequence [Answer (Radio 2),
>                                           SText (Plain " two")],
>                                 Expansion "symptom1"
>                                  [SelectEntry [SelectBetween 1 2]
>                                   [Section "s2"]]]] [] []))
>               "Parse.section"
>
> testSyndrome = Test.resultEQ (P.parse p_network ""
>                               "syndrome sy1\n\
>                               \  -5\n\
>                               \  +sy2 1-2 -> 1\n\
>                               \         3 -> 2\n\
>                               \         5 -> ^sy2\n")
>                              (Right (Network []
>                                      [Entity "sy1" ""
>                                       [Sequence [Weight (Number (-5))],
>                                        Expansion "sy2"
>                                         [SelectEntry [SelectBetween 1 2] [Weight (Number 1)],
>                                          SelectEntry [SelectEQ 3] [Weight (Number 2)],
>                                          SelectEntry [SelectEQ 5] [Syndrome "sy2"]]]] []))
>                "Parse.syndrome"
>
> testExplanation = Test.resultEQ (P.parse p_network ""
>                                   "explanation ex\n\
>                                   \  Title\n\
>                                   \  +sy1.a  1 -> One\n\
>                                   \          2 -> Two @sy1\n")
>                                 (Right (Network [] []
>                                         [Entity "ex" ""
>                                          [Sequence
>                                           [XText (Plain "Title")],
>                                           Expansion "sy1.a"
>                                            [SelectEntry [SelectEQ 1]
>                                             [XText (Plain "One")],
>                                             SelectEntry [SelectEQ 2]
>                                             [XText (Plain "Two "),
>                                              XText (Value "sy1")]]]]))
>                   "Test.explanation"

= Test Suite =

The test suite contains tests for tricky or complex constructs, as well
as test instances for each of the kinds of entities.

> testsParse = Test.runSuite [testSelection,
>                             testSepBy,
>                             testNumber,
>                             testSection,
>                             testSyndrome,
>                             testExplanation
>                            ]

