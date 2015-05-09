$Id: Synet.lhs,v 1.26 2007/05/02 20:43:36 dvd Exp $

Syndrome Networks Concepts

> module Synet ( Name
>              , Label
>              , Value(..)
>              , liftToNumber1
>              , liftToNumber2
>              , showval
>              , undefString
>              , undefNumber
>              , falseNumber
>              , trueNumber
>              , Text(..)
>              , SelectEntry(..)
>              , SelectGuard(..)
>              , allows
>              , Content(..)
>              , concatMapM
>              , Network(..)
>              , Entity(..)
>              , Section
>              , qname
>              , Syndrome
>              , Explanation
>              , SEntry(..)
>              , Answer(..)
>              , YEntry(..)
>              , XEntry(..)
>              , inBounds
>              , missing
>              , testsSynet ) where
> import qualified Test
> import Monad
> import Debug.Trace

Entities have names used for cross-referencing, labels for human perception
and values for computations.

> type Name = String
> type Label = String

Values are polymorphic: either numbers or text strings. Thresholds and 
summation work on numeric values only, but intersect with values to put
syndromes and evidence into the same domain.

> data Value = Number Float | String String deriving (Show,Read)

Evidence variables may have undefined values. To make the type system simpler,
boolean variables are given numeric values.

> undefString = String ""
> undefNumber = Number 0
> falseNumber = Number 1
> trueNumber = Number 2

Make Value an instance of Num -- for computations on thresholds.

> liftToNumber1 op (Number x) = Number (op x)
> liftToNumber1 op x = liftToNumber1 op (valueToNumber x)

> liftToNumber2 op (Number x) (Number y) = Number (op x y)
> liftToNumber2 op x y = liftToNumber2 op (valueToNumber x) (valueToNumber y)

An empty string is 0, any other answer is 1.

> valueToNumber (String "") = Number 0
> valueToNumber (String _) = Number 1
> valueToNumber v = v

> instance Num Value where
>     (+) = liftToNumber2 (+)
>     (-) = liftToNumber2 (-)
>     negate = liftToNumber1 negate
>     (*) = liftToNumber2 (*)
>     abs = liftToNumber1 abs
>     signum = liftToNumber1 signum
>     fromInteger x = Number (fromInteger x)

> instance Eq Value where
>     (Number x)==(Number y) = x==y
>     a==b = (valueToNumber a)==(valueToNumber b)

> instance Ord Value where
>     (Number x)<=(Number y) = x<=y
>     x<=y = (valueToNumber x)<=(valueToNumber y)

When inserted literally into explanations or HTML, constructors are removed.

> showval (Number n) = (show n)
> showval (String s) = s

Text anywhere may contain syndrome references.

> data Text = Plain String
>           | Value Name
>             deriving (Show,Read,Eq)

Contents are lists optionally parameterized by syndromes. They are used
throughout the network model. Selectors are lists of opcodes instead of
being just functions to compile them into target machine instructions,
JavaScript, in particular.

> data SelectGuard = SelectAlways
>                  | SelectEQ Value
>                  | SelectLE Value
>                  | SelectGE Value
>                  | SelectBetween Value Value
>                    deriving (Show,Read,Eq)

Guard resolution is also used fo analysis in Haskell code. Strings are matched
against guards by their lengths.

> allows _ SelectAlways = True
> allows v (SelectEQ x) = v==x
> allows v (SelectGE x) = v>=x
> allows v (SelectLE y) = v<=y
> allows v (SelectBetween x y) = x<=v && v<=y

> data SelectEntry a = SelectEntry [SelectGuard] [a] deriving (Show,Read,Eq)
> data Content a = Expansion Name [SelectEntry a]
>                  | Sequence [a]
>                    deriving (Show,Read,Eq)

Since lists of children are often mapped to lists or strings, here
is concatMapM, which concats values in a monad sequence.

> concatMapM f = liftM concat . sequence . map f

Network is a collection of entities of several kinds.

> data Network = Network [Section] [Syndrome] [Explanation] deriving (Show,Read,Eq)

Entity is a labeled sequence of expansions:

> data Entity a = Entity Name Label [Content a] deriving (Show,Read,Eq)

A section is an entity consisting of entries. Entries are:
  * plain text;
  * question;
  * section reference.

A question may be either scalar or tabular. Tabular questions are specified
by their row prefixes and column suffixes and answer forms. Rows and columns
have labels.

> type TableRow = (Name,Label)
> type TableColumn = (Answer,Label)
>
> data SEntry = SText Text
>             | Question Name
>             | Table [TableRow] [TableColumn]
>             | Answer Answer
>             | Section Name
>               deriving (Show,Read,Eq)
> type Section = Entity SEntry

Qualified evidence names are composed by joining by '.'section,
question and column names, any one is optional, but at least one
must be present. 

> qname Nothing Nothing = ""
> qname Nothing (Just q) = q
> qname (Just s) Nothing = s
> qname (Just s) (Just q) = s++"."++q

An answer form is one of checkbox, radio button and freeform text
entry. All have a label, radio buttons provide a list of choices.
Checkbox name is the same as question name in scalar questions.
In tabular questions, it provides the name suffix for the variable.

> data Answer = Checkbox Name
>             | Radio Value
>             | Freeform
>               deriving (Show,Read,Eq)

Additionally, syndromes -- derived traits -- can be computed. A syndrome
is the sum of weights assigned to its symptoms. Variables become symptoms 
when they pass selectors and expand into their weights. Syndro

> data YEntry = Weight Value | Syndrome Name deriving (Show,Read,Eq)
> type Syndrome = Entity YEntry

Explanations are generated text fragments. The text then may be passed
to a formatter/pretty-printer.

> data XEntry = XText Text
>             | Explanation Name
>               deriving (Show,Read,Eq)
> type Explanation = Entity XEntry

Each selector entry is governed by a list of guards. Each guard is specified
by its bounds.

> type Bound = Maybe Value
>
> inBounds Nothing Nothing = SelectAlways
> inBounds (Just x) Nothing = SelectGE x
> inBounds Nothing (Just y)  = SelectLE y
> inBounds (Just x) (Just y) | x==y = SelectEQ x
>                            | otherwise = SelectBetween x y

Reports missing references.

> missing kind name = trace (kind++"? "++name)

= Tests =

> testInBoundsNN = Test.resultEQ (inBounds Nothing Nothing) SelectAlways
>                  "Synet.inBounds Nothing Nothing"
> testInBoundsJN = Test.resultEQ (inBounds (Just 1) Nothing) (SelectGE 1)
>                  "Synet.inBounds Just Nothing"
> testInBoundsNJ = Test.resultEQ (inBounds Nothing (Just 1)) (SelectLE 1)
>                  "Synet.inBounds Nothing Just"
> testInBoundsJJ = Test.resultEQ (inBounds (Just 1) (Just 1)) (SelectEQ 1)
>                  "Synet.inBounds Just==Just"
> testInBoundsIJ = Test.resultEQ (inBounds (Just 1) (Just 2)) (SelectBetween 1 2)
>                  "Synet.inBounds Just/=Just"

> testsSynet = Test.runSuite [testInBoundsNN,
>                             testInBoundsJN,
>                             testInBoundsNJ,
>                             testInBoundsJJ,
>                             testInBoundsIJ]