{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib
    ( interact
    , interact2handler
    , process
    ) where


import           Data.Char
import           Prelude   hiding (interact)


------------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------------

data Tok
  = Name String    -- Identifier (starts with lowercase)
  | Op String       -- Operator symbol
  | Bracket Char    -- Bracket
  deriving Eq

-- Core syntax
data Tm
  -- Tokenizer
  = Text String       -- Source text
  | Tok Tok           -- Token
  -- Lists
  | Pair Tm Tm        -- list
  | Nil               -- Empty list
  -- Combinators
  | Cons               -- Prepends a term to a list
  | Decons             -- Splits a list into its head and tail

  | Dup                -- duplicate the following term
  | Swap               -- swap the two following terms
  | Drop               -- drop the following term

  | Tokens             -- Tokenizing a source text
  | ParseLists         -- Parse lists, passes anything else
  | ParseListBody      -- Build a list from the following primary terms
  | Values             -- Classifies a stream of terms as either positive or negative values

  | Fix                -- Fixed-point combinator
  | Call               -- Flatten and interpret the following list of terms


------------------------------------------------------------------------------
-- Printer
------------------------------------------------------------------------------

instance Show Tm where
  show (Text s)          = s
  show (Tok (Name s))    = s
  show (Tok (Op s))      = s
  show (Tok (Bracket c)) = [c]
  show (Pair a b)        = "{" ++ showPair a b ++ "}"
  show Nil               = "{}"

  show Cons              = "cons"
  show Decons            = "decons"

  show Dup               = "dup"
  show Swap              = "swap"
  show Drop              = "drop"

  show Tokens            = "tokens"
  show ParseLists        = "parseLists"
  show ParseListBody     = "parseListBody"
  show Values            = "Values"

  show Fix               = "fix"
  show Call              = "call"

showPair :: Tm -> Tm -> String
showPair a (Pair b c) = show a ++ showPair b c
showPair a Nil        = show a
showPair a b          = show a ++ " : " ++ show b

showTerms :: [Tm] -> String
showTerms []       = ""
showTerms [t]      = show t
showTerms (t : ts) = show t ++ " " ++ showTerms ts

------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

-- Values
data Value
  = Pos Tm             -- positive value
  | Neg Tm             -- negative value
  | Fail               -- failure

-- Programs
type Prog = [Value]

-- Continuation
type Cont = Prog -> BreakPoint

-- Interaction function
type Interact = Tm -> Tm -> Prog

-- Effect handler
type Handler = Tm -> Tm -> Cont -> BreakPoint

-- Evaluator breakpoint
data BreakPoint
  = BInter Tm Tm Cont  -- an interaction is requested
  | BOutput Tm Cont    -- an output term has been produced
  | BInput Cont        -- input is required, no more output can be produced
  | BFail              -- evaluation has failed


------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction of two terms.
interact :: Tm -> Tm -> Prog
interact n p = case (n, p) of

  (Cons, a)                          -> [Neg $ Pair Cons a]
  (Pair Cons a, b)                   -> [Pos $ Pair a b]
  (Decons, Pair a b)                 -> [Pos a, Pos b]

  (Dup, a)                           -> [Pos a, Pos a]
  (Drop, _)                          -> []
  (Swap, a)                          -> [Neg $ Pair Swap a]

  (Tokens, Text s)                   -> tokens s
  (Tokens, _)                        -> [Fail]

  (ParseLists, Tok (Bracket '{'))    -> [Neg ParseListBody]
  (ParseLists, a)                    -> [Pos a, Neg ParseLists]

  (ParseListBody, Tok (Bracket '}')) -> [Pos Nil, Neg ParseLists]
  (ParseListBody, a)                 -> [Neg Cons, Pos a, Neg ParseListBody]

  (Pair Swap a, b)                   -> [Pos b, Pos a]
  (Fix, a)                           -> [Neg Call, Pos a, Pos $ Pair Fix a]

  (Call, a)                          -> call a

  (Values, a)                        -> [value a, Neg Values]

  -- default
  _                                  -> [Fail]

-- Extract the next token from a source text.
tokens :: String -> Prog
tokens s = case s of
  [] -> []
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in [Pos $ Tok (Op (c:tok)), Neg Tokens, Pos $ Text rest]
  c : cs | isAlpha c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ Tok (Name (c:tok)), Neg Tokens, Pos $ Text rest]
  c : cs | c `elem` "(){}[]" -> [Pos $ Tok (Bracket c), Neg Tokens, Pos $ Text cs]
  _ -> [Fail] -- unexpected character

opChar :: String
opChar = "+-*=!?/\\|<>$@#%^&~:"

-- Flatten a list and compute its meaning.
call :: Tm -> Prog
call (Pair a b) = value a : call b
call Nil        = []
call a          = [value a]

value :: Tm -> Value
value (Tok t) = valueOfTok t
value a       = Pos a

valueOfTok :: Tok -> Value
valueOfTok (Name "cons")          = Neg Cons
valueOfTok (Name "decons")        = Neg Decons

valueOfTok (Name "dup")           = Neg Dup
valueOfTok (Name "swap")          = Neg Swap
valueOfTok (Name "drop")          = Neg Drop

valueOfTok (Name "tokens")        = Neg Tokens
valueOfTok (Name "parseLists")    = Neg ParseLists
valueOfTok (Name "parseListBody") = Neg ParseListBody
valueOfTok (Name "Values")        = Neg Values

valueOfTok (Name "fix")           = Neg Fix
valueOfTok (Name "call")          = Neg Call

valueOfTok (Name "fail")          = Fail

valueOfTok t                      = Pos $ Tok t


------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

-- Create a continuation from a stack and an input stream.
-- The evaluation strategy is "operator side to operand side"
-- A stack is a list of negative values.
makeCont :: [Tm] -> [Value] -> Cont
makeCont stk input arg = continue stk (arg ++ input)

-- Continue the evaluation of a program using "operator side to operand side"
-- strategy until reaching a breakpoint.
continue :: [Tm] -> [Value] -> BreakPoint
continue stk input = case (stk, input) of
  -- failure stops evaluation
  (_, Fail:_)          -> BFail
  -- negative values are pushed to the operator stack
  (stk, Neg n:input)   -> continue (n:stk) input
  -- active operator terms interact with the top of the stack
  (n:stk, Pos p:input) -> BInter n p (makeCont stk input)
  -- passive operator terms are sent to the output
  ([], Pos p:input)    -> BOutput p (makeCont [] input)
  -- more input is required
  (stk, [])            -> BInput (makeCont stk [])

-- Create an effect handler from an interaction handler.
interact2handler :: Interact -> Handler
interact2handler ih n p k = k (ih n p)

-- Run a program on an input stream and produce an output stream.
process :: Handler -> Prog -> String -> String
process handle p s = showTerms $ process' $ continue [] (p ++ [Pos (Text s)])
  where
  process' :: BreakPoint -> [Tm]
  process' (BInter p n k) = process' (handle p n k)
  process' (BOutput p k)  = p : process' (k [])
  process' (BInput _)     = []  -- the whole input stream is consumed, stop
  process' BFail          = [Tok (Name "fail")]
