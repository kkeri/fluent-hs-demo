{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib
    ( process
    ) where


import           Data.Char
import           Prelude   hiding (concat, interact)


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
  | Pol                -- Polarize a stream of terms

  | Fix                -- Fixed-point combinator
  | Call               -- Flatten and interpret the following list of terms
  -- Special terms
  | Fail               -- Local failure


------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

-- Primary values
data PVal
  = Pos Tm             -- positive value
  | Neg Tm             -- negative value

type Global = Tm

-- Programs
-- Just []: empty/successful program
-- Nothing: failed program
type Prog = Maybe [PVal]

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

-- Concatenate two programs.
concat :: Prog -> Prog -> Prog
concat Nothing _         = Nothing
concat _ Nothing         = Nothing
concat (Just a) (Just b) = Just (a ++ b)


------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction of two terms.
interact :: Global -> Tm -> Tm -> Prog
interact g n p = case (n, p) of

  (Cons, a)          -> Just [Neg $ Pair Cons a]
  (Pair Cons a, b)   -> Just [Pos $ Pair a b]
  (Decons, Pair a b) -> Just [Pos a, Pos b]

  (Dup, a)           -> Just [Pos a, Pos a]
  (Drop, _)          -> Just []
  (Swap, a)          -> Just [Neg $ Pair Swap a]

  (Tokens, Text s)   -> tokens s
  (Tokens, _)        -> Nothing

  (ParseLists, Tok (Bracket '{')) -> Just [Neg ParseListBody]
  (ParseLists, a)    -> Just [Pos a, Neg ParseLists]

  (ParseListBody, Tok (Bracket '}')) -> Just [Pos Nil, Neg ParseLists]
  (ParseListBody, a) -> Just [Neg Cons, Pos a, Neg ParseListBody]

  (Pair Swap a, b)   -> Just [Pos b, Pos a]
  (Fix, a)           -> Just [Neg Call, Pos a, Pos $ Pair Fix a]

  (Call, a)          -> call a

  (Pol, a)           -> concat (polarized a) (Just [Neg Pol])

  -- default
  _                  -> Nothing

-- Extract the next token from a source text.
tokens :: String -> Prog
tokens s = case s of
  [] -> Just []
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in Just [Pos $ Tok (Op (c:tok)), Neg Tokens, Pos $ Text rest]
  c : cs | isAlpha c -> let (tok, rest) = span isAlphaNum cs
                        in Just [Pos $ Tok (Name (c:tok)), Neg Tokens, Pos $ Text rest]
  c : cs | c `elem` "(){}[]" -> Just [Pos $ Tok (Bracket c), Neg Tokens, Pos $ Text cs]
  _ -> Nothing -- unexpected character

opChar = "+-*=!?/\\|<>$@#%^&~:"

-- Flatten a list and compute its meaning.
call :: Tm -> Prog
call (Pair a b) = concat (polarized a) (call b)
call Nil        = Just []
call a          = polarized a

polarized :: Tm -> Prog
polarized (Tok t) = polarizedTok t
polarized a       = Just [Pos a]

polarizedTok :: Tok -> Prog
polarizedTok (Name "cons")          = Just [Neg Cons]
polarizedTok (Name "decons")        = Just [Neg Decons]

polarizedTok (Name "dup")           = Just [Neg Dup]
polarizedTok (Name "swap")          = Just [Neg Swap]
polarizedTok (Name "drop")          = Just [Neg Drop]

polarizedTok (Name "tokens")        = Just [Neg Pol]
polarizedTok (Name "parseLists")    = Just [Neg Pol]
polarizedTok (Name "parseListBody") = Just [Neg Pol]
polarizedTok (Name "pol")           = Just [Neg Pol]

polarizedTok (Name "fix")           = Just [Neg Fix]
polarizedTok (Name "call")          = Just [Neg Call]

polarizedTok (Name "fail")          = Nothing

polarizedTok t                      = Just [Pos $ Tok t]


------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

-- Create a continuation from a stack and an input stream.
-- The evaluation strategy is "operator side to operand side"
-- A stack is a list of negative values.
makeCont :: [Tm] -> [PVal] -> Cont
makeCont _ _ Nothing          = BFail
makeCont stk input (Just arg) = continue stk (arg ++ input)

-- Continue the evaluation of a program using "operator side to operand side"
-- strategy until reaching a breakpoint.
continue :: [Tm] -> [PVal] -> BreakPoint
continue stk input = case (stk, input) of
  (stk, Neg t:input)   -> continue (t:stk) input          -- negative values are pushed to the program stack
  (u:stk, Pos t:input) -> BInter t u (makeCont stk input) -- positive active values interact with the top of the stack
  ([], Pos t:input)    -> BOutput t (makeCont [] input) -- positive passive values are sent to the output
  (stk, [])            -> BInput (makeCont stk [])    -- no more input, input is required

-- Create an effect handler from an interaction handler.
interact2handler :: Interact -> Handler
interact2handler ih n p k = k (ih n p)

-- Run a program on an input stream and produce an output stream.
process :: Handler -> Prog -> String -> String
process handle p s = run' (makeCont [] (concat p (Just [Pos (Text s)])))
  where
  run' (BInter p n k) = run' (handle p n k)
  run' (BOutput p k)  = p : run' (k (Just []))
  run' (BInput _)     = []      -- the whole input stream is consumed, stop
  run' BFail          = [Fail]  -- don't revoke the output so far but stop with a failure
