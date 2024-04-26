-- Syntax of the core language.

module Core
  ( Tm(..)
  , Tok(..)
  , interactTm
  , defHandler
  ) where

import           Data.Char (isAlpha, isAlphaNum, isSpace)
import           Eval

------------------------------------------------------------------------------
-- Types
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
  -- Combinators have dedicated constructors for better readability and performance,
  -- however they could be represented as tokens as well.
  | Cons               -- Prepends a term to a list
  | Decons             -- Splits a list into its head and tail

  | Dup                -- duplicate the following term
  | Swap               -- swap the two following terms
  | Drop               -- drop the following term

  | Tokens             -- Tokenizing a source text
  | ParseLists         -- Parse lists, passes anything else
  | ParseListBody      -- Build a list from the following primary terms until a closing bracket
  | Values             -- Classifies a stream of terms as either positive or negative values

  | Fix                -- Fixed-point combinator
  | Apply              -- Flatten and interpret a list


------------------------------------------------------------------------------
-- Utilities
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
  show Apply             = "apply"

showPair :: Tm -> Tm -> String
showPair a (Pair b c) = show a ++ " " ++ showPair b c
showPair a Nil        = show a
showPair a b          = show a ++ " ; " ++ show b


------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction of two terms.
interactTm :: Interact Tm
interactTm n p = case (n, p) of

  (Cons, a)                          -> Just [Neg $ Pair Cons a]
  (Pair Cons a, b)                   -> Just [Pos $ Pair a b]
  (Decons, Pair a b)                 -> Just [Pos a, Pos b]
  (Decons, _)                        -> Just [Fail]

  (Dup, a)                           -> Just [Pos a, Pos a]
  (Drop, _)                          -> Just []
  (Swap, a)                          -> Just [Neg $ Pair Swap a]
  (Pair Swap a, b)                   -> Just [Pos b, Pos a]

  (Tokens, Text s)                   -> Just (tokens s)
  (Tokens, _)                        -> Just [Fail]

  (ParseLists, Tok (Bracket '{'))    -> Just [Neg ParseListBody]
  (ParseLists, a)                    -> Just [Pos a, Neg ParseLists]

  (ParseListBody, Tok (Bracket '}')) -> Just [Pos Nil, Neg ParseLists]
  (ParseListBody, a)                 -> Just [Neg Cons, Pos a, Neg ParseListBody]

  (Fix, f)                           -> Just [Neg Apply, Pos f, Pos $ Pair Fix f]

  (Apply, a)                         -> Just (apply a)

  (Values, a)                        -> Just [value a, Neg Values]

  -- undefined interactions
  _                                  -> Nothing

-- Split a string to tokens.
tokens :: String -> Prog Tm
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

-- Flatten a list and interprets it.
apply :: Tm -> Prog Tm
apply (Pair a b) = value a : apply b
apply Nil        = []
apply a          = [value a]

value :: Tm -> Value Tm
value (Tok t) = valueOfTok t
value a       = Pos a

valueOfTok :: Tok -> Value Tm
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
valueOfTok (Name "apply")         = Neg Apply

valueOfTok (Name "fail")          = Fail

valueOfTok t                      = Pos $ Tok t


------------------------------------------------------------------------------
-- Effect handlers
------------------------------------------------------------------------------

type Env = [(String, Tm)]

-- Add user defined combinators to the language.
defHandler :: Env -> Handler Tm
defHandler env p = case p of
  -- partial application of `def` to a name
  PEffect (Tok (Name "def")) (Tok (Name n)) k ->
    defHandler env $ k [Neg $ Pair (Tok (Name "def")) (Tok (Name n))]
  -- define name by adding the definition to the environment
  PEffect (Pair (Tok (Name "def")) (Tok (Name n))) t k ->
    defHandler ((n, t) : env) (k [])
  -- apply a defined name to a term
  PEffect (Tok (Name n)) t k -> case lookup n env of
    Just v  -> defHandler env $ k [Neg Apply, Pos v, Pos t]
    Nothing -> p
  p' -> p'
