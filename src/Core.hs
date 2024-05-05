-- Syntax of the core language.

module Core
  ( Tm(..)
  , interactTm
  ) where

import           Data.Char (isAlpha, isAlphaNum, isSpace)
import           Eval
import           Prelude   hiding (False, True)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- Terms
-- Syntactically, a program is a sequence of terms.
data Tm
  -- Tokens are embedded into the term type for convenience
  = Name String     -- Identifier (starts with lowercase)
  | Op String       -- Operator symbol
  | Bracket Char    -- Bracket
  | Str String      -- String literal
  -- Lists
  | Pair Tm Tm      -- Building block of lists
  | Nil             -- Empty list
  -- Positive special terms
  | True            -- Boolean true
  | False           -- Boolean false
  | EndNorm         -- End a normalization region
  -- Negative special terms
  | Cons            -- Prepends a term to a list
  | Uncons          -- Splits a list into its head and tail

  | Dup             -- duplicate the following term
  | Swap            -- swap the two following terms
  | Drop            -- drop the following term

  | Tokens          -- Tokenize a source text
  | Nest            -- Parse lists, passes anything else
  | Coll            -- Cons together the following terms until a closing bracket
  | Pol             -- Assign polarity to a term
  | Pols            -- Assign polarity to a stream of terms

  | Fix             -- Fixed-point combinator
  | Apply           -- Flatten and interpret a list
  | StartNorm       -- Begin a normalization region

  | Cond            -- Conditional

  deriving (Eq)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

instance Show Tm where
  show (Name s)    = s
  show (Op s)      = s
  show (Bracket c) = [c]
  show (Str s)     = s

  show (Pair a b)  = "(" ++ showPair a b ++ ")"
  show Nil         = "()"

  show Cons        = "cons"
  show Uncons      = "uncons"

  show Dup         = "dup"
  show Swap        = "swap"
  show Drop        = "drop"

  show Tokens      = "tokens"
  show Nest        = "nest"
  show Coll        = "coll"
  show Pol         = "pol"
  show Pols        = "pols"

  show StartNorm   = "startNorm"
  show EndNorm     = "endNorm"

  show Fix         = "fix"
  show Apply       = "apply"

  show True        = "true"
  show False       = "false"
  show Cond        = "cond"

showPair :: Tm -> Tm -> String
showPair a (Pair b c) = show a <> " " ++ showPair b c
showPair a Nil        = show a
showPair a b          = show a ++ " . " ++ show b

------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction of two terms.
interactTm :: Interact Tm
interactTm n p = case (n, p) of

  (StartNorm, EndNorm) -> Just []
  (StartNorm, a)       -> Just [Pos a, Neg StartNorm]
  (a, EndNorm)         -> Just (Pos EndNorm : flatPA a)

  (Cons, a)            -> Just [Neg $ Pair Cons a]
  (Pair Cons a, b)     -> Just [Pos $ Pair a b]
  (Uncons, Pair a b)   -> Just [Pos a, Pos b]
  (Uncons, _)          -> Just [runtimeError "uncons: not a pair: " p]

  (Dup, a)             -> Just [Pos a, Pos a]
  (Drop, _)            -> Just []
  (Swap, a)            -> Just [Neg $ Pair Swap a]
  (Pair Swap a, b)     -> Just [Pos b, Pos a]

  (Tokens, Str s)      -> Just (tokens s)
  (Tokens, _)          -> Just [runtimeError "tokens: not a character: " p]

  (Nest, Bracket '(')  -> Just [Neg Coll, Neg Nest]
  (Nest, a)            -> Just [Pos a, Neg Nest]

  (Coll, Bracket ')')  -> Just [Pos Nil]
  (Coll, a)            -> Just [Neg Cons, Pos a, Neg Coll]

  (Pol, a)             -> Just [pol a]
  (Pols, a)            -> Just [Neg Pol, Pos a, Neg Pols]

  (Fix, f)             -> Just [Neg Apply, Pos f, Pos $ Pair Fix f]
  (Apply, a)           -> Just (apply a)

  (Cond, a)            -> Just [Neg $ Pair Cond a]
  (Pair Cond (Pair a _), True) -> Just [Pos a]
  (Pair Cond (Pair _ b), _)    -> Just [Pos b]
  (Pair Cond a, b)     -> Just [Pos $ Pair Cond $ Pair a b]


  -- undefined interactions
  _                    -> Nothing

runtimeError :: String -> Tm -> Value Tm
runtimeError msg t = Error $ Pair (Str msg) t

-- Returns a program that lazily splits a string to tokens.
tokens :: String -> Prog Tm
tokens s = case s of
  [] -> []
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in [Pos $ Op (c:tok), Neg Tokens, Pos $ Str rest]
  c : cs | isAlpha c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ Name (c:tok), Neg Tokens, Pos $ Str rest]
  c : cs | c `elem` "(){}[]" -> [Pos $ Bracket c, Neg Tokens, Pos $ Str cs]
  c : _ -> [runtimeError "unexpected character" (Str [c])]

opChar :: String
opChar = "+-*=!?/\\|<>$@#%^&~:"

-- Flatten a list and interpret it.
apply :: Tm -> Prog Tm
apply (Pair a b) = pol a : apply b
apply Nil        = []
apply a          = [pol a]

-- Assign polarity to a term.
pol :: Tm -> Value Tm
pol (Name "cons")      = Neg Cons
pol (Name "uncons")    = Neg Uncons

pol (Name "dup")       = Neg Dup
pol (Name "swap")      = Neg Swap
pol (Name "drop")      = Neg Drop

pol (Name "tokens")    = Neg Tokens
pol (Name "nest")      = Neg Nest
pol (Name "coll")      = Neg Coll
pol (Name "pols")      = Neg Pols

pol (Name "fix")       = Neg Fix
pol (Name "apply")     = Neg Apply

pol (Name "startNorm") = Neg StartNorm
pol (Name "endNorm")   = Pos EndNorm

-- Everything else is a positive value
pol a                  = Pos a

-- Flatten a partially applied combinator.
flatPA :: Tm -> Prog Tm
flatPA (Pair a b) = Pos a : flatPA b
flatPA Nil        = []
flatPA a          = [Pos a]
