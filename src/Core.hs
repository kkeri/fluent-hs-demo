-- Syntax of the core language.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Core where

import           Data.Char   (isAlphaNum, isSpace)
import           GHC.Unicode (isLower, isUpper)
import           Prelude     hiding (False, True, interact)

import           Eval

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Token
  = Name String         -- Lowercase name
  | Sym String          -- Capitalized name
  | Op String           -- Operator symbol
  | Paren Char          -- Parenthesis
  | Str String          -- String literal
  deriving (Eq)

data Neg
  = NToken Token        -- A token that is used as a combinator
  | Part Neg [Pos]      -- Partial application

data Pos
  -- Syntax (terms)
  = PToken Token        -- Token
  | Pair Pos Pos        -- Building block of lists
  | PError Pos          -- Runtime error
  | PCont (Cont Neg Pos) -- Continuation

------------------------------------------------------------------------------
-- Pattern synonyms for readability
------------------------------------------------------------------------------

pattern Combinator :: String -> Neg
pattern Combinator s = NToken (Name s)

pattern Symbol :: String -> Pos
pattern Symbol s = PToken (Sym s)

pattern Dup     = Combinator "dup"
pattern Swap    = Combinator "swap"
pattern Drop    = Combinator "drop"
pattern Cons    = Combinator "cons"
pattern Uncons  = Combinator "uncons"
pattern Tokens  = Combinator "tokens"
pattern List    = Combinator "list"
pattern Lists   = Combinator "lists"
pattern Nest    = Combinator "nest"
pattern Flat    = Combinator "flat"
pattern Eval    = Combinator "eval"
pattern Vals    = Combinator "vals"
pattern Fix     = Combinator "fix"
pattern NDump   = Combinator "ndump"
pattern Cond    = Combinator "cond"
pattern EqTok   = Combinator "eqtok"
pattern NError  = Combinator "nerror"
pattern NCont   = Combinator "ncont"
pattern Effect  = Combinator "effect"

pattern Nil     = Symbol "Nil"
pattern End     = Symbol "End"
pattern PDump   = Symbol "PDump"
pattern True    = Symbol "True"
pattern False   = Symbol "False"

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

instance Show Token where
  show (Name s)  = s
  show (Sym s)   = s
  show (Op s)    = s
  show (Paren c) = [c]
  show (Str s)   = show s

instance Show Pos where
  show (PToken t) = show t

  show (Pair a b) = "(" ++ showPair a b ++ ")"
  show Nil        = "()"
  show End        = "End"
  show PDump      = "PDump"

  show True       = "True"
  show False      = "False"
  show (PError e) = "error " ++ show e
  show (PCont _)  = "<cont>"

showPair :: Pos -> Pos -> String
showPair a (Pair b c) = show a <> " " ++ showPair b c
showPair a Nil        = show a
showPair a b          = show a ++ " . " ++ show b

-- Show a sequence of positive values.
showPosList :: [Pos] -> String
showPosList []     = ""
showPosList [p]    = show p
showPosList (p:ps) = show p ++ " " ++ showPosList ps

instance Show Neg where
  show n = showPosList (quote n)


------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction of two terms.
interact :: Neg -> Pos -> Maybe (Prog Neg Pos)
interact n p = case (n, p) of

  -- error propagation
  (_, PError e)                -> Just [Pos $ PError e]

  -- normalization
  (NDump, PDump)               -> Just []
  (NDump, a)                   -> Just [Pos a, Neg NDump]
  (a, PDump)                   -> Just (Pos PDump : map Pos (quote a))

  (Dup, a)                     -> Just [Pos a, Pos a]

  (Drop, _)                    -> Just []

  (Swap, a)                    -> Just [Neg $ Part Swap [a]]
  (Part Swap [a], b)           -> Just [Pos b, Pos a]

  (Cons, a)                    -> Just [Neg $ Part Cons [a]]
  (Part Cons [a], b)           -> Just [Pos $ Pair a b]

  (Uncons, Pair a b)           -> Just [Pos a, Pos b]
  (Uncons, _)                  -> Just [runtimeError "uncons: not a list: " p]

  (Tokens, PToken (Str s))     -> Just (tokens s)
  (Tokens, _)                  -> Just [runtimeError "tokens: not a string: " p]

  (List, PToken (Paren ')'))   -> Just [Pos Nil]
  (List, a)                    -> Just [Neg Cons, Pos a, Neg List]

  (Lists, End)                 -> Just [Pos End]
  (Lists, PToken (Paren '('))  -> Just [Neg List, Neg Lists]
  (Lists, a)                   -> Just [Pos a, Neg Lists]

  (Nest, End)                  -> Just [Pos Nil]
  (Nest, a)                    -> Just [Neg Cons, Pos a, Neg Nest]

  (Flat, Nil)                  -> Just [Pos End]
  (Flat, Pair a b)             -> Just [Pos a, Neg Flat, Pos b]
  (Flat, a)                    -> Just [runtimeError "flat: not a list: " a]

  (Eval, a)                    -> Just [eval a]

  (Vals, End)                  -> Just []
  (Vals, a)                    -> Just [Neg Eval, Pos a, Neg Vals]

  (Fix, f)                     -> Just [Neg Vals, Neg Flat, Pos f, Pos $ Pair (PToken (Name "fix")) $ Pair f Nil]

  (Cond, a)                    -> Just [Neg $ Part Cond [a]]
  (Part Cond [a, _], True)     -> Just [Neg Vals, Neg Flat, Pos a]
  (Part Cond [_, b], _)        -> Just [Neg Vals, Neg Flat, Pos b]
  (Part Cond [a], b)           -> Just [Neg $ Part Cond [a, b]]

  (EqTok, a)                   -> Just [Neg $ Part EqTok [a]]
  (Part EqTok [PToken t], PToken t') -> Just [Pos $ if t == t' then True else False]
  (Part EqTok [Nil], Nil)       -> Just [Pos True]
  (Part EqTok [_], _)          -> Just [Pos False]

  (NError, a)                  -> Just [Pos $ PError a]

  (NCont, PCont k)             -> Just [Neg $ Part NCont [PCont k]]
  -- (Part NCont [PCont k], a)    -> Just $ cont (k a)

  (Effect, a)                  -> Just [Neg $ Part Effect [a]]
  -- (Part Effect [a], b)         -> effect a b

  -- undefined interactions
  _                            -> Nothing

runtimeError :: String -> Pos -> Value Neg Pos
runtimeError msg t = Pos (PError $ Pair (PToken (Str msg)) $ Pair t Nil)

-- Lazily splits a string to a flat list of tokens.
tokens :: String -> Prog Neg Pos
tokens s = case s of
  [] -> [Pos End]
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in [Pos $ PToken $ Op (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | isLower c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ PToken $ Name (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | isUpper c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ PToken $ Sym (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | c `elem` "(){}[]" -> [Pos $ PToken $ Paren c, Neg Tokens, Pos $ PToken $ Str cs]
  '"' : cs -> strToken cs "\""
  c : _ -> [runtimeError "unexpected character" (PToken $ Str [c])]

strToken :: String -> String -> Prog Neg Pos
strToken ('"':cs) s = let t = read (reverse ('"':s))
                      in [Pos $ PToken $ Str t, Neg Tokens, Pos $ PToken $ Str cs]
strToken (c:cs) s   = strToken cs (c:s)
strToken [] s       = [runtimeError "unexpected end of input in string literal" (PToken $ Str s)]

opChar :: String
opChar = "+-*=!?/\\|<>$@#%^&~,.:;"

-- Evaluate a positive term.
eval :: Pos -> Value Neg Pos
eval (PToken (Name n)) = Neg (Combinator n)
eval a                 = Pos a

-- Quote a negative value.
quote :: Neg -> [Pos]
-- predefined combinators
quote (NToken t)  = [PToken t]
quote (Part n ps) = quote n ++ ps


apply :: Pos -> Prog Neg Pos
apply (Pair a b) = [Neg Vals, Neg Flat, Pos $ Pair a b]
apply a          = [eval a]
