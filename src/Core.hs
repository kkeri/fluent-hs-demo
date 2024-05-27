-- Syntax of the core language.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE PatternSynonyms #-}

module Core where

import           Data.Char   (isAlphaNum, isSpace)
import           GHC.Unicode (isLower, isUpper)
import           Prelude     hiding (interact)

import           Proc

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Token
  = Name String          -- Lowercase name
  | Sym String           -- Capitalized name
  | Op String            -- Operator symbol
  | Paren Char           -- Parenthesis
  | Str String           -- String literal
  deriving (Eq)

data Neg
  = NComb Token          -- Combinator
  | NPart Neg [Pos]      -- Partial application

data Pos
  = PToken Token         -- Token
  | PPair Pos Pos        -- Building block of lists
  | PEnd                 -- End of flat list
  | PError Pos           -- Runtime error
  | PCont (Cont Neg Pos) -- Continuation

------------------------------------------------------------------------------
-- Pattern synonyms for readability
------------------------------------------------------------------------------

pattern Combinator :: String -> Neg
pattern Combinator s = NComb (Name s)

pattern Symbol :: String -> Pos
pattern Symbol s = PToken (Sym s)

pattern Dup       = Combinator "dup"
pattern Swap      = Combinator "swap"
pattern Drop      = Combinator "drop"
pattern Cons      = Combinator "cons"
pattern Uncons    = Combinator "uncons"
pattern Tokens    = Combinator "tokens"
pattern List      = Combinator "list"
pattern Lists     = Combinator "lists"
pattern Nest      = Combinator "nest"
pattern Flat      = Combinator "flat"
pattern Eval      = Combinator "eval"
pattern Vals      = Combinator "vals"
pattern Fix       = Combinator "fix"
pattern NDump     = Combinator "ndump"
pattern Cond      = Combinator "cond"
pattern Eq        = Combinator "eq"
pattern NError    = Combinator "error"
pattern NCont     = Combinator "ncont"
pattern Effect    = Combinator "effect"
pattern Neg_      = Combinator "neg"
pattern IsName    = Combinator "isname"
pattern IsSym     = Combinator "issym"
pattern IsList    = Combinator "islist"
pattern ExprKind  = Combinator "exprkind"
pattern TokenKind = Combinator "tokenkind"

pattern PNil    = Symbol "Nil"
pattern PDump   = Symbol "PDump"
pattern PTrue   = Symbol "True"
pattern PFalse  = Symbol "False"

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
  show (PToken t)  = show t
  show (PPair a b) = "(" ++ showPair a b ++ ")"
  show PEnd        = "End"
  show PNil        = "()"
  show (PError e)  = "error " ++ show e
  show (PCont _)   = "<cont>"

showPair :: Pos -> Pos -> String
showPair a (PPair b c) = show a <> " " ++ showPair b c
showPair a PNil        = show a
showPair a b           = show a ++ " . " ++ show b

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

  (Swap, a)                    -> Just [Neg $ NPart Swap [a]]
  (NPart Swap [a], b)          -> Just [Pos b, Pos a]

  (Cons, a)                    -> Just [Neg $ NPart Cons [a]]
  (NPart Cons [a], b)          -> Just [Pos $ PPair a b]

  (Uncons, PPair a b)          -> Just [Pos a, Pos b]
  (Uncons, _)                  -> Just [runtimeError "uncons: not a list: " p]

  (Tokens, PToken (Str s))     -> Just (tokens s)
  (Tokens, _)                  -> Just [runtimeError "tokens: not a string: " p]

  (List, PToken (Paren ')'))   -> Just [Pos PNil]
  (List, a)                    -> Just [Neg Cons, Pos a, Neg List]

  (Lists, PEnd)                -> Just [Pos PEnd]
  (Lists, PToken (Paren '('))  -> Just [Neg List, Neg Lists]
  (Lists, a)                   -> Just [Pos a, Neg Lists]

  (Nest, PEnd)                 -> Just [Pos PNil]
  (Nest, a)                    -> Just [Neg Cons, Pos a, Neg Nest]

  (Flat, PNil)                 -> Just [Pos PEnd]
  (Flat, PPair a b)            -> Just [Pos a, Neg Flat, Pos b]
  (Flat, a)                    -> Just [runtimeError "flat: not a list: " a]

  (Eval, a)                    -> Just [eval a]

  (Vals, PEnd)                 -> Just []
  (Vals, a)                    -> Just [Neg Eval, Pos a, Neg Vals]

  (Fix, f)                     -> Just [Neg Vals, Neg Flat, Pos f, Pos $ PPair (PToken (Name "fix")) $ PPair f PNil]

  (Cond, a)                    -> Just [Neg $ NPart Cond [a]]
  (NPart Cond [a, _], PTrue)   -> Just [Neg Vals, Neg Flat, Pos a]
  (NPart Cond [_, b], _)       -> Just [Neg Vals, Neg Flat, Pos b]
  (NPart Cond [a], b)          -> Just [Neg $ NPart Cond [a, b]]

  (Eq, a)                      -> Just [Neg $ NPart Eq [a]]
  (NPart Eq [a], b)            -> Just [Pos $ if posEq a b then PTrue else PFalse]

  (NError, a)                  -> Just [Pos $ PError a]

  (NCont, PCont k)             -> Just [Neg $ NPart NCont [PCont k]]
  -- (NPart NCont [PCont k], a)    -> Just $ cont (k a)

  (Effect, a)                  -> Just [Neg $ NPart Effect [a]]
  -- (NPart Effect [a], b)         -> effect a b

  (Neg_, PToken t)             -> Just [Neg $ NComb t]
  (Neg_, a)                    -> Just [runtimeError "neg: not a token: " a]

  (IsName, PToken (Name _))    -> Just [Pos PTrue]
  (IsName, _)                  -> Just [Pos PFalse]

  (IsSym, PToken (Sym _))      -> Just [Pos PTrue]
  (IsSym, _)                   -> Just [Pos PFalse]

  (IsList, PNil)               -> Just [Pos PTrue]
  (IsList, PPair _ _)          -> Just [Pos PTrue]
  (IsList, _)                  -> Just [Pos PFalse]

  (ExprKind, a)                -> Just [Pos $ PToken $ Sym $ posKind a]

  -- undefined interactions
  _                            -> Nothing

runtimeError :: String -> Pos -> Value Neg Pos
runtimeError msg t = Pos (PError $ PPair (PToken (Str msg)) $ PPair t PNil)

-- Lazily splits a string to a flat list of tokens.
tokens :: String -> Prog Neg Pos
tokens s = case s of
  [] -> [Pos PEnd]
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in [Pos $ PToken $ Op (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | isLower c -> let (tok, rest) = span isNameChar cs
                        in [Pos $ PToken $ Name (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | isUpper c -> let (tok, rest) = span isNameChar cs
                        in [Pos $ PToken $ Sym (c:tok), Neg Tokens, Pos $ PToken $ Str rest]
  c : cs | c `elem` "(){}[]" -> [Pos $ PToken $ Paren c, Neg Tokens, Pos $ PToken $ Str cs]
  '"' : cs -> strToken cs "\""
  c : _ -> [runtimeError "unexpected character" (PToken $ Str [c])]

strToken :: String -> String -> Prog Neg Pos
strToken ('"':cs) s = let t = read (reverse ('"':s))
                      in [Pos $ PToken $ Str t, Neg Tokens, Pos $ PToken $ Str cs]
strToken (c:cs) s   = strToken cs (c:s)
strToken [] s       = [runtimeError "unexpected end of input in string literal" (PToken $ Str s)]

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

opChar :: String
opChar = "+-*=!?/\\|<>$@#%^&~,.:;"

-- Evaluate a positive term.
eval :: Pos -> Value Neg Pos
eval (PToken (Name n))    = Neg (Combinator n)
eval (PToken (Sym "End")) = Pos PEnd
eval a                    = Pos a

-- Quote a negative value.
quote :: Neg -> [Pos]
-- predefined combinators
quote (NComb t)    = [PToken t]
quote (NPart n ps) = quote n ++ ps

apply :: Pos -> Prog Neg Pos
apply (PPair a b) = [Neg Vals, Neg Flat, Pos $ PPair a b]
apply a           = [eval a]

posKind :: Pos -> String
posKind (PToken _)  = "Token"
posKind (PPair _ _) = "Pair"
posKind PEnd        = "End"
posKind (PError _)  = "Error"
posKind (PCont _)   = "Cont"

tokenKind :: Token -> String
tokenKind (Name _)  = "Name"
tokenKind (Sym _)   = "Symbol"
tokenKind (Op _)    = "Operator"
tokenKind (Paren _) = "Paren"
tokenKind (Str _)   = "String"

posEq :: Pos -> Pos -> Bool
posEq (PToken t) (PToken t')    = t == t'
posEq (PPair a b) (PPair a' b') = posEq a a' && posEq b b'
posEq PEnd PEnd                 = True
posEq (PError e) (PError e')    = posEq e e'
posEq _ _                       = False
