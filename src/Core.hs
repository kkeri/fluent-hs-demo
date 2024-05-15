-- Syntax of the core language.

module Core where

import           Data.Char   (isAlphaNum, isSpace)
import           Eval
import           GHC.Unicode (isLower, isUpper)
import           Prelude     hiding (False, True, interact)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Token
  = Name String         -- Lowercase name
  | Symbol String       -- Capitalized name
  | Op String           -- Operator symbol
  | Paren Char          -- Parenthesis
  | Str String          -- String literal
  deriving (Eq)

data Neg
  -- Predefined combinators
  = Cons                -- Prepend a term to a list
  | Uncons              -- Split a list into its head and tail
  | Dup                 -- Duplicate a term
  | Swap                -- Swap two terms
  | Drop                -- Drop a term
  | Tokens              -- Tokenize a string
  | List                -- Parse a list
  | Lists               -- Parse lists, passes anything else
  | Nest                -- Cons together the following terms until end symbol
  | Flat                -- The inverse of Nest
  | Eval                -- Evaluate a positive term
  | Vals                -- Evaluate a stream of positive terms
  | Fix                 -- Fixed-point combinator
  | NDump               -- Negative end of stack dump region
  | Cond                -- Conditionally applies one of two functions
  | EqTok               -- Token equality test
  | NError              -- Create runtime error

  | User String         -- User-defined combinator
  | Part Neg [Pos]      -- Partial application

data Pos
  -- Syntax (terms)
  = Token Token         -- Token
  | Pair Pos Pos        -- Building block of lists
  | Nil                 -- Empty list
  -- Internal terms
  | End                 -- End of a flat list
  | PDump               -- Positive end of stack dump region
  | True                -- Boolean true
  | False               -- Boolean false

  | PError Pos          -- Runtime error
  | Cont (Cont Neg Pos) -- Continuation

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

instance Show Token where
  show (Name s)   = s
  show (Symbol s) = s
  show (Op s)     = s
  show (Paren c)  = [c]
  show (Str s)    = show s

instance Show Pos where
  show (Token t)  = show t

  show (Pair a b) = "(" ++ showPair a b ++ ")"
  show Nil        = "()"
  show End        = "End"
  show PDump      = "PDump"

  show True       = "True"
  show False      = "False"
  show (PError e) = "error " ++ show e
  show (Cont _)   = "<cont>"

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
interact :: Interact Neg Pos
interact n p = case (n, p) of

  -- error propagation
  (_, PError e)                -> Just [Pos $ PError e]

  -- normalization
  (NDump, PDump)               -> Just []
  (NDump, a)                   -> Just [Pos a, Neg NDump]
  (a, PDump)                   -> Just (Pos PDump : map Pos (quote a))

  (Cons, a)                    -> Just [Neg $ Part Cons [a]]
  (Part Cons [a], b)           -> Just [Pos $ Pair a b]

  (Uncons, Pair a b)           -> Just [Pos a, Pos b]
  (Uncons, _)                  -> Just [runtimeError "uncons: not a list: " p]

  (Dup, a)                     -> Just [Pos a, Pos a]

  (Drop, _)                    -> Just []

  (Swap, a)                    -> Just [Neg $ Part Swap [a]]
  (Part Swap [a], b)           -> Just [Pos b, Pos a]

  (Tokens, Token (Str s))      -> Just (tokens s)
  (Tokens, _)                  -> Just [runtimeError "tokens: not a string: " p]

  (List, Token (Paren ')'))    -> Just [Pos Nil]
  (List, a)                    -> Just [Neg Cons, Pos a, Neg List]

  (Lists, End)                 -> Just []
  (Lists, Token (Paren '('))   -> Just [Neg List, Neg Lists]
  (Lists, a)                   -> Just [Pos a, Neg Lists]

  (Nest, End)                  -> Just [Pos Nil]
  (Nest, a)                    -> Just [Neg Cons, Pos a, Neg Nest]

  (Flat, Nil)                  -> Just [Pos End]
  (Flat, Pair a b)             -> Just [Pos a, Neg Flat, Pos b]
  (Flat, a)                    -> Just [runtimeError "flat: not a list: " a]

  (Eval, a)                    -> Just [eval a]

  (Vals, End)                  -> Just []
  (Vals, a)                    -> Just [Neg Eval, Pos a, Neg Vals]

  (Fix, f)                     -> Just [Neg Vals, Neg Flat, Pos f, Pos $ Pair (Token (Name "fix")) $ Pair f Nil]

  (Cond, a)                    -> Just [Neg $ Part Cond [a]]
  (Part Cond [a, _], True)     -> Just [Neg Vals, Neg Flat, Pos a]
  (Part Cond [_, b], _)        -> Just [Neg Vals, Neg Flat, Pos b]
  (Part Cond [a], b)           -> Just [Neg $ Part Cond [a, b]]

  (EqTok, a)                   -> Just [Neg $ Part EqTok [a]]
  (Part EqTok [Token t], Token t') -> Just [Pos $ if t == t' then True else False]
  (Part EqTok [Nil], Nil)       -> Just [Pos True]
  (Part EqTok [_], _)          -> Just [Pos False]

  (NError, a)                  -> Just [Pos $ PError a]

  -- undefined interactions
  _                            -> Nothing

runtimeError :: String -> Pos -> Value Neg Pos
runtimeError msg t = Pos (PError $ Pair (Token (Str msg)) $ Pair t Nil)

-- Lazily splits a string to a flat list of tokens.
tokens :: String -> Prog Neg Pos
tokens s = case s of
  [] -> [Pos End]
  c : cs | isSpace c -> tokens cs
  c : cs | c == '#' -> let (_, rest) = span (/= '\n') cs in tokens rest
  c : cs | c `elem` opChar -> let (tok, rest) = span (`elem` opChar) cs
                              in [Pos $ Token $ Op (c:tok), Neg Tokens, Pos $ Token $ Str rest]
  c : cs | isLower c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ Token $ Name (c:tok), Neg Tokens, Pos $ Token $ Str rest]
  c : cs | isUpper c -> let (tok, rest) = span isAlphaNum cs
                        in [Pos $ Token $ Symbol (c:tok), Neg Tokens, Pos $ Token $ Str rest]
  c : cs | c `elem` "(){}[]" -> [Pos $ Token $ Paren c, Neg Tokens, Pos $ Token $ Str cs]
  '"' : cs -> strToken cs "\""
  c : _ -> [runtimeError "unexpected character" (Token $ Str [c])]

strToken :: String -> String -> Prog Neg Pos
strToken ('"':cs) s = let t = read (reverse ('"':s))
                      in [Pos $ Token $ Str t, Neg Tokens, Pos $ Token $ Str cs]
strToken (c:cs) s   = strToken cs (c:s)
strToken [] s       = [runtimeError "unexpected end of input in string literal" (Token $ Str s)]

opChar :: String
opChar = "+-*=!?/\\|<>$@#%^&~,.:;"

-- Evaluate a positive term.
eval :: Pos -> Value Neg Pos
eval (Token (Name n))   = Neg (evalName n)
eval (Token (Symbol n)) = Pos (evalSymbol n)
eval a                  = Pos a

evalName :: String -> Neg
evalName "cons"   = Cons
evalName "uncons" = Uncons
evalName "dup"    = Dup
evalName "swap"   = Swap
evalName "drop"   = Drop
evalName "tokens" = Tokens
evalName "list"   = List
evalName "lists"  = Lists
evalName "nest"   = Nest
evalName "flat"   = Flat
evalName "eval"   = Eval
evalName "vals"   = Vals
evalName "fix"    = Fix
evalName "ndump"  = NDump
evalName "cond"   = Cond
evalName "eqtok"  = EqTok
evalName "error"  = NError
evalName s        = User s

evalSymbol :: String -> Pos
evalSymbol "End"   = End
evalSymbol "PDump" = PDump
evalSymbol "True"  = True
evalSymbol "False" = False
evalSymbol s       = Token (Symbol s)

-- Quote a negative value.
quote :: Neg -> [Pos]
-- predefined combinators
quote Cons        = [Token $ Name "cons"]
quote Uncons      = [Token $ Name "uncons"]
quote Dup         = [Token $ Name "dup"]
quote Swap        = [Token $ Name "swap"]
quote Drop        = [Token $ Name "drop"]
quote Tokens      = [Token $ Name "tokens"]
quote List        = [Token $ Name "list"]
quote Lists       = [Token $ Name "lists"]
quote Nest        = [Token $ Name "nest"]
quote Flat        = [Token $ Name "flat"]
quote Eval        = [Token $ Name "eval"]
quote Vals        = [Token $ Name "vals"]
quote NDump       = [Token $ Name "ndump"]
quote Fix         = [Token $ Name "fix"]
quote Cond        = [Token $ Name "cond"]
quote EqTok       = [Token $ Name "eqtok"]
quote NError      = [Token $ Name "error"]
quote (User s)    = [Token $ Name s]
quote (Part n ps) = quote n ++ ps
