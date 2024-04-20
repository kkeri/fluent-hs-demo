{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib
    ( tokenize
    , runTm
    , Tm(..)
    ) where


import           Control.Applicative   (some)
import           Data.Char
import           Data.Functor.Identity
import           Prelude               hiding (interact)
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Token


------------------------------------------------------------------------------
-- Tokenizer
------------------------------------------------------------------------------

-- Tokens
data Tok
  = Sym String      -- Symbol name (starts with uppercase)
  | Ident String    -- Identifier (starts with lowercase)
  | Op String       -- Operator symbol
  | Bracket Char    -- Bracket
  deriving Eq

opChar :: [Char]
opChar = "+-*=!?/\\|<>$@#%^&~:"
nameChar = alphaNum <|> char '_'

languageDef =
  emptyDef { Token.caseSensitive   = True
           , Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "--"
           , Token.nestedComments  = True
           , Token.identStart      = letter
           , Token.identLetter     = nameChar
           , Token.reservedOpNames = ["==", "|>", "&", "|", "->", ">>", ";", ":"]
           , Token.reservedNames   = [ "throw" ]
           }

lexer = Token.makeTokenParser languageDef

identifier  =  Token.identifier     lexer
operator :: Parsec String st String
operator    =  some (oneOf opChar)
reservedOp  =  Token.reservedOp     lexer
reserved    =  Token.reserved       lexer
parens      =  Token.parens         lexer
braces      =  Token.braces         lexer
signed      = do s <- option "" (string "-"); i <- unsigned; return (read (s ++ show i) :: Int)
unsigned    =  Token.natural        lexer
stringLiteral = Token.stringLiteral lexer
whitespace  =  Token.whiteSpace     lexer
var         =  char '$' >> some nameChar

-- Parse a token
pToken :: ParsecT String u Identity Tok
pToken = choice
  [ do i <- identifier
       if isUpper (head i) then return (Sym i) else return (Ident i)
  , Op <$> operator
  , Bracket <$> oneOf "(){}[]"
  ]

-- Parse a list of tokens
tokenize :: String -> String -> Either ParseError [Tok]
tokenize = parse (only (many pToken))


------------------------------------------------------------------------------
-- Printer
------------------------------------------------------------------------------

instance Show Tok where
  show (Sym s)     = s
  show (Ident s)   = s
  show (Op s)      = s
  show (Bracket c) = [c]


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- Core syntax
data Tm

  -- Tokens
  = Tok Tok           -- Token

  -- list
  | Pair Tm Tm        -- list
  | Nil               -- Empty list

  -- Combinators
  | Cons               -- Prepends a term to a list
  | Decons             -- Splits a list into its head and tail
  | Dup                -- duplicate the following term
  | Swap               -- swap the two following terms
  | Drop               -- drop the following term
  | Global             -- Global context, for top level recursion
  | Mng                -- Meaning of the following term
  | MngList            -- Meaning of the following list of terms
  | Fix                -- Fixed-point combinator
  | Call               -- Flatten and interpret the following list of terms

  -- Special terms
  | Any                -- Matches any term
  | Fail               -- Local failure

-- Primary values
data VPrim
  = Pos Tm             -- positive term
  | Neg Tm             -- negative term

type Global = Tm

-- Programs
-- Just []: empty/successful program
-- Nothing: failed program
type Prog = Maybe [VPrim]

-- Continuation
type Cont = Prog -> BreakPoint

-- Interaction function
type Interact = Tm -> Tm -> Prog

-- Effect handler
type Handler = Tm -> Tm -> Cont -> BreakPoint

-- Transition relation


-- Evaluator breakpoint
data BreakPoint
  = SInter Tm Tm Cont  -- an interaction is requested
  | SOutput Tm Cont    -- an output term has been produced
  | SInput Cont        -- input is required, no more output can be produced
  | SFail              -- evaluation has failed


------------------------------------------------------------------------------
-- Interactions
------------------------------------------------------------------------------

-- Compute the interaction between two terms.
interact :: Global -> Tm -> Tm -> Prog
interact g n p = case (n, p) of

  -- tokens, any
  (Tok a, Tok b)     -> if a == b then Just [] else Nothing
  (Any, Tok _)       -> Just []
  (Tok _, Any)       -> Just []

  -- combinators
  (Global, _)        -> mng g
  (Cons, a)          -> Just [Neg $ Pair Cons a]
  (Pair Cons a, b)   -> Just [Pos $ Pair a b]
  (Decons, Pair a b) -> Just [Pos a, Pos b]
  (Dup, a)           -> Just [Pos a, Pos a]
  (Drop, a)          -> Just []
  (Swap, a)          -> Just [Neg $ Pair Swap a]
  (Pair Swap a, b)   -> Just [Pos b, Pos a]
  (Fix, a)           -> Just [Neg Call, Pos a, Pos $ Pair Fix a]
  (Call, a)          -> call a

  -- meaning of terms
  (Mng, Fail)        -> Nothing
  (Mng, a)           -> Just (mng a)
  (MngList, a)       -> Just (mngList a)

  -- default
  _                  -> Nothing

-- Computes the meaning of a flattened list.
call :: Tm -> Prog
call (Pair a b) = Just (mng a ++ call b)
call Nil        = Just []


------------------------------------------------------------------------------
-- Meaning of terms
------------------------------------------------------------------------------

mng :: Tm -> [VPrim]
-- tokens
mng (Tok t) = mngTok t
-- combinators
mng Cons    = [Neg Cons]
mng Nil     = [Pos Nil]
mng Decons  = [Neg Decons]
mng Dup     = [Neg Dup]
mng Drop    = [Neg Drop]
mng Swap    = [Neg Swap]
mng Global  = [Neg Global]
mng Mng     = [Neg Mng]
mng MngList = [Neg MngList]
mng Fix     = [Neg Fix]
-- special terms
mng Any     = [Pos Any]
-- everything else is a positive term
mng a       = [Pos a]

mngTok :: Tok -> [VPrim]
-- special tokens
mngTok (Bracket '{')  = [Neg MngList]
mngTok (Sym "cons")   = [Pos Cons]
mngTok (Sym "decons") = [Pos Decons]
mngTok (Sym "dup")    = [Pos Dup]
mngTok (Sym "drop")   = [Pos Drop]
mngTok (Sym "swap")   = [Pos Swap]
mngTok (Sym "global") = [Pos Global]
mngTok (Sym "mng")    = [Pos Mng]
mngTok (Sym "fix")    = [Pos Fix]
mngTok (Sym "call")   = [Pos Call]
mngTok (Sym "any")    = [Pos Any]
mngTok (Sym "Nil")    = [Pos Nil]
mngTok (Sym "Fail")   = [Pos Fail]
-- ordinary tokens
mngTok t              = [Pos (Tok t)]

-- Parse a list of terms inside curly braces
mngList :: Tm -> [VPrim]
mngList (Tok (Bracket '}')) = Nil
mngList a                   = mng a


------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

-- Continue evaluation of a program using rightmost reduction strategy
-- until a breakpoint is reached.
continue :: [VPrim] -> [Tm] -> Cont
continue _ _ Nothing = SFail
continue rest st (Just arg) = continue' (arg ++ rest) st
  where
  continue' rest st = case (rest, st) of
    (Neg t:rest, st)   -> continue' rest (t:st)          -- negative terms are pushed to the program stack
    (Pos t:rest, u:st) -> SInter t u (continue rest st) -- positive active terms interact with the top of the stack
    (Pos t:rest, [])   -> SOutput t (continue rest []) -- positive passive terms are sent to the output
    ([], st)           -> SInput (continue [] st)    -- no more input, input is required

-- Run a program on an input stream and produce an output stream.
run :: Handler -> Prog -> [Tm] -> [Tm]
run handle p inp = run' (continue (map Pos inp) [] p)
  where
  run' (SInter p n k) = run' (handle p n k)
  run' (SOutput p k)  = p : run' (k (Just []))
  run' (SInput _)     = []      -- the whole input stream is consumed, stop
  run' SFail          = [Fail]  -- don't revoke the output so far but stop with a failure

-- Creates an effect handler from an interaction handler.
interact2handler :: Interact -> Handler
interact2handler ih n p k = k (ih n p)

-- Run a recursive program on an input stream and produce an output stream.
runTm :: Tm -> [Tm] -> [Tm]
runTm g = run (interact2handler (interact g)) Fail (mng Forth g)
