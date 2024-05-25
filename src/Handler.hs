-- Native effect handlers.

{-# LANGUAGE PatternSynonyms #-}

module Handler where

import           Core
import           Eval
import           Prelude     hiding (interact)
import           System.Exit (ExitCode (..))


------------------------------------------------------------------------------
-- Interaction handler
------------------------------------------------------------------------------

-- Wraps the interaction function into an effect handler.
-- Also converts embedded effects to native effects.
interactHandler :: Handler Neg Pos
interactHandler pr = case pr of
  -- PInter Effect p            -> interactHandler . k $ [Neg $ Part Effect [p]]
  -- PInter (Part Effect [a]) p -> PEffect a p
  PInter n p k  -> case interact n p of
    Just p' -> interactHandler . k $ p'
    -- fall through to the next handler
    Nothing -> PInter n p (interactHandler . k)
  POutput v k   -> POutput v (interactHandler . k)
  PInput k      -> PInput (interactHandler . k)
  PFinish       -> PFinish


------------------------------------------------------------------------------
-- User definitions
------------------------------------------------------------------------------

pattern Defs :: Neg
pattern Defs = NToken (Name "defs")

pattern Def :: Neg
pattern Def = NToken (Name "def")

pattern GetDef :: Neg
pattern GetDef = NToken (Name "getdef")

pattern Print :: Neg
pattern Print = NToken (Name "print")

-- Usage:
-- - Insert a `defHandler` into the handler chain.
-- - Insert `defs` into the kernel between list parsing and evaluation.
-- - Use the `def <name> <body>` syntax to add definitions.

type Env = [(String, Pos)]

-- Add user defined combinators to the language.
defHandler :: Env -> Handler Neg Pos
defHandler e pr = case pr of
  PInter n p k -> handleInter n p k
  POutput t k  -> POutput t (defHandler e . k)
  PInput k     -> PInput (defHandler e . k)
  PFinish      -> PFinish
  where

  handleInter :: Neg -> Pos -> Cont Neg Pos -> Proc Neg Pos
  handleInter n p k = case n of
    Defs         -> handleDefs p k
    Def          -> defHandler e . k $ [Neg $ Part Def [p]]
    Part Def [m] -> handleDef m p k
    Eval         -> handleEval p k
    GetDef       -> handleGetDef p k
    _            -> PInter n p (defHandler e . k)

  -- Detect definitions in the program.
  handleDefs :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDefs p k = case p of
    End                   -> defHandler e . k $ []
    (PToken (Name "def")) -> defHandler e . k $ [Neg Def, Neg Defs]
    p                     -> defHandler e . k $ [Pos p, Neg Defs]

  -- Add definition to the environment.
  handleDef :: Pos -> Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDef name body k = case name of
    PToken (Name n) -> defHandler ((n, body):e) . k $ []
    _              -> defHandler e . k $ [runtimeError "def: not a lowercase name: " name]

  -- Resolve a definition.
  handleEval :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleEval p k = case p of
    PToken (Name n) -> case lookup n e of
      Just v  -> defHandler e . k $ [Neg Vals, Neg Flat, Pos v]
      -- fall through to the next handler
      Nothing -> PInter Eval p (defHandler e . k)
    _              -> PInter Eval p (defHandler e . k)

  -- Get a definition by name but do not apply it.
  handleGetDef :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleGetDef name k = case name of
    PToken (Str n) -> case lookup n e of
      Just v  -> defHandler e . k $ [Pos v]
      Nothing -> defHandler e . k $ [runtimeError "def: not found: " name]
    _             -> defHandler e . k $ [runtimeError "def: not a lowercase name: " name]

------------------------------------------------------------------------------
-- Execute a process interactively.
------------------------------------------------------------------------------

execIO :: Proc Neg Pos -> IO ExitCode
execIO pr = case pr of
  PInter n p k -> handleInter n p k
  POutput t k  -> do
    case t of
      PError p -> do
        putStr "error: "
        print p
        return (ExitFailure 1)
      _ -> do
        putStr (show t)
        putStr " "
        execIO (k [])
  PInput _     -> return ExitSuccess
  PFinish      -> return ExitSuccess
  where

  handleInter :: Neg -> Pos -> Cont Neg Pos -> IO ExitCode
  handleInter n p k = case n of
    Print -> do
      putStr (toString p)
      execIO (k [])
    _ -> do
      putStrLn ("undefined interaction: " ++ show n ++ " " ++ show p)
      return (ExitFailure 1)

  toString :: Pos -> String
  toString p = case p of
    PToken (Str s) -> s
    _              -> show p
