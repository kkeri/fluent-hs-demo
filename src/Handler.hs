-- Native effect handlers.

module Handler where

import           Core
import           Eval
import           System.Exit (ExitCode (..))


------------------------------------------------------------------------------
-- Interaction handler
------------------------------------------------------------------------------

-- Convert an interaction function to an effect handler.
interactHandler :: Interact n p -> Handler n p
interactHandler interact pr = case pr of
  PInter n p k  -> case interact n p of
    Just p' -> interactHandler interact . k $ p'
    -- fall through to the next handler
    Nothing -> PInter n p (interactHandler interact . k)
  POutput v k   -> POutput v (interactHandler interact . k)
  PInput k      -> PInput (interactHandler interact . k)
  PFinish       -> PFinish


------------------------------------------------------------------------------
-- User definitions
------------------------------------------------------------------------------

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
    User "defs"           -> handleDefs p k
    User "def"            -> defHandler e . k $ [Neg $ Part (User "def") [p]]
    Part (User "def") [m] -> handleDef m p k
    Eval                  -> handleEval p k
    User "getdef"         -> handleGetDef p k
    _                     -> PInter n p (defHandler e . k)

  -- Detect definitions in the program.
  handleDefs :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDefs p k = case p of
    End                  -> defHandler e . k $ []
    (Token (Name "def")) -> defHandler e . k $ [Neg $ User "def", Neg $ User "defs"]
    p                    -> defHandler e . k $ [Pos p, Neg $ User "defs"]

  -- Add definition to the environment.
  handleDef :: Pos -> Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDef name body k = case name of
    Token (Name n) -> defHandler ((n, body):e) . k $ []
    _              -> defHandler e . k $ [runtimeError "def: not a lowercase name: " name]

  -- Resolve a definition.
  handleEval :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleEval p k = case p of
    Token (Name n) -> case lookup n e of
      Just v  -> defHandler e . k $ [Neg Vals, Neg Flat, Pos v]
      -- fall through to the next handler
      Nothing -> PInter Eval p (defHandler e . k)
    _              -> PInter Eval p (defHandler e . k)

  -- Get a definition by name but do not apply it.
  handleGetDef :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleGetDef name k = case name of
    Token (Str n) -> case lookup n e of
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
    User "print" -> do
      putStr (toString p)
      execIO (k [])
    _ -> do
      putStrLn ("undefined interaction: " ++ show n ++ " " ++ show p)
      return (ExitFailure 1)

  toString :: Pos -> String
  toString p = case p of
    Token (Str s) -> s
    _             -> show p
