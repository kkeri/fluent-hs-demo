-- Native effect handlers.

{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Handler where

import           Control.Exception (try)
import           Control.Monad     (when)
import           Core
import           GHC.IO.Exception  (IOErrorType (ResourceVanished),
                                    IOException (..))
import           Prelude           hiding (interact)
import           Proc
import           System.Exit       (ExitCode (..))
import           System.IO         hiding (interact)


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

pattern Defs   = Combinator "defs"
pattern Def    = Combinator "def"
pattern GetDef = Combinator "getdef"

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
    Defs          -> handleDefs p k
    Def           -> defHandler e . k $ [Neg $ NPart Def [p]]
    NPart Def [m] -> handleDef m p k
    Eval          -> handleEval p k
    GetDef        -> handleGetDef p k
    _             -> PInter n p (defHandler e . k)

  -- Detect definitions in the program.
  handleDefs :: Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDefs p k = case p of
    End                   -> defHandler e . k $ [Pos End]
    (PToken (Name "def")) -> defHandler e . k $ [Neg Def, Neg Defs]
    p                     -> defHandler e . k $ [Pos p, Neg Defs]

  -- Add definition to the environment.
  handleDef :: Pos -> Pos -> Cont Neg Pos -> Proc Neg Pos
  handleDef name body k = case (name, body) of
    (PToken (Name n), Pair h t) -> defHandler ((n, Pair h t):e) . k $ []
    (PToken (Name n), Nil)      -> defHandler ((n, Nil):e) . k $ []
    (PToken (Name _), b)        -> defHandler e . k $ [runtimeError "def: body is not a list: " b]
    (n, _)                      -> defHandler e . k $ [runtimeError "def: not a lowercase name: " n]

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
-- Terminal IO handler
------------------------------------------------------------------------------

pattern Print      = Combinator "print"
pattern LoadFile   = Combinator "loadfile"

-- Execute a process interactively and handles a couple of IO effects.
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
        isTerminal <- hIsTerminalDevice stdout
        when isTerminal $ hFlush stdout
        execIO . k $ []
  PInput _     -> return ExitSuccess
  PFinish      -> return ExitSuccess
  where

  handleInter :: Neg -> Pos -> Cont Neg Pos -> IO ExitCode
  handleInter n p k = case n of
    Print -> handlePrint p k
    LoadFile -> handleLoadFile p k
    _ -> do
      putStrLn ("undefined interaction: " ++ show n ++ " " ++ show p)
      return (ExitFailure 1)

  handlePrint :: Pos -> Cont Neg Pos -> IO ExitCode
  handlePrint p k = do
    putStr (toString p)
    isTerminal <- hIsTerminalDevice stdout
    when isTerminal $ hFlush stdout
    execIO . k $ []

  handleLoadFile :: Pos -> Cont Neg Pos -> IO ExitCode
  handleLoadFile p k = case p of
    PToken (Str path) -> do
      res <- try (readFile path)
      case res of
        Left e -> case e of
          IOError {ioe_type = ResourceVanished} ->
            execIO . k $ [runtimeError "loadfile: file not found: " p]
          _ -> do
            execIO . k $ [runtimeError "loadfile: error reading file " p]
        Right s -> do
          execIO . k $ [Pos $ PToken (Str s)]
    _ -> do
      execIO . k $ [runtimeError "loadfile: not a string: " p]

  toString :: Pos -> String
  toString p = case p of
    PToken (Str s) -> s
    _              -> show p
