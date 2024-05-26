{-# LANGUAGE PatternSynonyms #-}

module Interpreter where

import           Control.Exception (try)
import           Control.Monad     (when)

import           Core
import           Handler
import           Proc

import           GHC.IO.Exception  (IOErrorType (ResourceVanished),
                                    IOException (..))
import           System.Exit       (ExitCode (..))
import           System.IO         hiding (interact)


------------------------------------------------------------------------------
-- Kernel
------------------------------------------------------------------------------

-- A program that parses a string into a flat list of terms.
parser :: String -> Prog Neg Pos
parser s = [ Neg Lists
           , Neg Tokens
           , Pos $ PToken (Str s)
           ]

-- A program that interpreters a flat list of terms.
-- kernel (s ++ t) = (kernel s) ++ (kernel t)
ipr :: Prog Neg Pos
ipr = [Neg Vals, Neg Defs]

kernel :: String -> Prog Neg Pos
kernel s = ipr ++ parser s


------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

pattern Print    = Combinator "print"
pattern LoadFile = Combinator "loadfile"

-- Execute a process interactively and handles a couple of IO effects.
execProc :: Proc Neg Pos -> IO ExitCode
execProc pr = case pr of
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
        execProc . k $ []
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
    execProc . k $ []

  handleLoadFile :: Pos -> Cont Neg Pos -> IO ExitCode
  handleLoadFile p k = case p of
    PToken (Str path) -> do
      res <- try (readFile path)
      case res of
        Left e -> case e of
          IOError {ioe_type = ResourceVanished} ->
            execProc . k $ [runtimeError "loadfile: file not found: " p]
          _ -> do
            execProc . k $ [runtimeError "loadfile: error reading file " p]
        Right s -> do
          execProc . k $ [Pos $ PToken (Str s)]
    _ -> do
      execProc . k $ [runtimeError "loadfile: not a string: " p]

  toString :: Pos -> String
  toString p = case p of
    PToken (Str s) -> s
    _              -> show p

