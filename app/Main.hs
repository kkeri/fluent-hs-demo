module Main (main) where

import           System.Exit

import           Handler
import           Interpreter
import           Proc

main :: IO ()
main = do
  s <- getContents
  xc <- execProc . interactHandler . defHandler [] . cont $ kernel s
  exitWith xc
