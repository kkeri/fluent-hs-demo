module Main (main) where

import           System.Exit

import           Interpreter
import           System.IO   (stdin)

main :: IO ()
main = do
  xc <- interpretHandle stdin
  exitWith xc
