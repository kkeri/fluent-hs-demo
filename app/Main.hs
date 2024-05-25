{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import           Prelude     hiding (interact)
import           System.Exit

import           Core
import           Handler
import           Proc

-- Build a kernel that interpreters a string.
-- kern (s ++ t) = (kern s) ++ (kern t)
kern :: String -> Prog Neg Pos
kern s = [ Neg Vals
         , Neg Defs
         , Neg Lists
         , Neg Tokens
         , Pos $ PToken (Str s)
         ]

main :: IO ()
main = do
  s <- getContents
  xc <- execIO . interactHandler . defHandler [] . cont $ kern s
  exitWith xc
