{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import           Prelude     hiding (interact)
import           System.Exit

import           Core
import           Eval
import           Handler

-- Build a kernel that interpreters a string.
-- kern (s ++ t) = (kern s) ++ (kern t)
kern :: String -> Prog Neg Pos
kern s = [ Neg Vals
           , Neg $ User "defs"
             , Neg Lists
             , Neg Tokens, Pos $ Token (Str s)
             , Pos End
           , Pos End
         ]

main :: IO ()
main = do
  s <- getContents
  xc <- execIO . interactHandler interact . defHandler [] . cont $ kern s
  exitWith xc
