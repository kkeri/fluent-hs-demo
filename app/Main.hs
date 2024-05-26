{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import           Prelude     hiding (interact)
import           System.Exit

import           Core
import           Handler
import           Proc

-- A program that parses a string into a flat list of terms.
parser :: String -> Prog Neg Pos
parser s = [ Neg Lists
           , Neg Tokens
           , Pos $ PToken (Str s)
           ]

-- A program that interpreters a flat list of terms.
-- kernel (s ++ t) = (kernel s) ++ (kernel t)
kernel :: Prog Neg Pos
kernel = [Neg Vals, Neg Defs]

main :: IO ()
main = do
  s <- getContents
  xc <- execIO . interactHandler . defHandler [] . cont $ kernel ++ parser s
  exitWith xc
