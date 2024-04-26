module Main (main) where

import           System.IO

import           Core
import           Eval

loadString :: String -> Prog Tm
loadString s = [Neg ParseLists, Neg Tokens, Pos $ Text s]

execProc :: Proc Tm -> IO ()
execProc pr = case pr of
  PEffect n p _ -> error ("unhandled effect: " ++ show n ++ " " ++ show p)
  POutput t k -> do
    putStr (show t)
    putStr " "
    hFlush stdout
    execProc (k [])
  PInput _ -> return ()
  Succeed -> return ()
  PFail -> error "failure"

main :: IO ()
main = do
  s <- getContents
  let p = interactHandler interactTm . proc $ loadString s
  execProc p
