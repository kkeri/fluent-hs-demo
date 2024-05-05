module Main (main) where

import           System.IO

import           Core
import           Eval


------------------------------------------------------------------------------
-- Effect handlers
------------------------------------------------------------------------------

type Env = [(String, Tm)]

-- Add user defined combinators to the language.
defHandler :: Env -> Handler Tm
defHandler env pr = case pr of
  PInter n p k -> case n of
    -- partial application of `def`
    Name "def" -> defHandler env . k $ [Neg $ Pair (Name "def") p]
    -- define a combinator
    Pair (Name "def") (Name m) -> defHandler ((m, p):env) . k $ []
    -- apply a combinator
    Name m -> case lookup m env of
      Just v  -> defHandler env . k $ [Neg Apply, Pos v, Pos p]
      Nothing -> PInter n p (defHandler env . k)
    _ -> PInter n p (defHandler env . k)
  POutput t k -> POutput t (defHandler env . k)
  PInput k -> PInput (defHandler env . k)
  PFinish -> PFinish
  PError e -> PError e

-- Evaluate a string.
-- eval (s ++ t) = (eval s) ++ (eval t)
eval :: String -> Prog Tm
eval s = [Neg Pols, Neg Nest, Neg Tokens, Pos $ Str s]

execProc :: Proc Tm -> IO ()
execProc pr = case pr of
  PInter (Name "return") p _ -> do putStr "return "; print p
  PInter n p _ -> print ("error: unhandled interaction: " ++ show n ++ " " ++ show p)
  POutput t k -> do
    putStr (show t)
    putStr " "
    hFlush stdout
    execProc (k [])
  PInput _ -> return ()
  PFinish -> return ()
  PError e -> error (show e)

main :: IO ()
main = do
  s <- getContents
  let p = interactHandler interactTm . proc $ eval s
  execProc p
