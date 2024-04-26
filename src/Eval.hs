-- Generic process semantics.

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval where


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- Values
data Value t
  = Pos t             -- positive value
  | Neg t             -- negative value
  | Fail              -- failure

-- Program
type Prog t = [Value t]

-- Continuation
type Cont t = Prog t -> Proc t

-- Interaction function
type Interact t = t -> t -> Maybe (Prog t)

-- Effectful process - the external behavior of a program
data Proc t
  = PEffect t t (Cont t)  -- perform an effect
  | POutput t (Cont t)    -- produce output
  | PInput (Cont t)       -- require input (no more output can be produced)
  | Succeed               -- success
  | PFail                 -- failure

-- Effect handler
type Handler t = Proc t -> Proc t


------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

-- Create a continuation from a stack and an input stream.
-- A stack is a list of negative values.
makeCont :: [t] -> Prog t -> Cont t
makeCont stk input arg = continue stk (arg ++ input)

-- Continue the execution of a process until reaching a breakpoint.
-- Implement "operator side to operand side" evaluation strategy.
continue :: [t] -> Prog t -> Proc t
continue stk input = case (stk, input) of
  -- interactions are converted to effects
  (n:stk, Pos p:input) -> PEffect n p (makeCont stk input)
  -- negative values are pushed to the operator stack
  (stk, Neg n:input)   -> continue (n:stk) input
  -- passive positive values are sent to the output
  ([], Pos p:input)    -> POutput p (makeCont [] input)
  -- more input is required
  (stk, [])            -> PInput (makeCont stk [])
  -- propagate failure
  (_, Fail:_)          -> PFail

-- Convert a program to a process.
proc :: Prog t -> Proc t
proc = continue []

-- Convert an interaction function to an effect handler.
interactHandler :: Interact t -> Handler t
interactHandler i (PEffect n p k) = case i n p of
    Just p' -> interactHandler i (k p')
    Nothing -> PEffect n p (interactHandler i . k)
interactHandler i (POutput t k) = POutput t (interactHandler i . k)
interactHandler i (PInput k) = PInput (interactHandler i . k)
interactHandler _ Succeed = Succeed
interactHandler _ PFail = PFail
