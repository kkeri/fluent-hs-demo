-- Language independent semantics.

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval where

import           Data.Monoid ()


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- Values
data Value t
  = Pos t             -- positive value
  | Neg t             -- negative value
  | Error t           -- failure with reason

-- Program
type Prog t = [Value t]

-- Continuation
type Cont t = Prog t -> Proc t

-- Interaction function
type Interact t = t -> t -> Maybe (Prog t)

-- Effectful process - the external behavior of a program
data Proc t
  = PInter t t (Cont t)   -- unhandled interaction (user-defined effect)
  | POutput t (Cont t)    -- produce output
  | PInput (Cont t)       -- require input (when no more output can be produced)
  | PFinish               -- inactive process
  | PError t              -- failed process

-- Effect handler
type Handler t = Proc t -> Proc t


------------------------------------------------------------------------------
-- Execution of a program
------------------------------------------------------------------------------

-- Create a continuation from a stack and an input stream.
-- A stack is a list of negative values.
makeCont :: [t] -> Prog t -> Cont t
makeCont stk input arg = continue stk (arg ++ input)

-- Continue the execution of a process until reaching a breakpoint.
-- Implement "operator side to operand side" evaluation strategy.
continue :: [t] -> Prog t -> Proc t
continue stk input = case (stk, input) of
  -- request an interaction
  (n:stk, Pos p:input) -> PInter n p (makeCont stk input)
  -- negative values are pushed to the operator stack
  (stk, Neg n:input)   -> continue (n:stk) input
  -- passive positive values are sent to the output
  ([], Pos p:input)    -> POutput p (makeCont [] input)
  -- more input is required
  (stk, [])            -> PInput (makeCont stk [])
  -- propagate failure
  (_, Error e:_)       -> PError e

-- Convert an interaction function to an effect handler.
interactHandler :: Interact t -> Handler t
interactHandler i (PInter n p k) = case i n p of
    Just p' -> interactHandler i (k p')
    Nothing -> PInter n p (interactHandler i . k)
interactHandler i (POutput t k) = POutput t (interactHandler i . k)
interactHandler i (PInput k) = PInput (interactHandler i . k)
interactHandler _ PFinish = PFinish
interactHandler _ (PError e) = PError e

-- Create an empty continuation. Convert a program to a process.
proc :: Cont t
proc = continue []

-- Composition of processes.
-- The second process is prioritized over the first one to increase laziness.
-- proc (p ++ q) = proc p <> proc q
instance Semigroup (Proc t) where
  p <> (POutput a k)          = POutput a (\t -> p <> k t)
  p <> (PInter a b k)         = PInter a b (\t -> p <> k t)
  (POutput a j) <> (PInput k) = j [] <> k [Pos a]
  (PInter n p k) <> q         = PInter n p (\t -> k t <> q)
  (PInput k) <> q             = PInput (\t -> k t <> q)
  PFinish <> q                = q
  p <> PFinish                = p
  (PError e) <> _             = PError e
  _ <> (PError e)             = PError e

instance Monoid (Proc t) where
  mempty = PFinish
  mappend = (<>)

