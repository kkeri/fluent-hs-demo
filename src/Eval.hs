-- Language independent semantics.

module Eval where

import           Data.Monoid ()


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- Polarized value
data Value n p
  = Pos p
  | Neg n

-- Program
type Prog n p = [Value n p]

-- Continuation
type Cont n p = Prog n p -> Proc n p

-- Interaction function
type Interact n p = n -> p -> Maybe (Prog n p)

-- Effectful process - the external behavior of a program
data Proc n p
  = PInter n p (Cont n p)  -- unhandled interaction (user-defined effect)
  | POutput p (Cont n p)   -- produce output
  | PInput (Cont n p)      -- require input (when no more output can be produced)
  | PFinish                -- inactive process

-- Effect handler
type Handler n p = Proc n p -> Proc n p

------------------------------------------------------------------------------
-- Execution of a program
------------------------------------------------------------------------------

-- Create a continuation from a stack and a queue.
makeCont :: [n] -> Prog n p -> Cont n p
makeCont stk queue arg = continue stk (arg ++ queue)

-- Continue the execution of a process until reaching a breakpoint.
-- Implement "negative side to positive side" strategy.
continue :: [n] -> Prog n p -> Proc n p
continue stk queue = case (stk, queue) of
  -- request an interaction
  (n:stk, Pos p:queue) -> PInter n p (makeCont stk queue)
  -- negative values are pushed to the operator stack
  (stk, Neg n:queue)   -> continue (n:stk) queue
  -- passive positive values are sent to the output
  ([], Pos p:queue)    -> POutput p (makeCont [] queue)
  -- more queue is required
  (stk, [])            -> PInput (makeCont stk [])

-- Create an empty continuation, or a function that converts a program
-- to a process.
cont :: Cont n p
cont = continue []

-- Composition of processes.
-- The second process is prioritized over the first one to increase laziness.
-- proc (p ++ q) = proc p <> proc q
instance Semigroup (Proc n p) where
  a <> (POutput p k)          = POutput p (\v -> a <> k v)
  a <> (PInter n p k)         = PInter n p (\v -> a <> k v)
  (POutput p j) <> (PInput k) = j [] <> k [Pos p]
  (PInter n p k) <> b         = PInter n p (\v -> k v <> b)
  (PInput k) <> b             = PInput (\v -> k v <> b)
  PFinish <> b                = b
  a <> PFinish                = a

instance Monoid (Proc n p) where
  mempty = PFinish
  mappend = (<>)
