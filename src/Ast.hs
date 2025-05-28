module Ast where

-- ----------------------------------------------------------------------------
-- Abstract Syntax Tree (AST)
-- ----------------------------------------------------------------------------

data Expr
  = Var String -- Variable: x
  | Lam String Expr -- Lambda Abstraction: \x -> e (can be nested by parser)
  | App Expr Expr -- Application: e1 e2
  | Let String Expr Expr -- Let Binding: let x = e1 in e2
  | Num Int -- Integer Literal: 5
  | Add Expr Expr -- Addition: e1 + e2
  | Sub Expr Expr -- Subtraction: e1 - e2
  | Mul Expr Expr -- Multiplication: e1 * e2
  | Eq Expr Expr -- Equality Check: e1 == e2 (Needed for conditions)
  | List [Expr] -- List Literal: [e1, e2]
  | Cons Expr Expr -- List Constructor: cons e1 e2
  | Head Expr -- List Head: head e
  | Tail Expr -- List Tail: tail e
  | IsEmpty Expr -- Check if list is empty: isEmpty e
  | BoolLit Bool -- Boolean Literal: True, False
  | IfThenElse Expr Expr Expr -- Conditional: if cond then expr1 else expr2
  deriving (Show, Eq)
