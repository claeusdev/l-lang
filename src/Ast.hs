module Ast (Expression (..), Environment, Value (..), evaluator) where

import qualified Data.Map as Map

data Expression
  = Number Int
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Id String
  | Fun String Expression
  | Apply Expression Expression
  | If Expression Expression Expression -- condition, then-expr, else-expr
  | GreaterThan Expression Expression
  | LessThan Expression Expression
  | Equals Expression Expression
  deriving
    ( Show,
      Eq
    )

data Value = NumberValue Int | Closure String Expression Environment deriving (Show, Eq)

type Environment = Map.Map String Value

evaluator :: Expression -> Environment -> Value
evaluator (Number n) _ = NumberValue n
evaluator (Add left right) env =
  case (evaluator left env, evaluator right env) of
    (NumberValue n, NumberValue m) -> NumberValue (n + m)
    _ -> error "Type error: Addition requires numeric values"
evaluator (Subtract left right) env =
  case (evaluator left env, evaluator right env) of
    (NumberValue n, NumberValue m) -> NumberValue (n - m)
    _ -> error "Type error: Subtraction requires numeric values"
evaluator (Multiply left right) env =
  case (evaluator left env, evaluator right env) of
    (NumberValue n, NumberValue m) -> NumberValue (n * m)
    _ -> error "Type error: Multiplication requires numeric values"
evaluator (Divide left right) env =
  case (evaluator left env, evaluator right env) of
    (NumberValue n, NumberValue m) -> NumberValue (n `div` m)
    _ -> error "Type error: Division requires numeric values"
evaluator (Id name) env =
  case Map.lookup name env of
    Just val -> val
    Nothing -> error $ "Variable " ++ name ++ " not found in environment"
evaluator (Fun x b) env = Closure x b env
evaluator (Apply f a) env =
  case evaluator f env of
    Closure x b fEnv -> evaluator b (Map.insert x (evaluator a env) fEnv)
    _ -> error "Type error: Cannot apply a non-function value"
evaluator (If condition thenExpr elseExpr) env =
  case evaluator condition env of
    NumberValue 0 -> evaluator elseExpr env -- 0 is considered false
    NumberValue _ -> evaluator thenExpr env -- Any non-zero value is considered true
    _ -> error "Type error: Condition in If expression must evaluate to a number"
evaluator (GreaterThan l r) env =
  case (evaluator l env, evaluator r env) of
    (NumberValue a, NumberValue b) -> NumberValue (if a > b then 1 else 0)
    _ -> error "Type error: > expects numbers"
evaluator (LessThan l r) env =
  case (evaluator l env, evaluator r env) of
    (NumberValue a, NumberValue b) -> NumberValue (if a < b then 1 else 0)
    _ -> error "Type error: < expects numbers"
evaluator (Equals l r) env =
  case (evaluator l env, evaluator r env) of
    (NumberValue a, NumberValue b) -> NumberValue (if a == b then 1 else 0)
    _ -> error "Type error: == expects numbers"
