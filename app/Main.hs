module Main (main) where

import qualified Data.Map as Data
import qualified Data.Map as Map

data Expression
  = Number Int
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Val String Expression Expression
  | Id String
  deriving
    ( Show,
      Eq
    )

type Environment = Data.Map String Int

evaluator :: Expression -> Environment -> Int
evaluator (Number n) _ = n
evaluator (Add x y) env = evaluator x env + evaluator y env
evaluator (Subtract x y) env = evaluator x env - evaluator y env
evaluator (Multiply x y) env = evaluator x env * evaluator y env
evaluator (Divide x y) env = evaluator x env `div` evaluator y env
evaluator (Val x identifier body) env =
  let newEnv = Map.insert x (evaluator identifier env) env
   in evaluator body newEnv
evaluator (Id name) env =
  case Map.lookup name env of
    Just val -> val
    Nothing -> error $ "Variable " ++ name ++ " not found in environment"

main :: IO ()
main = do
  let env = Map.empty
  let add = Val "x" (Number 10) (Add (Id "x") (Number 5))
   in print $ evaluator add env
