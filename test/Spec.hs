{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Ast
import Control.Exception
import qualified Data.Map as Map
import Parser
import qualified ParserTest
import Test.HUnit
import qualified TestRunner

-- Helper function to create test environment
env :: [(String, Value)] -> Environment
env = Map.fromList

-- Some useful abbreviations for expressions
num :: Int -> Expression
num = Number

add :: Expression -> Expression -> Expression
add = Add

sub :: Expression -> Expression -> Expression
sub = Subtract

mul :: Expression -> Expression -> Expression
mul = Multiply

div :: Expression -> Expression -> Expression
div = Divide

var :: String -> Expression
var = Id

lambda :: String -> Expression -> Expression
lambda = Fun

app :: Expression -> Expression -> Expression
app = Apply

-- Examples of simple expressions
simpleNum :: Expression
simpleNum = Number 5

simpleAdd = Add (Number 3) (Number 4)

simpleSub = Subtract (Number 10) (Number 4)

simpleMul :: Expression
simpleMul = Multiply (Number 3) (Number 5)

simpleDiv :: Expression
simpleDiv = Divide (Number 20) (Number 4)

simpleId :: Expression
simpleId = Id "x"

-- Examples of more complex expressions
complexExpr1 = Add (Multiply (Number 2) (Number 3)) (Number 5) -- 2*3+5 = 11

complexExpr2 = Subtract (Multiply (Number 10) (Number 2)) (Number 5) -- 10*2-5 = 15

complexExpr3 = Divide (Add (Number 8) (Number 4)) (Number 3) -- (8+4)/3 = 4

-- Function examples
identityFn = Fun "x" (Id "x") -- λx.x

constFn = Fun "x" (Fun "y" (Id "x")) -- λx.λy.x

addFn = Fun "x" (Fun "y" (Add (Id "x") (Id "y"))) -- λx.λy.x+y

-- Application examples
applyId = Apply identityFn (Number 42) -- (λx.x) 42 = 42

applyConst = Apply (Apply constFn (Number 5)) (Number 10) -- (λx.λy.x) 5 10 = 5

applyAdd = Apply (Apply addFn (Number 3)) (Number 4) -- (λx.λy.x+y) 3 4 = 7

-- Let binding examples (desugared to lambda applications)
letExpr1 = Apply (Fun "x" (Add (Id "x") (Number 1))) (Number 5) -- let x = 5 in x + 1 = 6

letExpr2 = Apply (Fun "x" (Apply (Fun "y" (Add (Id "x") (Id "y"))) (Number 2))) (Number 3) -- let x = 3 in let y = 2 in x + y = 5

-- Basic test list
basicTests =
  TestList
    [ "test_eval_number" ~: evaluator (Number 42) Map.empty ~?= NumberValue 42,
      "test_eval_addition" ~: evaluator simpleAdd Map.empty ~?= NumberValue 7
    ]

-- Tests for the evaluator
evalTests =
  TestList
    [ "test_eval_subtraction" ~: evaluator simpleSub Map.empty ~?= NumberValue 6,
      "test_eval_multiplication" ~: evaluator simpleMul Map.empty ~?= NumberValue 15,
      "test_eval_division" ~: evaluator simpleDiv Map.empty ~?= NumberValue 5,
      "test_eval_id_found" ~: evaluator simpleId (env [("x", NumberValue 10)]) ~?= NumberValue 10,
      "test_eval_complex1" ~: evaluator complexExpr1 Map.empty ~?= NumberValue 11,
      "test_eval_complex2" ~: evaluator complexExpr2 Map.empty ~?= NumberValue 15,
      "test_eval_complex3" ~: evaluator complexExpr3 Map.empty ~?= NumberValue 4,
      "test_eval_identity" ~: evaluator applyId Map.empty ~?= NumberValue 42,
      "test_eval_const" ~: evaluator applyConst Map.empty ~?= NumberValue 5,
      "test_eval_add_fn" ~: evaluator applyAdd Map.empty ~?= NumberValue 7,
      "test_eval_let1" ~: evaluator letExpr1 Map.empty ~?= NumberValue 6,
      "test_eval_let2" ~: evaluator letExpr2 Map.empty ~?= NumberValue 5
    ]

-- Tests for the parser
parserTests :: Test
parserTests =
  TestList
    [ "test_parse_number" ~: parseExpr "42" ~?= Right (Number 42),
      "test_parse_addition" ~: parseExpr "3 + 4" ~?= Right (Add (Number 3) (Number 4)),
      "test_parse_subtraction" ~: parseExpr "10 - 4" ~?= Right (Subtract (Number 10) (Number 4)),
      "test_parse_multiplication" ~: parseExpr "3 * 5" ~?= Right (Multiply (Number 3) (Number 5)),
      "test_parse_division" ~: parseExpr "20 / 4" ~?= Right (Divide (Number 20) (Number 4)),
      "test_parse_compound" ~: parseExpr "2 * 3 + 5" ~?= Right (Add (Multiply (Number 2) (Number 3)) (Number 5)),
      "test_parse_parentheses" ~: parseExpr "2 * (3 + 5)" ~?= Right (Multiply (Number 2) (Add (Number 3) (Number 5))),
      "test_parse_lambda" ~: parseExpr "lambda x. x" ~?= Right (Fun "x" (Id "x")),
      "test_parse_nested_lambda" ~: parseExpr "lambda x. lambda y. x" ~?= Right (Fun "x" (Fun "y" (Id "x"))),
      "test_parse_application" ~: parseExpr "f x" ~?= Right (Apply (Id "f") (Id "x")),
      "test_parse_multi_application" ~: parseExpr "f x y" ~?= Right (Apply (Apply (Id "f") (Id "x")) (Id "y")),
      "test_parse_let" ~: parseExpr "let x = 5 in x + 1" ~?= Right (Apply (Fun "x" (Add (Id "x") (Number 1))) (Number 5))
    ]

-- Error handling tests
errorTests :: Test
errorTests =
  TestList
    [ "test_eval_id_not_found" ~: TestCase $ do
        result <- try (evaluate (evaluator simpleId Map.empty))
        case result of
          Left (SomeException _) -> return ()
          Right _ -> assertFailure "Expected an exception for undefined variable"
    ]

-- Environment tests
envTests :: Test
envTests =
  TestList
    [ "test_eval_nested_env"
        ~: evaluator
          ( Apply
              (Fun "x" (Add (Id "x") (Id "y")))
              (Number 5)
          )
          (env [("y", NumberValue 3)])
        ~?= NumberValue 8,
      "test_nested_environments"
        ~: evaluator
          ( Apply
              ( Fun
                  "x"
                  ( Apply
                      (Fun "y" (Add (Id "x") (Id "y")))
                      (Number 3)
                  )
              )
              (Number 5)
          )
          Map.empty
        ~?= NumberValue 8,
      "test_shadowing"
        ~: evaluator
          ( Apply
              ( Fun
                  "x"
                  ( Apply
                      (Fun "x" (Id "x"))
                      (Number 10)
                  )
              )
              (Number 5)
          )
          Map.empty
        ~?= NumberValue 10
    ]

-- Advanced parser tests
advancedParserTests :: Test
advancedParserTests =
  TestList
    [ "test_mult_precedence" ~: parseExpr "2 + 3 * 4" ~?= Right (Add (Number 2) (Multiply (Number 3) (Number 4))),
      "test_div_precedence" ~: parseExpr "10 - 8 / 2" ~?= Right (Subtract (Number 10) (Divide (Number 8) (Number 2))),
      "test_chained_addition" ~: parseExpr "1 + 2 + 3" ~?= Right (Add (Add (Number 1) (Number 2)) (Number 3)),
      "test_chained_subtraction" ~: parseExpr "10 - 5 - 2" ~?= Right (Subtract (Subtract (Number 10) (Number 5)) (Number 2)),
      "test_mixed_ops1" ~: parseExpr "2 * 3 + 4 * 5" ~?= Right (Add (Multiply (Number 2) (Number 3)) (Multiply (Number 4) (Number 5))),
      "test_apply_with_arithmetic" ~: parseExpr "f (x + y)" ~?= Right (Apply (Id "f") (Add (Id "x") (Id "y"))),
      "test_let_with_arithmetic"
        ~: parseExpr "let x = 10 in x * 2 + 5"
        ~?= Right (Apply (Fun "x" (Add (Multiply (Id "x") (Number 2)) (Number 5))) (Number 10))
    ]

-- Combine all tests
allTests :: Test
allTests =
  TestList
    [ basicTests,
      evalTests,
      parserTests,
      errorTests,
      envTests,
      advancedParserTests,
      ParserTest.tests,
      TestRunner.runnerTests
    ]

-- Run all tests
runEvalTests :: IO Counts
runEvalTests = runTestTT allTests

-- Main function for test runner
main :: IO ()
main = do
  counts <- runEvalTests
  return ()
