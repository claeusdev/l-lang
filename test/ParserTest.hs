module ParserTest where

import Ast
import qualified Data.Map as Map
import Parser (parseExpr)
import Test.HUnit

-- Helper function to run parser on a string and return the result
testParse :: String -> Either String Expression
testParse input = case parseExpr input of
  Left err -> Left (show err)
  Right expr -> Right expr

-- Basic arithmetic tests
testNumber :: Test
testNumber = TestCase $ do
  assertEqual "Parse a simple number" (Right (Number 42)) (testParse "42")

testAddition :: Test
testAddition = TestCase $ do
  assertEqual
    "Parse simple addition"
    (Right (Add (Number 1) (Number 2)))
    (testParse "1 + 2")

testMultiplication :: Test
testMultiplication = TestCase $ do
  assertEqual
    "Parse simple multiplication"
    (Right (Multiply (Number 3) (Number 4)))
    (testParse "3 * 4")

testArithmeticPrecedence :: Test
testArithmeticPrecedence = TestCase $ do
  assertEqual
    "Check operator precedence"
    (Right (Add (Number 1) (Multiply (Number 2) (Number 3))))
    (testParse "1 + 2 * 3")

  assertEqual
    "Parentheses override precedence"
    (Right (Multiply (Add (Number 1) (Number 2)) (Number 3)))
    (testParse "(1 + 2) * 3")

-- Variable and function tests
testVariable :: Test
testVariable = TestCase $ do
  assertEqual
    "Parse variable reference"
    (Right (Id "x"))
    (testParse "x")

testLambda :: Test
testLambda = TestCase $ do
  assertEqual
    "Parse lambda expression"
    (Right (Fun "x" (Add (Id "x") (Number 1))))
    (testParse "lambda x. x + 1")

  assertEqual
    "Parse nested lambda"
    (Right (Fun "x" (Fun "y" (Add (Id "x") (Id "y")))))
    (testParse "lambda x. lambda y. x + y")

testApplication :: Test
testApplication = TestCase $ do
  assertEqual
    "Parse simple function application"
    (Right (Apply (Id "f") (Number 1)))
    (testParse "f 1")

  assertEqual
    "Parse function application chain"
    (Right (Apply (Apply (Id "f") (Number 1)) (Number 2)))
    (testParse "f 1 2")

  assertEqual
    "Parse function with arithmetic"
    (Right (Apply (Id "f") (Add (Number 1) (Number 2))))
    (testParse "f (1 + 2)")

-- Let binding tests
testLet :: Test
testLet = TestCase $ do
  assertEqual
    "Parse simple let binding"
    (Right (Apply (Fun "x" (Id "x")) (Number 42)))
    (testParse "let x = 42 in x")

  assertEqual
    "Parse let with expression in body"
    (Right (Apply (Fun "x" (Multiply (Id "x") (Number 2))) (Number 10)))
    (testParse "let x = 10 in x * 2")

  assertEqual
    "Parse let with arithmetic in binding"
    (Right (Apply (Fun "x" (Id "x")) (Add (Number 1) (Number 2))))
    (testParse "let x = 1 + 2 in x")

-- Function definition tests
testFunctionDef :: Test
testFunctionDef = TestCase $ do
  assertEqual
    "Parse simple function definition"
    (Right (Apply (Fun "double" (Number 0)) (Fun "x" (Multiply (Id "x") (Number 2)))))
    (testParse "func double x = x * 2")

  assertEqual
    "Parse function with multiple params"
    (Right (Apply (Fun "add" (Number 0)) (Fun "x" (Fun "y" (Add (Id "x") (Id "y"))))))
    (testParse "func add x y = x + y")

  assertEqual
    "Parse multiple function definitions"
    ( Right
        ( Apply
            (Fun "f" (Apply (Fun "g" (Number 0)) (Fun "y" (Add (Id "y") (Number 1)))))
            (Fun "x" (Multiply (Id "x") (Number 2)))
        )
    )
    (testParse "func f x = x * 2\nfunc g y = y + 1")

-- Program with function and expression tests
testProgram :: Test
testProgram = TestCase $ do
  assertEqual
    "Parse program with function and usage"
    ( Right
        ( Apply
            (Fun "square" (Apply (Id "square") (Number 5)))
            (Fun "x" (Multiply (Id "x") (Id "x")))
        )
    )
    (testParse "func square x = x * x\nsquare 5")

-- Complex expression tests
testComplex :: Test
testComplex = TestCase $ do
  assertEqual
    "Parse complex nested expression"
    ( Right
        ( Apply
            (Fun "x" (Apply (Fun "y" (Add (Id "x") (Id "y"))) (Number 2)))
            (Number 1)
        )
    )
    (testParse "(lambda x. (lambda y. x + y) 2) 1")

-- Whitespace handling tests
testWhitespace :: Test
testWhitespace = TestCase $ do
  assertEqual
    "Handle various whitespace"
    (Right (Add (Number 1) (Number 2)))
    (testParse "1   +     2")

  assertEqual
    "Handle newlines and tabs"
    (Right (Add (Number 1) (Number 2)))
    (testParse "1\n+\t2")

-- Comment tests
testComments :: Test
testComments = TestCase $ do
  assertEqual
    "Handle line comments"
    (Right (Add (Number 1) (Number 2)))
    (testParse "1 + 2 # This is a comment")

  assertEqual
    "Handle comment at beginning"
    (Right (Add (Number 1) (Number 2)))
    (testParse "# Comment at start\n1 + 2")

-- Test evaluation (optional, if you want to test the full pipeline)
testEvaluation :: Test
testEvaluation = TestCase $ do
  let expr = case parseExpr "let x = 10 in x * 2" of
        Right e -> e
        Left _ -> error "Parse failed"

  assertEqual
    "Evaluate let expression"
    (NumberValue 20)
    (evaluator expr Map.empty)

-- Run all tests
tests :: Test
tests =
  TestList
    [ TestLabel "Number" testNumber,
      TestLabel "Addition" testAddition,
      TestLabel "Multiplication" testMultiplication,
      TestLabel "Precedence" testArithmeticPrecedence,
      TestLabel "Variable" testVariable,
      TestLabel "Lambda" testLambda,
      TestLabel "Application" testApplication,
      TestLabel "Let" testLet,
      TestLabel "FunctionDef" testFunctionDef,
      TestLabel "Program" testProgram,
      TestLabel "Complex" testComplex,
      TestLabel "Whitespace" testWhitespace,
      TestLabel "Comments" testComments,
      TestLabel "Evaluation" testEvaluation
    ]

-- main :: IO Counts
-- main = runTestTT tests
