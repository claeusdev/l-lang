-- Suppress warnings for unused imports if any during development
{-# OPTIONS_GHC -w #-}

module Main where

import Ast
import qualified Data.Map as Map
import Evaluator (eval, initialEnv)
import Parser (Parser (parse), parseExpr)
import Test.HUnit
import Value

-- Helper function to parse an input string and evaluate the resulting expression.
-- It discards the trace log, focusing on the resulting value or error.
evalTest :: String -> Either String Value
evalTest inputStr =
  case parse parseExpr inputStr of
    Nothing -> Left "TEST_ERROR: Parser failed to parse the input string."
    Just (expr, "") ->
      -- Successfully parsed and consumed all input
      case eval initialEnv expr of
        Left errMsg -> Left errMsg
        Right (val, _traceLog) -> Right val -- Discard trace log for simplicity in tests
    Just (expr, rest) -> Left ("TEST_ERROR: Parser did not consume entire input. Remainder: " ++ rest)

-- Test Cases

-- 1. Literal Evaluation Tests
testEvalNum :: Test
testEvalNum = "evaluate number" ~: Right (VNum 42) ~=? evalTest "42"

testEvalNumZero :: Test
testEvalNumZero = "evaluate zero" ~: Right (VNum 0) ~=? evalTest "0"

-- The parser for 'Num' expects only digits. "0-5" is parsed as Sub (Num 0) (Num 5).
testEvalNegativeNumExpr :: Test
testEvalNegativeNumExpr = "evaluate negative number expression" ~: Right (VNum (-5)) ~=? evalTest "0-5"

-- "-5" by itself should be a parse error with the current parser.
testParseNegativeNumDirectly :: Test
testParseNegativeNumDirectly = "parse -5 directly (should fail)" ~: Left "TEST_ERROR: Parser failed to parse the input string." ~=? evalTest "-5"

testEvalTrue :: Test
testEvalTrue = "evaluate True" ~: Right (VBool True) ~=? evalTest "True"

testEvalFalse :: Test
testEvalFalse = "evaluate False" ~: Right (VBool False) ~=? evalTest "False"

-- 2. Arithmetic Operation Tests
testAdd :: Test
testAdd = "evaluate 1 + 2" ~: Right (VNum 3) ~=? evalTest "1 + 2"

testSub :: Test
testSub = "evaluate 5 - 2" ~: Right (VNum 3) ~=? evalTest "5 - 2"

testMul :: Test
testMul = "evaluate 3 * 4" ~: Right (VNum 12) ~=? evalTest "3 * 4"

testArithmeticPrecedence :: Test
testArithmeticPrecedence = "evaluate 1 + 2 * 3 (precedence)" ~: Right (VNum 7) ~=? evalTest "1 + 2 * 3"

testArithmeticParentheses :: Test
testArithmeticParentheses = "evaluate (1 + 2) * 3 (parentheses)" ~: Right (VNum 9) ~=? evalTest "(1 + 2) * 3"

testArithmeticLeftAssociativity :: Test
testArithmeticLeftAssociativity = "evaluate 10 - 3 - 2 (left-associativity)" ~: Right (VNum 5) ~=? evalTest "10 - 3 - 2"

-- 3. Equality Operation Tests
testEqNumTrue :: Test
testEqNumTrue = "evaluate 5 == 5" ~: Right (VBool True) ~=? evalTest "5 == 5"

testEqNumFalse :: Test
testEqNumFalse = "evaluate 5 == 3" ~: Right (VBool False) ~=? evalTest "5 == 3"

testEqBoolTrue :: Test
testEqBoolTrue = "evaluate True == True" ~: Right (VBool True) ~=? evalTest "True == True"

testEqBoolFalse :: Test
testEqBoolFalse = "evaluate True == False" ~: Right (VBool False) ~=? evalTest "True == False"

testEqMixedTypes :: Test
testEqMixedTypes = "evaluate 1 == False (mixed types)" ~: Right (VBool False) ~=? evalTest "1 == False"

testEqEmptyLists :: Test
testEqEmptyLists = "evaluate [] == []" ~: Right (VBool True) ~=? evalTest "[] == []"

testEqListsTrue :: Test
testEqListsTrue = "evaluate [1,2] == [1,2]" ~: Right (VBool True) ~=? evalTest "[1,2] == [1,2]" -- Assuming parser takes comma separated

testEqListsFalse :: Test
testEqListsFalse = "evaluate [1,2] == [1,3]" ~: Right (VBool False) ~=? evalTest "[1,2] == [1,3]"

-- 4. Let Expression Tests
testLetSimple :: Test
testLetSimple = "evaluate let x = 10 in x" ~: Right (VNum 10) ~=? evalTest "let x = 10 in x"

testLetCalculation :: Test
testLetCalculation = "evaluate let x = 5 in x + x" ~: Right (VNum 10) ~=? evalTest "let x = 5 in x + x"

testLetNested :: Test
testLetNested = "evaluate let x = 3 in let y = 4 in x + y" ~: Right (VNum 7) ~=? evalTest "let x = 3 in let y = 4 in x + y"

testLetShadowing :: Test
testLetShadowing = "evaluate let x = 1 in let x = 2 in x" ~: Right (VNum 2) ~=? evalTest "let x = 1 in let x = 2 in x"

testLetVarUndefinedInBody :: Test
testLetVarUndefinedInBody = "let x = 5 in y (y undefined)" ~: Left "Undefined variable: y" ~=? evalTest "let x = 5 in y"

-- 5. Variable Lookup Test
testVarUndefined :: Test
testVarUndefined = "evaluate undefined variable" ~: Left "Undefined variable: myVar" ~=? evalTest "myVar"

-- 6. If-Then-Else Expression Tests
testIfTrueBranch :: Test
testIfTrueBranch = "evaluate if True then 100 else 200" ~: Right (VNum 100) ~=? evalTest "if True then 100 else 200"

testIfFalseBranch :: Test
testIfFalseBranch = "evaluate if False then 100 else 200" ~: Right (VNum 200) ~=? evalTest "if False then 100 else 200"

testIfCondEqTrue :: Test
testIfCondEqTrue = "evaluate if 1 == 1 then 1 else 0" ~: Right (VNum 1) ~=? evalTest "if 1 == 1 then 1 else 0"

testIfCondEqFalse :: Test
testIfCondEqFalse = "evaluate if 1 == 0 then 1 else 0" ~: Right (VNum 0) ~=? evalTest "if 1 == 0 then 1 else 0"

testIfCondTypeErr :: Test
testIfCondTypeErr = "if 1 then 10 else 20 (type error in condition)" ~: Left "Type Error: 'if' condition must evaluate to a Boolean." ~=? evalTest "if 1 then 10 else 20"

-- 7. Type Error Tests for Operations
testAddTypeErrNumBool :: Test
testAddTypeErrNumBool = "evaluate 1 + True (type error)" ~: Left "Type Error: + expects numbers. Got 1 and True" ~=? evalTest "1 + True"

testAddTypeErrBoolNum :: Test
testAddTypeErrBoolNum = "evaluate False + 2 (type error)" ~: Left "Type Error: + expects numbers. Got False and 2" ~=? evalTest "False + 2"

testMulTypeErrBoolBool :: Test
testMulTypeErrBoolBool = "evaluate True * False (type error)" ~: Left "Type Error: * expects numbers. Got True and False" ~=? evalTest "True * False"

-- 8. List Literal and Operation Tests
testEvalEmptyList :: Test
testEvalEmptyList = "evaluate []" ~: Right (VList []) ~=? evalTest "[]"

testEvalNumList :: Test
testEvalNumList = "evaluate [1, 2, 3]" ~: Right (VList [VNum 1, VNum 2, VNum 3]) ~=? evalTest "[1, 2, 3]"

testEvalMixedList :: Test
testEvalMixedList = "evaluate [1, True, []]" ~: Right (VList [VNum 1, VBool True, VList []]) ~=? evalTest "[1, True, []]"

testCons :: Test
testCons = "evaluate cons 1 [2,3]" ~: Right (VList [VNum 1, VNum 2, VNum 3]) ~=? evalTest "cons 1 [2,3]"

testConsToEmpty :: Test
testConsToEmpty = "evaluate cons 1 []" ~: Right (VList [VNum 1]) ~=? evalTest "cons 1 []"

testHead :: Test
testHead = "evaluate head [1,2,3]" ~: Right (VNum 1) ~=? evalTest "head [1,2,3]"

testTail :: Test
testTail = "evaluate tail [1,2,3]" ~: Right (VList [VNum 2, VNum 3]) ~=? evalTest "tail [1,2,3]"

testIsEmptyTrue :: Test
testIsEmptyTrue = "evaluate isEmpty []" ~: Right (VBool True) ~=? evalTest "isEmpty []"

testIsEmptyFalse :: Test
testIsEmptyFalse = "evaluate isEmpty [1]" ~: Right (VBool False) ~=? evalTest "isEmpty [1]"

-- 9. List Error Condition Tests
testConsTypeError :: Test
testConsTypeError = "cons 1 2 (type error)" ~: Left "Type Error: Second argument to 'cons' must be a list." ~=? evalTest "cons 1 2"

testHeadEmptyListError :: Test
testHeadEmptyListError = "head [] (runtime error)" ~: Left "Runtime Error: Cannot take head of an empty list." ~=? evalTest "head []"

testHeadTypeError :: Test
testHeadTypeError = "head 1 (type error)" ~: Left "Type Error: 'head' requires a list argument." ~=? evalTest "head 1"

testTailEmptyListError :: Test
testTailEmptyListError = "tail [] (runtime error)" ~: Left "Runtime Error: Cannot take tail of an empty list." ~=? evalTest "tail []"

testTailTypeError :: Test
testTailTypeError = "tail False (type error)" ~: Left "Type Error: 'tail' requires a list argument." ~=? evalTest "tail False"

testIsEmptyTypeError :: Test
testIsEmptyTypeError = "isEmpty 0 (type error)" ~: Left "Type Error: 'isEmpty' requires a list argument." ~=? evalTest "isEmpty 0"

-- 10. Complex Expression Test
testComplexLetIfArith :: Test
testComplexLetIfArith = "complex let/if/arithmetic" ~: Right (VNum 15) ~=? evalTest "let x = 10 in if x == 10 then (let y = x * 2 in y - 5) else 0"

-- 11. Parser Error Propagation Tests (via evalTest helper)
testParseErrorUnbalancedParen :: Test
testParseErrorUnbalancedParen = "parser error (unbalanced paren)" ~: Left "TEST_ERROR: Parser failed to parse the input string." ~=? evalTest "(1 + 2"

testParseErrorInvalidToken :: Test
testParseErrorInvalidToken = "parser error (invalid token)" ~: Left "TEST_ERROR: Parser did not consume entire input. Remainder: + #" ~=? evalTest "1 + #"

-- List of all tests to run
tests :: Test
tests =
  TestList
    [ TestLabel "testEvalNum" testEvalNum,
      TestLabel "testEvalNumZero" testEvalNumZero,
      TestLabel "testEvalNegativeNumExpr" testEvalNegativeNumExpr,
      TestLabel "testParseNegativeNumDirectly" testParseNegativeNumDirectly,
      TestLabel "testEvalTrue" testEvalTrue,
      TestLabel "testEvalFalse" testEvalFalse,
      TestLabel "testAdd" testAdd,
      TestLabel "testSub" testSub,
      TestLabel "testMul" testMul,
      TestLabel "testArithmeticPrecedence" testArithmeticPrecedence,
      TestLabel "testArithmeticParentheses" testArithmeticParentheses,
      TestLabel "testArithmeticLeftAssociativity" testArithmeticLeftAssociativity,
      TestLabel "testEqNumTrue" testEqNumTrue,
      TestLabel "testEqNumFalse" testEqNumFalse,
      TestLabel "testEqBoolTrue" testEqBoolTrue,
      TestLabel "testEqBoolFalse" testEqBoolFalse,
      TestLabel "testEqMixedTypes" testEqMixedTypes,
      TestLabel "testEqEmptyLists" testEqEmptyLists,
      TestLabel "testEqListsTrue" testEqListsTrue,
      TestLabel "testEqListsFalse" testEqListsFalse,
      TestLabel "testLetSimple" testLetSimple,
      TestLabel "testLetCalculation" testLetCalculation,
      TestLabel "testLetNested" testLetNested,
      TestLabel "testLetShadowing" testLetShadowing,
      TestLabel "testLetVarUndefinedInBody" testLetVarUndefinedInBody,
      TestLabel "testVarUndefined" testVarUndefined,
      TestLabel "testIfTrueBranch" testIfTrueBranch,
      TestLabel "testIfFalseBranch" testIfFalseBranch,
      TestLabel "testIfCondEqTrue" testIfCondEqTrue,
      TestLabel "testIfCondEqFalse" testIfCondEqFalse,
      TestLabel "testIfCondTypeErr" testIfCondTypeErr,
      TestLabel "testAddTypeErrNumBool" testAddTypeErrNumBool,
      TestLabel "testAddTypeErrBoolNum" testAddTypeErrBoolNum,
      TestLabel "testMulTypeErrBoolBool" testMulTypeErrBoolBool,
      TestLabel "testEvalEmptyList" testEvalEmptyList,
      TestLabel "testEvalNumList" testEvalNumList,
      TestLabel "testEvalMixedList" testEvalMixedList,
      TestLabel "testCons" testCons,
      TestLabel "testConsToEmpty" testConsToEmpty,
      TestLabel "testHead" testHead,
      TestLabel "testTail" testTail,
      TestLabel "testIsEmptyTrue" testIsEmptyTrue,
      TestLabel "testIsEmptyFalse" testIsEmptyFalse,
      TestLabel "testConsTypeError" testConsTypeError,
      TestLabel "testHeadEmptyListError" testHeadEmptyListError,
      TestLabel "testHeadTypeError" testHeadTypeError,
      TestLabel "testTailEmptyListError" testTailEmptyListError,
      TestLabel "testTailTypeError" testTailTypeError,
      TestLabel "testIsEmptyTypeError" testIsEmptyTypeError,
      TestLabel "testComplexLetIfArith" testComplexLetIfArith,
      TestLabel "testParseErrorUnbalancedParen" testParseErrorUnbalancedParen,
      TestLabel "testParseErrorInvalidToken" testParseErrorInvalidToken
    ]

-- Main function to run the tests
main :: IO Counts
main = runTestTT tests
