module Parser where

import Ast
import qualified Data.Functor
import Data.Functor.Identity
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Token as Token

-- Lexer definition
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style =
      emptyDef
        { Token.commentLine = "#",
          Token.reservedNames = ["lambda", "in", "let", "func"],
          Token.reservedOpNames = ["+", "-", "*", "/", "=", "."]
        }

-- Parser components - use the lexer's whitespace handling
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Int
integer = Token.integer lexer Data.Functor.<&> fromIntegral

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

-- Function definition
functionDef :: Parser (String, Expression)
functionDef = do
  reserved "func" -- Make sure this matches your reserved keyword
  name <- identifier
  params <- many identifier
  reservedOp "="
  body <- expr
  -- Create a lambda abstraction that takes all parameters
  let function = foldr Fun body params
  return (name, function)

-- Program parser - handles the entire file with proper whitespace
program :: Parser Expression
program = do
  whiteSpace -- Consume initial whitespace
  -- Parse function definitions
  defs <- many (try functionDef)
  -- Create an environment from the definitions
  let makeEnv [] exprAst = exprAst -- Changed from 'expr' to 'exprAst' to fix shadowing
      makeEnv ((name, func) : ds) exprAst =
        Apply (Fun name (makeEnv ds exprAst)) func
  -- Parse the main expression (if there is one)
  e <- option (Number 0) expr -- Default to 0 if no expression
  whiteSpace -- Consume trailing whitespace
  eof -- Ensure we've consumed the entire input
  -- Wrap the expression in the function definitions
  return $ makeEnv defs e

-- Expression parser
expr :: Parser Expression
expr = application

-- Application has lower precedence than arithmetic
application :: Parser Expression
application = do
  -- Parse one or more terms and fold them as function applications
  terms <- many1 aexpr
  return $ foldl1 Apply terms

-- Arithmetic expressions
aexpr :: Parser Expression
aexpr = buildExpressionParser arithmeticOps term

-- Terms are simple expressions
term :: Parser Expression
term =
  parens expr
    <|> Number
      <$> integer
    <|> try lambdaExpr
    <|> try letBinding
    <|> Id
      <$> identifier

arithmeticOps :: [[Operator String () Identity Expression]]
arithmeticOps =
  [ [ Infix (reservedOp "*" >> return Multiply) AssocLeft,
      Infix (reservedOp "/" >> return Divide) AssocLeft
    ],
    [ Infix (reservedOp "+" >> return Add) AssocLeft,
      Infix (reservedOp "-" >> return Subtract) AssocLeft
    ]
  ]

-- Lambda expression (Function)
lambdaExpr :: Parser Expression
lambdaExpr = do
  reserved "lambda"
  param <- identifier
  reservedOp "."
  Fun param <$> expr

-- Let binding - explicitly for your "let x = 100 in x * 2" syntax
letBinding :: Parser Expression
letBinding = do
  reserved "let"
  name <- identifier
  reservedOp "="
  value <- expr
  reserved "in"
  body <- expr
  -- Desugar to ((\name -> body) value)
  return $ Apply (Fun name body) value

-- Parse a string into an expression
parseExpr :: String -> Either ParseError Expression
parseExpr = parse program ""

-- Parse a file into an expression
parseExprFile :: FilePath -> IO (Either ParseError Expression)
parseExprFile = parseFromFile program

-- Function to get the parsed expression without evaluating
getParsedExpr :: FilePath -> IO (Either String Expression)
getParsedExpr path = do
  result <- parseExprFile path
  case result of
    Left err -> return $ Left $ "Parse error: " ++ show err
    Right exprAst -> return $ Right exprAst

extractValue :: Value -> Either String Int
extractValue (NumberValue n) = Right n
extractValue (Closure {}) = Left "Result is a function, not a number"

-- Main function to interpret a file
interpretFile :: FilePath -> IO (Either String Int)
interpretFile path = do
  result <- parseExprFile path
  case result of
    Left err -> return $ Left $ "Parse error: " ++ show err
    Right exprAst ->
      let value = evaluator exprAst Map.empty
       in return $ extractValue value
