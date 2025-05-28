{-# LANGUAGE LambdaCase #-}

module Parser where

import Ast
import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- ----------------------------------------------------------------------------
-- Parser
-- ----------------------------------------------------------------------------

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    return (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap -- Use Control.Monad.ap for Applicative (<*>)

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \s -> do
    (a, s') <- p s
    parse (f a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- Basic Parser Combinators
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  (c : cs) | p c -> Just (c, cs)
  _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

-- Whitespace handling
ws :: Parser ()
ws = () <$ many (satisfy isSpace)

token :: Parser a -> Parser a
token p = ws *> p <* ws

-- Lexemes
identifier :: Parser String
identifier = token $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

integer :: Parser Int
integer = token $ read <$> some (satisfy isDigit)

-- Reserved words
reservedWords :: [String]
reservedWords =
  [ "let",
    "in",
    "cons",
    "head",
    "tail",
    "isEmpty",
    "True",
    "False",
    "if",
    "then",
    "else" -- Added if/then/else
  ]

-- Expression Parsers
parseVar :: Parser Expr
parseVar = do
  name <- identifier
  if name `elem` reservedWords
    then empty -- Fail if the identifier is a reserved word
    else return $ Var name

parseNum :: Parser Expr
parseNum = Num <$> integer

parseBool :: Parser Expr
parseBool = BoolLit <$> (True <$ token (string "True") <|> False <$ token (string "False"))

-- Parses one or more arguments for a lambda, creating nested Lambdas
parseLambda :: Parser Expr
parseLambda = do
  _ <- token $ char '\\' <|> char 'λ' -- Allow both \ and λ
  vars <- some identifier -- Parse one or more variable names
  _ <- token $ string "->"
  body <- parseExpr -- Recursively parse the body
  -- Fold the variables into nested Lam structures
  return $ foldr Lam body vars

parseList :: Parser Expr
parseList = do
  _ <- token $ char '['
  exprs <- parseExpr `sepBy` (token $ char ',') -- Use token for separator
  _ <- token $ char ']'
  return $ List exprs

-- Helper for separated lists
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure []

parseLet :: Parser Expr
parseLet = do
  _ <- token $ string "let"
  var <- identifier
  _ <- token $ char '='
  boundExpr <- parseExpr
  _ <- token $ string "in"
  bodyExpr <- parseExpr
  return $ Let var boundExpr bodyExpr

-- Parsers for built-in list functions using keywords for dedicated AST nodes:
parseConsKeyword :: Parser Expr
parseConsKeyword = do
  _ <- token $ string "cons"
  arg1 <- parseAtom
  arg2 <- parseAtom
  return $ Cons arg1 arg2

parseHeadKeyword :: Parser Expr
parseHeadKeyword = do
  _ <- token $ string "head"
  arg <- parseAtom
  return $ Head arg

parseTailKeyword :: Parser Expr
parseTailKeyword = do
  _ <- token $ string "tail"
  arg <- parseAtom
  return $ Tail arg

parseIsEmptyKeyword :: Parser Expr
parseIsEmptyKeyword = do
  _ <- token $ string "isEmpty"
  arg <- parseAtom
  return $ IsEmpty arg

-- Parenthesized expressions
parseParen :: Parser Expr
parseParen = token (char '(') *> parseExpr <* token (char ')')

-- Basic building block (atom)
parseAtom :: Parser Expr
parseAtom =
  parseNum
    <|> parseBool
    <|> parseVar
    <|> parseLambda -- Multi-arg lambda included
    <|> parseList
    -- <|> parseLet -- Let is not usually an atom, move higher
    <|> parseConsKeyword -- Use keyword parsers
    <|> parseHeadKeyword
    <|> parseTailKeyword
    <|> parseIsEmptyKeyword
    <|> parseParen

-- Application (left-associative)
parseApp :: Parser Expr
parseApp = foldl1 App <$> some parseAtom -- Application is just atoms next to each other

-- Operator Precedence using chainl1
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> return x

-- Equality check (lowest operator precedence before if/let/lambda)
parseEq :: Parser Expr
parseEq = chainl1 parseAddSub (token (string "==") *> pure Eq)

-- Multiplication (higher precedence)
parseMul :: Parser Expr
parseMul = chainl1 parseApp (token (char '*') *> pure Mul)

-- Addition/Subtraction (medium precedence)
parseAddSub :: Parser Expr
parseAddSub =
  chainl1 parseMul $
    (token (char '+') *> pure Add) <|> (token (char '-') *> pure Sub)

-- If-Then-Else parser
parseIf :: Parser Expr
parseIf = do
  _ <- token $ string "if"
  cond <- parseExpr -- Parse condition
  _ <- token $ string "then"
  thenBranch <- parseExpr -- Parse then branch
  _ <- token $ string "else"
  elseBranch <- parseExpr -- Parse else branch
  return $ IfThenElse cond thenBranch elseBranch

-- Top-level expression parser: handles let, lambda, if, and falls back to operators/atoms
parseExpr :: Parser Expr
parseExpr =
  parseIf -- Try parsing 'if' first
    <|> parseLet -- Try 'let'
    <|> parseLambda -- Try lambda (already handled in atom, but could be here too)
    <|> parseEq -- Then equality and other operators

-- Parser for top-level definitions ( name = expression )
-- Consumes the whole line if it matches this pattern.
parseDefinition :: Parser (String, Expr)
parseDefinition = do
  name <- identifier
  _ <- token $ char '='
  expr <- parseExpr
  -- Check that there's nothing left on the line after the expression
  ws -- Consume trailing whitespace if any
  Parser $ \rest -> if null rest then Just ((name, expr), "") else Nothing
