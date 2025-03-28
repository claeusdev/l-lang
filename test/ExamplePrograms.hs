module ExamplePrograms where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hPutStr, openFile)

-- Create a directory for example programs
createExamplesDir :: IO FilePath
createExamplesDir = do
  let dir = "test-examples"
  createDirectoryIfMissing True dir
  return dir

-- Write a program to file
writeProgram :: FilePath -> String -> String -> IO FilePath
writeProgram dir name content = do
  let path = dir </> name
  handle <- openFile path WriteMode
  hPutStr handle content
  hClose handle
  return path

-- Simple arithmetic program
arithmeticProgram :: String
arithmeticProgram =
  unlines
    [ "# Simple arithmetic operations",
      "1 + 2 * 3",
      "# Expected result: 7"
    ]

-- Program with variables and let bindings
letProgram :: String
letProgram =
  unlines
    [ "# Let binding example",
      "let x = 100 in",
      "let y = 50 in",
      "x + y * 2",
      "# Expected result: 200"
    ]

-- Lambda expressions
lambdaProgram :: String
lambdaProgram =
  unlines
    [ "# Lambda expressions",
      "(lambda x. x * x) 5",
      "# Expected result: 25"
    ]

-- Function definitions
functionProgram :: String
functionProgram =
  unlines
    [ "# Function definitions",
      "func square x = x * x",
      "func twice f x = f (f x)",
      "",
      "twice square 3",
      "# Expected result: 81"
    ]

-- Factorial function
factorialProgram :: String
factorialProgram =
  unlines
    [ "# Factorial implementation",
      "# Note: This requires if-then-else which is commented out in your AST",
      "# This example won't work until you uncomment that part",
      "func factorial n =",
      "  # if n = 0 then 1 else n * factorial (n - 1)",
      "  # Since conditional is commented out, we'll just use a direct calculation",
      "  n * n # placeholder",
      "",
      "factorial 5",
      "# Expected result (with proper factorial): 120",
      "# Expected with placeholder: 25"
    ]

-- Higher-order functions
higherOrderProgram :: String
higherOrderProgram =
  unlines
    [ "# Higher-order functions",
      "func apply f x = f x",
      "func add1 x = x + 1",
      "func mul2 x = x * 2",
      "",
      "apply add1 (apply mul2 10)",
      "# Expected result: 21"
    ]

-- Create all example programs
createAllExamples :: IO [FilePath]
createAllExamples = do
  dir <- createExamplesDir

  -- Write each program to a file
  arithmetic <- writeProgram dir "arithmetic.l" arithmeticProgram
  letBindings <- writeProgram dir "let.l" letProgram
  lambda <- writeProgram dir "lambda.l" lambdaProgram
  functions <- writeProgram dir "functions.l" functionProgram
  factorial <- writeProgram dir "factorial.l" factorialProgram
  higherOrder <- writeProgram dir "higher-order.l" higherOrderProgram

  return [arithmetic, letBindings, lambda, functions, factorial, higherOrder]

-- Main function to create examples
-- main :: IO ()
-- main = do
--   paths <- createAllExamples
--   putStrLn $ "Created example programs in: " ++ head paths
--   putStrLn $ "Total examples created: " ++ show (length paths)
