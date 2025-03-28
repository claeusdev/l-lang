module TestRunner where

import Ast
import Control.Monad (forM)
import ExamplePrograms (createAllExamples)
import Parser
import ParserTest (tests)
import System.IO (readFile)
import Test.HUnit

-- Test parsing a file
testParseFile :: FilePath -> IO Test
testParseFile path = do
  fileName <- return $ last $ words path -- Extract just the filename
  result <- getParsedExpr path

  -- For actual interpreted result (if needed)
  evalResult <- interpretFile path

  return $ TestLabel fileName $ TestCase $ do
    putStrLn $ "\nTesting file: " ++ path
    case result of
      Left err -> do
        putStrLn $ "Parse error: " ++ err
        assertFailure $ "Failed to parse " ++ fileName
      Right expr -> do
        putStrLn $ "Successfully parsed: " ++ fileName
        putStrLn $ "Parse result: " ++ show expr
        case evalResult of
          Left err -> putStrLn $ "Evaluation error: " ++ err
          Right val -> putStrLn $ "Evaluated result: " ++ show val

-- Create tests for files
fileTests :: IO Test
fileTests = do
  examplePaths <- createAllExamples
  tests <- forM examplePaths testParseFile
  return $ TestList tests

-- For direct use in HUnit test list
runnerTests :: Test
runnerTests = TestCase $ do
  fileTestsList <- fileTests
  _ <- runTestTT fileTestsList
  return ()

-- Run unit tests and file tests
runAllTests :: IO Counts
runAllTests = do
  putStrLn "Running unit tests..."
  unitResults <- runTestTT tests

  putStrLn "\nCreating and parsing example programs..."
  examplePaths <- createAllExamples
  fileTests <- forM examplePaths testParseFile
  let fileTestList = TestList fileTests

  putStrLn "\nRunning file parsing tests..."
  fileResults <- runTestTT fileTestList

  putStrLn "\nTest Summary:"
  putStrLn $ "Unit tests: " ++ show unitResults
  putStrLn $ "File tests: " ++ show fileResults

  return unitResults
