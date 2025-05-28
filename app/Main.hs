{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Char (isSpace)
import Data.IORef
import Data.List (dropWhile)
import qualified Data.Map as Map
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Evaluator
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Parser
import System.Environment (getArgs)
import System.IO (getLine, hFlush, isEOF, stdout)
import System.Timeout (timeout)
import Value
import Web.Scotty

-- ----------------------------------------------------------------------------
-- Web Server Specific Definitions
-- ----------------------------------------------------------------------------

data StepResult = StepResult {output :: String, ast :: Maybe String} deriving (Show)

instance ToJSON StepResult where
  toJSON (StepResult out maybeAst) = object ["output" .= out, "ast" .= maybeAst]

data MultiEvalResult = MultiEvalResult
  { steps :: [StepResult],
    finalError :: Maybe String,
    finalEnvironment :: Env,
    traceLog :: Maybe [String] -- Field for combined trace log
  }
  deriving (Show)

instance ToJSON MultiEvalResult where
  toJSON (MultiEvalResult stepList err envMap mLog) =
    object
      [ "steps" .= stepList,
        "finalError" .= err,
        "finalEnvironment" .= envMap,
        "traceLog" .= mLog
      ]

-- Timeout for evaluation in microseconds (e.g., 5 seconds)
evaluationTimeoutDuration :: Int
evaluationTimeoutDuration = 5 * 1000 * 1000 -- 5 seconds

-- Function to process a single line for the web handler's fold
-- Takes the environment accumulated so far, and the current line string
-- Returns Either Error Message (New Env, Output String, AST String, Trace Log for this line)
processWebLine :: Env -> String -> IO (Either String (Env, String, Maybe String, TraceLog))
processWebLine currentEnv line = do
  let trimmedLine = dropWhile isSpace line
  if null trimmedLine
    then return $ Right (currentEnv, "", Nothing, [])
    else case parse parseDefinition trimmedLine of
      Just ((name, defExpr), "") -> do
        let astString = Just (show defExpr)
        let envForDefEval = Map.insert name valueForDef currentEnv
            -- evalResultAndTrace now holds Either String (Value, TraceLog)
            evalResultAndTrace = eval envForDefEval defExpr
            valueForDef = case evalResultAndTrace of
              Left e -> error $ "Internal error: accessing value from failed recursive definition for " ++ name ++ ": " ++ e
              Right (v, _) -> v
        case evalResultAndTrace of
          Left err -> return $ Left $ "Error in definition '" ++ name ++ "': " ++ err
          Right (_, trace) -> return $ Right (envForDefEval, "Defined (rec): " ++ name, astString, trace)
      _ ->
        case parse parseExpr trimmedLine of
          Just (exprAST, "") -> do
            let astString = Just (show exprAST)
            case eval currentEnv exprAST of -- eval now returns Either String (Value, TraceLog)
              Left errMsg -> return $ Left errMsg
              Right (val, trace) -> return $ Right (currentEnv, show val, astString, trace)
          _ ->
            return $ Left $ "Parse Error on line: " ++ take 40 trimmedLine

-- Function to run the Scotty Web Server
runWebServer :: IO ()
runWebServer = do
  putStrLn "Starting L Language Web Server on port 3000 (Stateful, Multi-line, with Trace)..."
  sharedEnvRef <- newIORef initialEnv -- Initialize shared state with built-ins
  scotty 3000 $ do
    middleware simpleCors
    -- middleware logStdoutDev

    get "/" $ do
      html $
        mconcat
          [ "<h1>L Web Interface</h1>",
            "<p>Enter code (definitions or expressions, one per line) below and click Evaluate.</p>",
            "<textarea id='code' rows='10' cols='80' style='font-family:monospace;'></textarea><br/>",
            "<button onclick='evaluateCode()'>Evaluate</button>",
            "<h2>Outputs / Steps:</h2><div id='outputSteps' style='background-color:#f0f0f0; padding:10px; border:1px solid #ccc; min-height:20px; white-space: pre-wrap;'></div>",
            "<h2>Evaluation Trace:</h2><pre id='outputTrace' style='background-color:#e0e0e0; padding:10px; border:1px solid #bbb; max-height: 300px; overflow-y: auto; white-space: pre-wrap;'></pre>",
            "<h2>Final Environment:</h2><pre id='outputEnv' style='background-color:#f0f0f0; padding:10px; border:1px solid #ccc; min-height:20px; max-height: 200px; overflow-y: auto;'></pre>",
            "<script src='/script.js'></script>"
          ]

    get "/script.js" $ do
      setHeader "Content-Type" "application/javascript"
      file "frontend.js"

    post "/evaluate" $ do
      codeText <- body
      let codeString = unpack (decodeUtf8 codeText)
      let codeLines = lines codeString

      initialEnvState <- liftIO $ readIORef sharedEnvRef

      -- Accumulator: Either Error (CurrentEnvInBlock, List of StepResults, Accumulated TraceLog)
      finalResult <-
        foldM
          ( \acc line -> case acc of
              Left err -> return $ Left err
              Right (env, stepsAcc, traceAcc) -> do
                lineResult <- liftIO $ processWebLine env line
                case lineResult of
                  Left err -> return $ Left err
                  Right (newEnv, outputStr, maybeAstStr, lineTrace) ->
                    let step = StepResult outputStr maybeAstStr
                     in return $ Right (newEnv, if null outputStr && maybeAstStr == Nothing then stepsAcc else stepsAcc ++ [step], traceAcc ++ lineTrace)
          )
          (Right (initialEnvState, [], []))
          codeLines

      response <- case finalResult of
        Left errorMsg ->
          return $ MultiEvalResult [] (Just errorMsg) initialEnvState Nothing
        Right (finalEnvState, steps, accumulatedTrace) -> do
          liftIO $ atomicModifyIORef' sharedEnvRef $ \_ -> (finalEnvState, ())
          return $ MultiEvalResult steps Nothing finalEnvState (Just accumulatedTrace)

      json response

-- ----------------------------------------------------------------------------
-- REPL Specific Definitions
-- ----------------------------------------------------------------------------
repl :: Env -> IO ()
repl currentEnv = do
  putStr "L-Repl> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "\nGoodbye!"
    else do
      line <- getLine
      case line of
        ":quit" -> putStrLn "Goodbye!"
        ":env" -> do
          print currentEnv
          repl currentEnv
        _ -> handleInput line currentEnv

handleInput :: String -> Env -> IO ()
handleInput line currentEnv =
  let trimmedLine = dropWhile isSpace line
   in if null trimmedLine
        then repl currentEnv
        else case parse parseDefinition trimmedLine of
          Just ((name, expr), "") -> do
            putStrLn $ "Evaluating potentially recursive definition for '" ++ name ++ "'..."
            let newEnv = Map.insert name value currentEnv
                evalResultAndTrace = eval newEnv expr -- Returns (Value, TraceLog)
                value = case evalResultAndTrace of
                  Left err -> Prelude.error $ "Internal error: accessing value from failed recursive eval for " ++ name ++ ": " ++ err
                  Right (val, _) -> val
            case evalResultAndTrace of
              Left err -> do
                putStrLn $ "Error in definition '" ++ name ++ "': " ++ err
                repl currentEnv
              Right (_, trace) -> do
                -- Definition succeeded
                putStrLn $ "Defined (rec): " ++ name
                putStrLn "--- Evaluation Trace (Definition) ---"
                mapM_ (putStrLn . ("  " ++)) trace
                putStrLn "-------------------------------------"
                repl newEnv
          _ ->
            case parse parseExpr trimmedLine of
              Just (exprAST, "") -> do
                case eval currentEnv exprAST of -- Returns (Value, TraceLog)
                  Left err -> putStrLn $ "Error: " ++ err
                  Right (val, trace) -> do
                    -- Expression evaluation succeeded
                    print val
                    putStrLn "--- Evaluation Trace ---"
                    mapM_ (putStrLn . ("  " ++)) trace
                    putStrLn "----------------------"
                repl currentEnv
              Just (_, rest) -> do
                putStrLn ("Parse Error: Unexpected input near: '" ++ take 20 rest ++ "...'")
                repl currentEnv
              Nothing -> do
                putStrLn "Parse Error: Invalid input."
                repl currentEnv

runRepl :: IO ()
runRepl = do
  putStrLn "Starting L Language REPL..."
  putStrLn "Define: name = expression (recursion supported)"
  putStrLn "Evaluate: expression (built-ins: map, filter, length)"
  putStrLn "Commands: :quit, :env"
  repl initialEnv

-- ----------------------------------------------------------------------------
-- Main Entry Point - Selects Mode
-- ----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r"] -> runRepl
    ["-w"] -> runWebServer
    _ -> do
      putStrLn "Usage: <program-name> [-r | -w]"
      putStrLn "  -r: Run interactive REPL"
      putStrLn "  -w: Run web server on port 3000 (Stateful, Multi-line, with Trace)"
      putStrLn "Defaulting to REPL mode."
      runRepl
