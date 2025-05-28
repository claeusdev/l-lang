{-# LANGUAGE OverloadedStrings #-}

module Value where

import Ast
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

-- ----------------------------------------------------------------------------
-- Values (Result of Evaluation)
-- ----------------------------------------------------------------------------

data Value
  = VNum Int
  | VBool Bool
  | VList [Value]
  | VClosure String Expr Env -- Closure: captures var name, body, and environment
  | VPrim String (Value -> Either String Value)

instance Eq Value where
  (VNum n1) == (VNum n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  -- Recursively compare lists; this requires Eq Value, which we are defining.
  (VList l1) == (VList l2) = l1 == l2
  -- Closures are generally not considered equal based on their parts
  (VClosure {}) == (VClosure {}) = False
  -- Primitive functions cannot be compared for equality.
  (VPrim n1 _) == (VPrim n2 _) = n1 == n2
  -- If the constructors are different, they are not equal.
  _ == _ = False

-- Custom Show instance for better readability
instance Show Value where
  show (VNum n) = show n
  show (VBool b) = show b
  show (VList vs) = "[" ++ unwords (map show vs) ++ "]"
  show (VClosure var body _) = "<closure: \\" ++ var ++ " -> ...>"
  show (VPrim name _) = "<primitive:" ++ name ++ ">"

instance ToJSON Value where
  toJSON (VNum n) = A.Number (fromIntegral n) -- Convert Int to Aeson's Number type
  toJSON (VBool b) = A.Bool b
  toJSON (VList vs) = A.Array $ V.fromList $ map toJSON vs -- Recursively convert list elements.
  -- Represent closures and primitives as simple placeholder strings in JSON
  toJSON (VClosure {}) = A.String (T.pack "<closure>")
  toJSON (VPrim name _) = A.String (T.pack ("<primitive:" ++ name ++ ">"))

-- Environment for variable bindings
type Env = Map.Map String Value
