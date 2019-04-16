{-# LANGUAGE OverloadedStrings #-}

{-
   Web assembly and BF types.
-}
module Types where

import Control.Monad

import Data.Either
import Data.Functor

import Text.Pretty.Simple (pPrint)

import Prelude

{----- Brainfck -----}
newtype BFProgram =
  BFProgram [BFOp]
  deriving (Show)

data BFOp
  = MoveRight
  | MoveLeft
  | Inc
  | Dec
  | ReadStdIn
  | WriteStdOut
  | Loop [BFOp]
  deriving (Show)

{---- Webassembly ----}
newtype WasmProgram =
  WasmProgram [WasmOp]

newtype VariableName =
  VariableName String

data WasmOp
  -- Control Instructions
  -- Memory Instructions
  = LoadI32
  | StoreI32
  -- Variable instructions
  | GetGlobal VariableName
  | SetGlobal VariableName
  --Numeric Instructions
  | AddI32
  | SubI32
  | ConstI32 Int

instance Show WasmProgram where
  show (WasmProgram wasmOps) = unlines $ show <$> wasmOps

instance Show VariableName where
  show (VariableName var) = '$' : var

instance Show WasmOp where
  show (ConstI32 val) = "i32.const " ++ show val
  show (GetGlobal var) = "get_global " ++ show var
  show (SetGlobal var) = "set_global " ++ show var
  show AddI32 = "i32.add"
  show SubI32 = "i32.sub"

ptrVariable :: VariableName
ptrVariable = VariableName "ptr"

movePtrRight :: [WasmOp]
movePtrRight =
  [GetGlobal ptrVariable, ConstI32 1, AddI32, SetGlobal ptrVariable]

movePtrLeft :: [WasmOp]
movePtrLeft = [GetGlobal ptrVariable, ConstI32 1, SubI32, SetGlobal ptrVariable]

transformAST :: BFProgram -> WasmProgram
transformAST (BFProgram bfOps) = WasmProgram $ concatMap toWasmOps bfOps

toWasmOps :: BFOp -> [WasmOp]
toWasmOps MoveLeft = movePtrLeft
toWasmOps MoveRight = movePtrRight
