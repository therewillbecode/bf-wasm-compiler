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
  deriving (Show)

newtype VariableName =
  VariableName String
  deriving (Show)

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
  deriving (Show)
 -- show _ = ""

--instance Show WasmOp where
-- Global variable which holds the element in memory we are pointing to
ptrVariable :: VariableName
ptrVariable = VariableName "ptr"

incPtr :: [WasmOp]
incPtr = [GetGlobal ptrVariable, ConstI32 1, AddI32, SetGlobal ptrVariable]

transformAST :: BFProgram -> WasmProgram
transformAST (BFProgram bfOps) = WasmProgram $ toWasmOps <$> bfOps

toWasmOps :: BFOp -> [[WasmOp]]
toWasmOps Inc = [incPtr]
