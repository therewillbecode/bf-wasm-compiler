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
newtype WASMProgram =
  WASMProgram [WasmOp]
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

-- Global variable which holds the element in memory we are pointing to
ptrVariable :: VariableName
ptrVariable = VariableName "ptr"

incPtr :: [WasmOp]
incPtr = [GetGlobal ptrVariable, ConstI32 1, AddI32, SetGlobal ptrVariable]
