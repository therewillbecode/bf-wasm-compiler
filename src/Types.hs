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
  | MulI32
  | ConstI32 Int

instance Show WasmProgram where
  show (WasmProgram wasmOps) =
    "(module \n (memory 1) \n" ++
    wasmGlobal "ptr" ++
    wasmGlobal "currCellVal" ++
    wasmGetPtrFunc ++
    wasmMainFunc code ++ "\n(export \"get_ptr\" (func $get_ptr)) \n )"
    where
      code = unlines $ show <$> wasmOps

instance Show VariableName where
  show (VariableName var) = '$' : var

instance Show WasmOp where
  show (ConstI32 val) = "  i32.const " ++ show val
  show (GetGlobal var) = "  get_global " ++ show var
  show (SetGlobal var) = "  set_global " ++ show var
  show AddI32 = "  i32.add"
  show SubI32 = "  i32.sub"
  show MulI32 = "  i32.mul"
  show LoadI32 = "  i32.load"
  show StoreI32 = "  i32.store"

wasmGlobal name = "(global $" ++ name ++ " (mut i32) (i32.const 0))\n"

wasmImport = "(import \"js\" \"print\" (func $print))"

-- function that is exported so that JS can inspect ptr value
wasmGetPtrFunc = "(func $get_ptr (result i32) \n get_global $ptr \n )\n\n"

wasmMainFunc body =
  "(export \"main\" (func $main)) \n (func $main\n" ++ body ++ ")"

ptrVariable :: VariableName
ptrVariable = VariableName "ptr"

currCellValue :: VariableName
currCellValue = VariableName "currCellValue"

movePtrRight :: [WasmOp]
movePtrRight =
  [GetGlobal ptrVariable, ConstI32 1, AddI32, SetGlobal ptrVariable]

movePtrLeft :: [WasmOp]
movePtrLeft = [GetGlobal ptrVariable, ConstI32 1, SubI32, SetGlobal ptrVariable]

incCurrCellVal :: [WasmOp]
incCurrCellVal = modifyCurrCellVal [ConstI32 1, AddI32]

decCurrCellVal :: [WasmOp]
decCurrCellVal = modifyCurrCellVal [ConstI32 1, SubI32]

-- Updates the currCellVal global variable with the current cell val of the pointed at cell 
modifyCurrCellVal :: [WasmOp] -> [WasmOp]
modifyCurrCellVal ops =
  [GetGlobal ptrVariable] ++ getCurrCellVal ++ ops ++ [StoreI32]

-- calculate the address offset so we can get the pointed at value (32 bit int is 4 bytes wide)
getCellByteOffset :: [WasmOp]
getCellByteOffset = [GetGlobal ptrVariable, ConstI32 elementBytesWidth, MulI32]
  where
    elementBytesWidth = 4

-- push the value at current pointed to cell to stack
getCurrCellVal :: [WasmOp]
getCurrCellVal =
  getCellByteOffset ++
  [ LoadI32
  --, SetGlobal currCellValue -- store pointed at value in global variable
  ]

transformAST :: BFProgram -> WasmProgram
transformAST (BFProgram bfOps) = WasmProgram $ concatMap toWasmOps bfOps

toWasmOps :: BFOp -> [WasmOp]
toWasmOps MoveLeft = movePtrLeft
toWasmOps MoveRight = movePtrRight
toWasmOps Inc = incCurrCellVal
toWasmOps Dec = decCurrCellVal
