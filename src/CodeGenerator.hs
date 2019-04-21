{-# LANGUAGE OverloadedStrings #-}

module CodeGenerator where

import Control.Monad

import Data.Either
import Data.Functor

import Text.Pretty.Simple (pPrint)

import Prelude

import Types

loopWhileCellZero :: [WasmOp] -> WasmOp
loopWhileCellZero ops = Block varName [blockOps]
  where
    varName = VariableName "loop_break"
    breakIfCellZero = currCellEqZero ++ [BrIf 1]
    blockOps = WasmLoop (VariableName "loop") (ops ++ breakIfCellZero)

currCellEqZero :: [WasmOp]
currCellEqZero = getCurrCellVal ++ [EqzI32]

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

-- Updates the curr Cell Val with the current cell val of the pointed at cell 
modifyCurrCellVal :: [WasmOp] -> [WasmOp]
modifyCurrCellVal ops =
  getCellByteOffset ++ getCurrCellVal ++ ops ++ toUnsigned8Bit ++ [StoreI32]

-- calculate the byte offset where a cell lives in memory (32 bit int is 4 bytes wide)
getCellByteOffset :: [WasmOp]
getCellByteOffset = [GetGlobal ptrVariable, ConstI32 elementBytesWidth, MulI32]
  where
    elementBytesWidth = 4

-- push the value at current pointed to cell to stack
getCurrCellVal :: [WasmOp]
getCurrCellVal = getCellByteOffset ++ [LoadI32]

-- prints cell integer as an ASCII encoded value
printCurrCellVal :: [WasmOp]
printCurrCellVal = [GetGlobal ptrVariable, Call $ VariableName "print"]

-- Truncate o 8 bit when > 255
-- Used before a number is written to memory to preserve unsigned 8 bit properties
toUnsigned8Bit :: [WasmOp]
toUnsigned8Bit = [ConstI32 255, AndI32]

transformAST :: BFProgram -> WasmProgram
transformAST (BFProgram bfOps) = WasmProgram $ concatMap toWasmOps bfOps

toWasmOps :: BFOp -> [WasmOp]
toWasmOps MoveLeft = movePtrLeft
toWasmOps MoveRight = movePtrRight
toWasmOps Inc = incCurrCellVal
toWasmOps Dec = decCurrCellVal
toWasmOps WriteStdOut = printCurrCellVal
toWasmOps (BFLoop bfOps) = [loopWhileCellZero $ concatMap toWasmOps bfOps]
