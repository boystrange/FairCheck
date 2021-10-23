-- This file is part of FairCheck
--
-- FairCheck is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License,
-- or (at your option) any later version.
--
-- FairCheck is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with FairCheck. If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2021 Luca Padovani

-- |Representation of FairCheck-specific syntax and typing errors.
module Exceptions where

import Atoms
import Type ( Type )
import Render
import Control.Exception
import qualified Data.List as List

data MyException
  = ErrorSyntax String
  | ErrorMultipleTypeDefinitions TypeName
  | ErrorMultipleProcessDefinitions ProcessName
  | ErrorMultipleNameDeclarations ChannelName
  | ErrorUnknownIdentifier String String
  | ErrorActionUnbounded ProcessName
  | ErrorCastUnbounded ProcessName ChannelName
  | ErrorSessionUnbounded ProcessName ChannelName
  | ErrorTypeUnbounded ChannelName
  | ErrorTypeNonContractive TypeName
  | ErrorTypeMismatch ChannelName String Type
  | ErrorArityMismatch ProcessName Int Int
  | ErrorLabelMismatch ChannelName [Label] [Label]
  | ErrorInvalidType String
  | ErrorInvalidCast ChannelName Type Type
  | ErrorLinearity [ChannelName]

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorMultipleTypeDefinitions tname) = "multiple type definitions: " ++ showWithPos tname
  show (ErrorMultipleProcessDefinitions pname) = "multiple process definitions: " ++ showWithPos pname
  show (ErrorUnknownIdentifier kind name) = "unknown " ++ kind ++ ": " ++ name
  show (ErrorMultipleNameDeclarations u) = "multiple declarations: " ++ showWithPos u
  show (ErrorTypeMismatch name e t) = "type error: " ++ showWithPos name ++ ": expected " ++ e ++ ", actual " ++ show t
  show (ErrorArityMismatch pname expected actual) =
    "arity mismatch for " ++ showWithPos pname ++ ": expected " ++
    show expected ++ ", actual " ++ show actual
  show (ErrorInvalidType msg) = "invalid type: " ++ msg
  show (ErrorActionUnbounded pname) = "action-unbounded process: " ++ showWithPos pname
  show (ErrorSessionUnbounded pname name) = "session-unbounded process: " ++ showWithPos pname ++ " creates " ++ showWithPos name
  show (ErrorCastUnbounded pname name) = "cast-unbounded process: " ++ showWithPos pname ++ " casts " ++ showWithPos name
  show (ErrorLinearity pnames) = "linearity violation: " ++ List.intercalate ", " (map showWithPos pnames)
  show (ErrorInvalidCast name t s) = "invalid cast for " ++ showWithPos name ++ ": " ++ show t ++ " is not a fair subtype of " ++ show s
  show (ErrorLabelMismatch name elabels alabels) = "labels mismatch for " ++ showWithPos name ++ ": expected " ++ show elabels ++ ", actual " ++ show alabels
  show (ErrorTypeUnbounded name) = "unbounded type: " ++ showWithPos name
  show (ErrorTypeNonContractive tname) = "non-contractive type: " ++ showWithPos tname
