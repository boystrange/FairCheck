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

-- |This module defines the external representation of __session types__
-- (Section 3.1).
module Type where

import Atoms
import Data.Set (Set)
import qualified Data.Set as Set

-- |Session type representation. In addition to the forms described in the
-- paper, we also provide a 'Rec' constructor to represent recursive session
-- types explicitly in a closed form that is easier to convert into regular
-- trees.
data Type
  -- |Terminated protocol.
  = End Polarity
  -- |Session type variable.
  | Var TypeName
  -- |Recursive session type.
  | Rec TypeName Type
  -- |Input/output of a channel.
  | Channel Polarity Type Type
  -- |Input/output of a label.
  | Label Polarity [(Label, Type)]

-- |Compute the set of free session type variables occurring in a session type.
fn :: Type -> Set TypeName
fn (End _) = Set.empty
fn (Var tname) = Set.singleton tname
fn (Rec tname t) = Set.delete tname (fn t)
fn (Channel _ t s) = Set.union (fn t) (fn s)
fn (Label _ bs) = Set.unions (map (fn . snd) bs)

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a session type.
type TypeDef = (TypeName, Type)
