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

module Type where

import Atoms
import Data.Set (Set)
import qualified Data.Set as Set

data Type
  = End Polarity
  | Var TypeName
  | Rec TypeName Type
  | Channel Polarity Type Type
  | Label Polarity [(Label, Type)]

fn :: Type -> Set TypeName
fn (End _) = Set.empty
fn (Var tname) = Set.singleton tname
fn (Rec tname t) = Set.delete tname (fn t)
fn (Channel _ t s) = Set.union (fn t) (fn s)
fn (Label _ bs) = Set.unions (map (fn . snd) bs)

-- DEFINITIONS

type TypeDef = (TypeName, Type)

