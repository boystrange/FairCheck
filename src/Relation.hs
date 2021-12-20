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

-- |Implementation of session type equality, unfair subtyping and fair subtyping
-- decision algorithms.
module Relation (equality, strongSubtype, weakSubtype, fairSubtype) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (foldM)

import Tree (Tree)
import qualified Tree
import qualified Node
import qualified Predicate

-- | Shortcut for a binary relation over session types.
type Relation u v = Tree u -> Tree v -> Bool

-- | Generic coinductive relation over session types. The provided
-- 'Tree.Comparator' is used to compute the list of pairs of subtrees that must
-- be compared in turn.
relation :: (Ord u, Ord v) => Tree.Comparator u v -> Relation u v
relation cmp = aux Set.empty
  where
    aux vset f g | Set.member (f, g) vset = True
    aux vset f g | Just (es, rs) <- cmp f g =
                   all (uncurry equality) es &&
                   all (uncurry (aux (Set.insert (f, g) vset))) rs
    aux _ _ _ = False

-- | Equality relation over session types.
equality :: (Ord u, Ord v) => Relation u v
equality = relation Tree.equalityCmp

-- | Unfair subtyping relation over session types.
strongSubtype :: Ord u => Relation u u
strongSubtype = relation Tree.strongSubCmp

weakSubtype :: Ord u => Relation u u
weakSubtype = relation Tree.weakSubCmp

-- | Convergence relation over session types (see 'fairSubtype' below for
-- references to the papers in which the convergence relation is defined).
converge :: (Ord u, Ord v) => Relation u v
converge f g = not (Predicate.viable (Tree.difference f g))

-- | Fair subtyping relation over session types (Table 1). This implementation
-- of fair subtyping is based on the characterization of fair subtyping
-- described in the papers /Fair Subtyping for Multi-Party Session Types/
-- <http://dx.doi.org/10.1017/S096012951400022X> and /Fair Subtyping for Open/
-- /Session Types/ <http://dx.doi.org/10.1007/978-3-642-39212-2_34>.
fairSubtype :: (Ord u, Ord v) => Relation u v
fairSubtype = relation cmp
  where
    cmp f g | converge f g = Tree.strongSubCmp f g
            | otherwise = Nothing
