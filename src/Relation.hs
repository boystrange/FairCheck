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

module Relation (equality, subtype, fairSubtype) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (foldM)

import Tree (Tree)
import qualified Tree
import qualified Node
import qualified Predicate

type Relation u v = Tree u -> Tree v -> Bool

relation :: (Ord u, Ord v) => Tree.Comparator u v -> Relation u v
relation cmp = aux Set.empty
  where
    aux vset f g | Set.member (f, g) vset = True
    aux vset f g | Just (es, rs) <- cmp f g =
                   all (uncurry equality) es &&
                   all (uncurry (aux (Set.insert (f, g) vset))) rs
    aux _ _ _ = False

equality :: (Ord u, Ord v) => Relation u v
equality = relation Tree.equalityCmp

subtype :: (Ord u, Ord v) => Relation u v
subtype = relation Tree.subtypeCmp

converge :: (Ord u, Ord v) => Relation u v
converge f g = not (Predicate.viable (Tree.difference f g))

fairSubtype :: (Ord u, Ord v) => Relation u v
fairSubtype = relation cmp
  where
    cmp f g | converge f g = Tree.subtypeCmp f g
            | otherwise = Nothing
