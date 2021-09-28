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

module Common where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Char (chr, ord, isDigit)

type Rel u = Set (u, u)

subscript :: Int -> String
subscript = map convert . show
  where
    convert :: Char -> Char
    convert ch | isDigit ch = chr (ord ch - ord '0' + ord '₀')
               | ch == '-' = '₋'
               | otherwise = error "impossible"

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (\(x, y) -> (x, f y))

limit :: Eq a => (a -> a) -> a -> a
limit f x | x == y = x
          | otherwise = limit f y
  where
    y = f x

zipMap :: Ord k => Map k a -> Map k b -> Map k (a, b) 
zipMap = Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMatched (const (,)))

mergeMap :: Ord k => (a -> c) -> (b -> c) -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeMap f g h = Merge.merge (Merge.mapMissing (const f)) (Merge.mapMissing (const g)) (Merge.zipWithMatched (const h))

showSet :: Show a => Set a -> String
showSet eset | null es = "∅"
             | otherwise = "{" ++ List.intercalate ", " (map show es) ++ "}"
  where
    es = Set.elems eset

closure :: Ord u => Rel u -> Rel u
closure = limit aux
  where
    aux rel = Set.union rel (Set.fromList [ (x, z) | let rels = Set.elems rel
                                                   , (x, y) <- rels
                                                   , (y', z) <- rels
                                                   , y == y' ])
