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

-- |Representation of a __node__ in a regular tree type.
module Node where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Set (Set)
import qualified Data.Set as Set

import Common (zipMap, mergeMap)
import Atoms ( Polarity(..), Label, dualP )

-- |Result of merging two node identifiers, depending on whether only the left
-- one is present, or only the right one is present, or both are present.
data Merge u v = OnlyLeft u | OnlyRight v | Both u v
  deriving (Eq, Ord)

diffMap :: Ord k => Map k a -> Map k b -> Map k (Merge a b)
diffMap = Merge.merge aux Merge.dropMissing (Merge.zipWithMatched (const Both))
  where
    aux = Merge.mapMissing (const OnlyLeft)

-- |Representation of a node. The parameter 'u' is the type of node identifiers
-- used for referring to other nodes.
data Node u
  -- |Special node indicating a non-viable type.
  = Nil
  -- |Termination.
  | End Polarity
  -- |Input/output of a channel.
  | Channel Polarity u u
  -- |Input/output of a label.
  | Label Polarity (Map Label u)
  deriving (Eq, Ord)

-- |A __node comparator__ is a function taking two nodes and returning either
-- 'Nothing', if the comparison fails, or 'Just' two lists of pairs of node
-- identifiers that must be compared in turn. The first list refers to
-- nodes that must be checked for __equality__, whereas the second list refers
-- to nodes that must be compared corecursively, according to the same comparing
-- being made between the two initial nodes.
type Comparator u v = Node u -> Node v -> Maybe ([(u, v)], [(u, v)])

instance Show u => Show (Node u) where
  show Nil = "<nil>"
  show (End pol) = show pol ++ "end"
  show (Channel pol u v) = show pol ++ show u ++ "." ++ show v
  show (Label pol bm) = show pol ++ "{" ++ List.intercalate ", " (Prelude.map aux (Map.toList bm)) ++ "}"
    where
      aux (label, u) = show label ++ ": " ++ show u

-- |Dual of a node. Not every node reachable from this one may need to be
-- dualized, so in the result every node identifier may have been 'Left' as it
-- was before the dualization or it may have been 'Right'ly dualized.
dual :: Ord u => Node u -> Node (Either u u)
dual Nil = Nil
dual (End pol) = End (dualP pol)
dual (Channel pol u v) = Channel (dualP pol) (Left u) (Right v)
dual (Label pol bm) = Label (dualP pol) (Map.map Right bm)

-- |Set of reachable node identifiers.
reachable :: Ord u => Node u -> Set u
reachable Nil = Set.empty
reachable (End _) = Set.empty
reachable (Channel _ _ u) = Set.singleton u
reachable (Label _ bm) = Set.fromList $ Map.elems bm

-- |Map the identifiers of a node.
map :: (u -> v) -> Node u -> Node v
map _ Nil = Nil
map _ (End pol) = End pol
map f (Channel pol i j) = Channel pol (f i) (f j)
map f (Label pol bm) = Label pol (Map.map f bm)

-- |Equality comparison between two nodes.
equalityCmp :: (Ord u, Ord v) => Comparator u v
equalityCmp (End p) (End q) | p == q = Just ([], [])
equalityCmp (Channel p u1 u2) (Channel q v1 v2) | p == q = Just ([], [(u1, v1), (u2, v2)])
equalityCmp (Label p um) (Label q vm) | p == q
                                      , Map.keysSet um == Map.keysSet vm = Just ([], Prelude.map snd (Map.toList (zipMap um vm)))
equalityCmp _ _ = Nothing

-- |Subtyping comparison between two nodes.
subtypeCmp :: (Ord u, Ord v) => Comparator u v
subtypeCmp (End p) (End q) | p == q = Just ([], [])
subtypeCmp (Channel p u1 u2) (Channel q v1 v2) | p == q = Just ([(u1, v1)], [(u2, v2)])
subtypeCmp (Label In um) (Label In vm) | Map.keysSet um `Set.isSubsetOf` Map.keysSet vm = Just ([], Prelude.map snd (Map.toList (zipMap um vm)))
subtypeCmp (Label Out um) (Label Out vm) | Map.keysSet vm `Set.isSubsetOf` Map.keysSet um = Just ([], Prelude.map snd (Map.toList (zipMap um vm)))
subtypeCmp _ _ = Nothing

-- |Behavioral difference between two nodes. The result is either a singleton
-- list with the result of the difference or the empty list if the difference is
-- undefined.
difference :: Node u -> Node v -> [Node (Merge u v)]
difference (Channel p i1 i2) (Channel q _ j2) | p == q = [Channel p (OnlyLeft i1) (Both i2 j2)]
difference (Label In bm1) (Label In bm2) | Map.keysSet bm1 `Set.isSubsetOf` Map.keysSet bm2 = [Label In (diffMap bm1 bm2)]
difference (Label Out bm1) (Label Out bm2) | Map.keysSet bm2 `Set.isSubsetOf` Map.keysSet bm1 = [Label Out (diffMap bm1 bm2)]
difference _ _ = []

-- |Behavioral meet between two nodes. The result is either a singleton list
-- with the result of the meet or the empty list if the meet is undefined.
meet :: Node u -> Node v -> [Node (Merge u v)]
meet (Channel p i1 i2) (Channel q _ j2) | p == q = [Channel p (OnlyLeft i1) (Both i2 j2)]
meet (Label In bm1) (Label In bm2) | not (Map.disjoint bm1 bm2) = [Label In $ Map.intersectionWith Both bm1 bm2]
meet (Label Out bm1) (Label Out bm2) | not (Map.disjoint bm1 bm2) = [Label Out $ mergeMap OnlyLeft OnlyRight Both bm1 bm2]
meet _ _ = []
