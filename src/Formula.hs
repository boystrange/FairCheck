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

module Formula where

import Data.Set (Set)
import qualified Data.Set as Set

import Common (limit)

data Formula a s
  = X Int
  | If (s -> Bool)
  | Or (Formula a s) (Formula a s)
  | And (Formula a s) (Formula a s)
  | Any (a -> Bool) (Formula a s)
  | All (a -> Bool) (Formula a s)
  | Mu (Formula a s)
  | Nu (Formula a s)

false :: Formula a s
false = If (const False)

true :: Formula a s
true = If (const True)

or :: Formula a s -> Formula a s -> Formula a s
or = Or

and :: Formula a s -> Formula a s -> Formula a s
and = And

neg :: Formula a s -> Formula a s
neg (X x) = X x
neg (If p) = If (not . p)
neg (Or f g) = And (neg f) (neg g)
neg (And f g) = Or (neg f) (neg g)
neg (Any p f) = All p (neg f)
neg (All p f) = Any p (neg f)
neg (Mu f) = Nu (neg f)
neg (Nu f) = Mu (neg f)

semantics :: Ord s => Set s -> ((a -> Bool) -> s -> [s]) -> Formula a s -> Set s
semantics uset after = aux []
  where
    aux val (X n) = val!!n
    aux _ (If p) = Set.filter p uset
    aux val (Or f g) = aux val f `Set.union` aux val g
    aux val (And f g) = aux val f `Set.intersection` aux val g
    aux val (Any p f) = Set.filter (any (`Set.member` aux val f) . after p) uset
    aux val (All p f) = Set.filter (all (`Set.member` aux val f) . after p) uset
    aux val (Mu f) = limit (\s -> aux (s : val) f) Set.empty 
    aux val (Nu f) = limit (\s -> aux (s : val) f) uset

check :: Ord s => ((a -> Bool) -> s -> [s]) -> Formula a s -> s -> Bool
check after f s = s `Set.member` semantics uset after f
  where
    uset = limit reachable (Set.singleton s)

    reachable set = set `Set.union` Set.unions [ Set.fromList (after (const True) s) | s <- Set.elems set ]
