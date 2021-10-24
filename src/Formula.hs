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

-- |Implementation of a naive model checker for the μ-calculus used in the
-- decision algorithm for fair subtyping.
module Formula where

import Data.Set (Set)
import qualified Data.Set as Set

import Common (limit)

-- |Representation of a μ-calculus formula. The two type parameters respectively
-- represent the type of labels in the labeled transition system and the type of
-- states.
data Formula a s
  -- |Fixpoint variable as de Bruijn index.
  = X Int
  -- |Proposition over a state.
  | If (s -> Bool)
  -- |Disjunction.
  | Or (Formula a s) (Formula a s)
  -- |Conjunction.
  | And (Formula a s) (Formula a s)
  -- |Diamond.
  | Any (a -> Bool) (Formula a s)
  -- |Box.
  | All (a -> Bool) (Formula a s)
  -- |Least fixed point.
  | Mu (Formula a s)
  -- |Greatest fixed point.
  | Nu (Formula a s)

-- |The always false formula.
false :: Formula a s
false = If (const False)

-- |The always true formula.
true :: Formula a s
true = If (const True)

-- |Another name for the disjunction operator.
or :: Formula a s -> Formula a s -> Formula a s
or = Or

-- |Another name for the conjunction operator.
and :: Formula a s -> Formula a s -> Formula a s
and = And

-- |Negation of a formula computed by duality (there is no native negation
-- operator).
neg :: Formula a s -> Formula a s
neg (X x) = X x
neg (If p) = If (not . p)
neg (Or f g) = And (neg f) (neg g)
neg (And f g) = Or (neg f) (neg g)
neg (Any p f) = All p (neg f)
neg (All p f) = Any p (neg f)
neg (Mu f) = Nu (neg f)
neg (Nu f) = Mu (neg f)

-- |Compute the semantics of a formula, namely the set of states for which the
-- formula holds, given an initial set of states and a transition function.
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

-- |Check whether a formula is true for a given state and a given transition
-- function.
check :: Ord s => ((a -> Bool) -> s -> [s]) -> Formula a s -> s -> Bool
check after f s = s `Set.member` semantics uset after f
  where
    uset = limit reachable (Set.singleton s)

    reachable set = set `Set.union` Set.unions [ Set.fromList (after (const True) s) | s <- Set.elems set ]
