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

-- |μ-calculus formulas used in the algorithm for fair subtyping.
module Predicate (defined, viable, bounded) where

import Atoms
import qualified Node
import qualified Tree
import Formula

-- |Alias for the type of μ-calculus formulas used in this particular module.
type F u = Formula (Tree.Action u) (Tree.Tree u)

definedF :: Ord u => F u
definedF = Formula.Nu (If aux `Formula.And` All (const True) (X 0))
  where
    aux g = Tree.unfold g /= Node.Nil

-- |A session type is __ended__ if it is constructed with 'Node.End'.
endF :: Ord u => F u
endF = If aux
  where
    aux g = case Tree.unfold g of
              Node.End _ -> True
              _ -> False

boundedF :: Ord u => F u
boundedF = Formula.Nu (toEndF `Formula.and` All (const True) (X 0))
  where
    toEndF = Formula.Mu (endF `Formula.or` Any (const True) (X 0))

-- |Formula for viability. Informally, a session type is viable if it is ended,
-- or if it is an input with at least one viable branch, or if it is an output
-- with all viable branches.
viableF :: Ord u => F u
viableF = Formula.Nu (Formula.Mu (endF `Formula.or` Any input (X 0) `Formula.or` (Any output (X 0) `Formula.and` All output (X 1))))
  where
    hasPolarity :: Polarity -> Tree.Action u -> Bool
    hasPolarity p (Tree.LabelA q _) = p == q
    hasPolarity p (Tree.ChannelA q) = p == q

    input :: Tree.Action u -> Bool
    input = hasPolarity In

    output :: Tree.Action u -> Bool
    output = hasPolarity Out

-- |Alias for predicates over session types represented as regular trees.
type Predicate u = Tree.Tree u -> Bool

-- |A session type is __defined__ if none of its subtrees is 'Node.Nil'.
defined :: Ord u => Predicate u
defined = Formula.check Tree.after definedF

-- |A session type is __viable__ if each of its subtrees has a session type
-- compatible with it.
viable :: Ord u => Predicate u
viable = Formula.check Tree.after viableF

-- |A session type is __bounded__ if each of its subtrees has a path to an ended
-- session type.
bounded :: Ord u => Predicate u
bounded = Formula.check Tree.after boundedF
