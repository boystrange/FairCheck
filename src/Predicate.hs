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

module Predicate (defined, viable, bounded) where

import Atoms
import qualified Node
import qualified Tree
import Formula

type F u = Formula (Tree.Action u) (Tree.Tree u)

definedF :: Ord u => F u
definedF = Formula.Nu (If aux `Formula.And` All (const True) (X 0))
  where
    aux g = Tree.unfold g /= Node.Nil

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

type Predicate u = Tree.Tree u -> Bool

defined :: Ord u => Predicate u
defined = Formula.check Tree.after definedF

viable :: Ord u => Predicate u
viable = Formula.check Tree.after viableF

bounded :: Ord u => Predicate u
bounded = Formula.check Tree.after boundedF
