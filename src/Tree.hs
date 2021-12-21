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

-- |Representation of session types as regular trees.
module Tree where

import Common (limit, subscript)
import Atoms
import Type (Type)
import qualified Type
import Node (Node)
import qualified Node
import Exceptions

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy (State)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as State
import Control.Monad (when)
import Control.Exception (throw)

-- |A 'NodeMap' is a map from node identifiers to nodes.
type NodeMap u = Map u (Node u)

-- |A regular tree consists of the node identifier of its root and of the
-- 'NodeMap' for all of the node identifiers reachable from it.
data Tree u = Tree u (NodeMap u)
  deriving (Show, Eq, Ord)

-- |Representation of an action used to identify the children of a node.
data Action u
  = LabelA Polarity Label
  | ChannelA Polarity

-- |The regular tree resulting from a type uses 'Int's as node identifiers.
type Vertex = Int
-- |The state of the monad used for creating a regular tree from a type
-- comprises the index of the next 'Vertex' that can be used as well as the
-- current map associating each created 'Vertex' with its node.
type CreatorState = (Vertex, Map Vertex (Node TempVertex))
-- |Specialization of the 'State' monad for the creation of a regular tree.
type Creator = State CreatorState
-- |A temporary vertex is used during the construction of a regular tree and can
-- be either a type name or a proper 'Vertex'.
type TempVertex = Either TypeName Vertex

-- |Create a 'NodeMap' from pairs consisting of a node identifier and a node.
makeNodeMap :: Ord u => [(u, Node u)] -> NodeMap u
makeNodeMap = Map.fromListWithKey (\_ -> error "invalid node map")

-- |Transform a 'NodeMap' by transforming all identifiers and all nodes in it.
mapNodeMap :: (Ord u, Ord v) => (u -> v) -> (Node u -> Node v) -> NodeMap u -> NodeMap v
mapNodeMap f g m = makeNodeMap [ (f u, g node) | (u, node) <- Map.toList m ]

-- |Similar to 'zipWith' but for 'NodeMap's using the provided merging function
-- for nodes.
zipNodeMap :: (Ord u, Ord v) => (Node u -> Node v -> [Node (Node.Merge u v)]) -> NodeMap u -> NodeMap v -> NodeMap (Node.Merge u v)
zipNodeMap f m1 m2 = Map.fromList [ (Node.Both i j, node) | (i, n1) <- Map.toList m1, (j, n2) <- Map.toList m2, node <- f n1 n2 ]

-- |Union of two 'NodeMap's. The operation fails if the two maps share the same
-- node identifier.
disjointUnion :: Ord u => NodeMap u -> NodeMap u -> NodeMap u
disjointUnion m1 m2 | Map.disjoint m1 m2 = Map.union m1 m2
disjointUnion _ _ = error "non-disjoint maps"

-- |Add a new node and return its identifier.
addNode :: Node TempVertex -> Creator TempVertex
addNode node = do
  (n, fmap) <- State.get
  State.put (n + 1, Map.insert n node fmap)
  return $ Right n

-- |Applied to a type name and a final node identifier, replaces all occurrences
-- of the type name used as temporary node identifier with the final node
-- identifier.
resolve :: TypeName -> TempVertex -> Creator ()
resolve tname u = do
  (n, fmap) <- State.get
  State.put (n, Map.map (Node.map aux) fmap)
  where
    aux v | v == Left tname = u
          | otherwise       = v

-- |Create a regular tree from a type.
fromType :: Type -> Tree Vertex
fromType t = Tree (vertex root) $ Map.map (Node.map vertex) fmap
  where
    root :: TempVertex
    fmap :: Map Vertex (Node TempVertex)
    (root, (_, fmap)) = State.runState (auxT t) (1, Map.empty)

    vertex :: TempVertex -> Vertex
    vertex (Left tname) = throw $ ErrorUnknownIdentifier "type" (showWithPos tname)
    vertex (Right n) = n

    auxT :: Type -> Creator TempVertex
    auxT (Type.End pol) = addNode (Node.End pol)
    auxT (Type.Rec tname t) = do
      u <- auxT t
      when (u == Left tname) $ throw $ ErrorTypeNonContractive tname
      resolve tname u
      return u
    auxT (Type.Var tname) = return (Left tname)
    auxT (Type.Channel pol s t) = do
      u <- auxT s
      v <- auxT t
      addNode (Node.Channel pol u v)
    auxT (Type.Label pol bs) = do
      bs' <- mapM auxB bs
      addNode (Node.Label pol (Map.fromList bs'))

    auxB :: (Label, Type) -> Creator (Label, TempVertex)
    auxB (label, t) = do
      u <- auxT t
      return (label, u)

-- |Create a type from a regular tree.
toType :: Tree Vertex -> Type
toType (Tree i nodem) = aux [] i
  where
    aux is i | i `elem` is = Type.Var (auxI i)
    aux is i =
      case Map.lookup i nodem of
        Nothing -> error "this should not happen"
        Just node -> let t = auxN (i : is) node in
                     let tname = auxI i in
                     if tname `Set.member` Type.fn t then Type.Rec tname t else t

    auxI i = Identifier Somewhere ("X" ++ subscript i)

    auxN is Node.Nil = error "this should not happen either"
    auxN is (Node.End pol) = Type.End pol
    auxN is (Node.Channel pol i j) = Type.Channel pol (aux is i) (aux is j)
    auxN is (Node.Label pol bm) = Type.Label pol [ (label, aux is i) | (label, i) <- Map.toList bm ]

-- |Renumber the node identifiers of a regular tree starting from 0.
remap :: Ord u => Tree u -> Tree Vertex
remap (Tree i m) = Tree (aux i) (mapNodeMap aux (Node.map aux) m)
  where
    imap = Map.fromList (zip (Map.keys m) [0..])

    aux i | Just n <- Map.lookup i imap = n
    aux _ = error "nope"

-- |Expose the top-level structure of a regular tree.
unfold :: Ord u => Tree u -> Node (Tree u)
unfold (Tree i nodem) =
  case Map.lookup i nodem of
    Nothing -> Node.Nil
    Just node -> Node.map (`Tree` nodem) node

-- |Compute the dual of a regular tree. The result uses 'Left'- and
-- 'Right'-injected node identifiers because not every node of the original tree
-- may need to be dualized.
dual :: Ord u => Tree u -> Tree (Either u u)
dual (Tree i m) = Tree (Right i) (disjointUnion ml mr)
  where
    ml = mapNodeMap Left (Node.map Left) m
    mr = mapNodeMap Right Node.dual m

-- |Compute the behavioral difference between two regular trees.
difference :: Ord u => Tree u -> Tree u -> Tree (Node.Merge u u)
difference (Tree i1 m1) (Tree i2 m2) = Tree (Node.Both i1 i2) m
  where
    ml = mapNodeMap Node.OnlyLeft (Node.map Node.OnlyLeft) m1
    md = zipNodeMap Node.difference m1 m2
    m  = ml `disjointUnion` md

-- |Labeled-transition system originating from the root or a regular tree.
actions :: Ord u => Tree u -> [(Action u, Tree u)]
actions t =
  case unfold t of
    Node.Nil -> []
    Node.End _ -> []
    Node.Channel pol _ s -> [(ChannelA pol, s)]
    Node.Label pol tm -> [ (LabelA pol label, s) | (label, s) <- Map.toList tm ]

-- |Compute the successors of a regular tree after every action that satisfies a
-- given predicate.
after :: Ord u => (Action u -> Bool) -> Tree u -> [Tree u]
after p = map snd . filter (p . fst) . actions

-- |Set of node identifiers reachable from the root of a regular tree.
reachable :: Ord u => Tree u -> Set u
reachable g@(Tree u nodem) = limit aux (Set.singleton u)
  where
    aux uset = Set.union uset (Set.unions [ Node.reachable n | (u, n) <- Map.toList nodem, u `Set.member` uset ])

-- |Restrict the 'NodeMap' of a regular tree to the set of node identifiers that
-- are reachable from the root of the tree.
reduce :: Ord u => Tree u -> Tree u
reduce g@(Tree u nodem) = Tree u (Map.filterWithKey (\v _ -> v `Set.member` uset) nodem)
  where
    uset = reachable g

-- |A __regular tree comparator__ is a function taking two regular trees and
-- returning either 'Nothing', if the comparison fails, or 'Just' two lists of
-- pairs of regular trees that must be compared in turn, where the meaning of
-- the two lists is the same as for a 'Node.Comparator'.
type Comparator u = Tree u -> Tree u -> Maybe ([(Tree u, Tree u)], [(Tree u, Tree u)])

-- |Equality comparison for regular trees.
equalityCmp :: Ord u => Comparator u
equalityCmp f g = Node.equalityCmp (unfold f) (unfold g)

-- | Strong subtyping comparison for regular trees.
strongSubCmp :: Ord u => Comparator u
strongSubCmp f g = Node.strongSubCmp (unfold f) (unfold g)

-- | Weak subtyping comparison for regular trees.
weakSubCmp :: Ord u => Comparator u
weakSubCmp f g = Node.weakSubCmp (unfold f) (unfold g)
