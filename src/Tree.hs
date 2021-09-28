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

type NodeMap u = Map u (Node u)

data Tree u = Tree u (NodeMap u)
  deriving (Show, Eq, Ord)

data Action u
  = LabelA Polarity Label
  | ChannelA Polarity

type Vertex = Int
type CreatorState = (Vertex, Map Vertex (Node TempVertex))
type Creator = State CreatorState
type TempVertex = Either TypeName Vertex

makeNodeMap :: Ord u => [(u, Node u)] -> NodeMap u
makeNodeMap = Map.fromListWithKey (\_ -> error "invalid node map")

mapNodeMap :: (Ord u, Ord v) => (u -> v) -> (Node u -> Node v) -> NodeMap u -> NodeMap v
mapNodeMap f g m = makeNodeMap [ (f u, g node) | (u, node) <- Map.toList m ]

zipNodeMap :: (Ord u, Ord v) => (Node u -> Node v -> [Node (Node.Merge u v)]) -> NodeMap u -> NodeMap v -> NodeMap (Node.Merge u v)
zipNodeMap f m1 m2 = Map.fromList [ (Node.Both i j, node) | (i, n1) <- Map.toList m1, (j, n2) <- Map.toList m2, node <- f n1 n2 ]

disjointUnion :: Ord u => NodeMap u -> NodeMap u -> NodeMap u
disjointUnion m1 m2 | Map.disjoint m1 m2 = Map.union m1 m2
disjointUnion _ _ = error "non-disjoint maps"

addNode :: Node TempVertex -> Creator TempVertex
addNode node = do
  (n, fmap) <- State.get
  State.put (n + 1, Map.insert n node fmap)
  return $ Right n

resolve :: TypeName -> TempVertex -> Creator ()
resolve tname u = do
  (n, fmap) <- State.get
  State.put (n, Map.map (Node.map aux) fmap)
  where
    aux v | v == Left tname = u
          | otherwise       = v

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

remap :: Ord u => Tree u -> Tree Vertex
remap (Tree i m) = Tree (aux i) (mapNodeMap aux (Node.map aux) m)
  where
    imap = Map.fromList (zip (Map.keys m) [0..])

    aux i | Just n <- Map.lookup i imap = n
    aux _ = error "nope"

unfold :: Ord u => Tree u -> Node (Tree u)
unfold (Tree i nodem) =
  case Map.lookup i nodem of
    Nothing -> Node.Nil
    Just node -> Node.map (`Tree` nodem) node

dual :: Ord u => Tree u -> Tree (Either u u)
dual (Tree i m) = Tree (Right i) (disjointUnion ml mr)
  where
    ml = mapNodeMap Left (Node.map Left) m
    mr = mapNodeMap Right Node.dual m    

difference :: (Ord u, Ord v) => Tree u -> Tree v -> Tree (Node.Merge u v)
difference (Tree i1 m1) (Tree i2 m2) = Tree (Node.Both i1 i2) m
  where
    ml = mapNodeMap Node.OnlyLeft (Node.map Node.OnlyLeft) m1
    md = zipNodeMap Node.difference m1 m2
    m  = ml `disjointUnion` md

meet :: (Ord u, Ord v) => Tree u -> Tree v -> Tree (Node.Merge u v)
meet (Tree i1 m1) (Tree i2 m2) = Tree (Node.Both i1 i2) m
  where
    ml = mapNodeMap Node.OnlyLeft (Node.map Node.OnlyLeft) m1
    mr = mapNodeMap Node.OnlyRight (Node.map Node.OnlyRight) m2
    mm = zipNodeMap Node.meet m1 m2
    m = ml `disjointUnion` mr `disjointUnion` mm

actions :: Ord u => Tree u -> [(Action u, Tree u)]
actions t =
  case unfold t of
    Node.Nil -> []
    Node.End _ -> []
    Node.Channel pol _ s -> [(ChannelA pol, s)]
    Node.Label pol tm -> [ (LabelA pol label, s) | (label, s) <- Map.toList tm ]

after :: Ord u => (Action u -> Bool) -> Tree u -> [Tree u]
after p = map snd . filter (p . fst) . actions

reachable :: Ord u => Tree u -> Set u
reachable g@(Tree u nodem) = limit aux (Set.singleton u)
  where
    aux uset = Set.union uset (Set.unions [ Node.reachable n | (u, n) <- Map.toList nodem, u `Set.member` uset ])

reduce :: Ord u => Tree u -> Tree u
reduce g@(Tree u nodem) = Tree u (Map.filterWithKey (\v _ -> v `Set.member` uset) nodem)
  where
    uset = reachable g

type Comparator u v = Tree u -> Tree v -> Maybe ([(Tree u, Tree v)], [(Tree u, Tree v)])

equalityCmp :: (Ord u, Ord v) => Comparator u v
equalityCmp f g = Node.equalityCmp (unfold f) (unfold g)

subtypeCmp :: (Ord u, Ord v) => Comparator u v
subtypeCmp f g = Node.subtypeCmp (unfold f) (unfold g)
