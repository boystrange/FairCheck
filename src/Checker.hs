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

-- |Implementation of the type checker.
module Checker where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (forM_, when, unless)
import Control.Exception (throw)
import Data.Maybe ( fromJust )

import Common
import Atoms
import Type (Type)
import qualified Type
import Process
import Exceptions
import Tree (Tree, Vertex)
import qualified Tree
import qualified Node
import qualified Predicate
import qualified Relation

-- |A __context__ is a finite map from channel names to session types
-- represented as regular trees.
type Context = Map ChannelName (Tree Vertex)

-- |The type of the function that decides whether two session types are related
-- by subtyping.
type Subtype = Tree Vertex -> Tree Vertex -> Bool

-- |Compute the __rank__ of a process given a list of process definitions
-- (Definition F.1).
rank :: [ProcessDef] -> Process -> Int
rank pdefs = aux []
  where
    pmap :: Map ProcessName Process
    pmap = Map.fromList [ (pname, p) | (pname, _, Just p) <- pdefs ]

    aux :: [ProcessName] -> Process -> Int
    aux _ Done = 0
    aux pnames (Call pname _) | pname `elem` pnames = 0
    aux pnames (Call pname _) =
        case Map.lookup pname pmap of
            Nothing -> 0 -- an undefined process is assumed to have rank 0
            Just p -> aux (pname : pnames) p
    aux pnames (Wait _ p) = aux pnames p
    aux _ (Close _) = 0
    aux pnames (Channel _ _ _ p) = aux pnames p
    aux pnames (Label _ _ cs) = maximum (map (aux pnames . snd) cs)
    aux pnames (New _ _ p q) = 1 + aux pnames p + aux pnames q
    aux pnames (Cast _ _ p) = 1 + aux pnames p
    aux pnames (Choice _ p) = aux pnames p

-- |Check whether all process definitions are __action bounded__ (Section 5.1).
checkActionBoundedness :: [ProcessDef] -> IO ()
checkActionBoundedness pdefs = forM_ pdefs check
  where
    pmap :: Map ProcessName Process
    pmap = Map.fromList [ (pname, p) | (pname, _, Just p) <- pdefs ]

    check :: ProcessDef -> IO ()
    check (pname, _, Nothing) = return ()
    check (pname, _, Just p) | aux [pname] p = return ()
    check (pname, _, _) = throw $ ErrorActionUnbounded pname

    aux :: [ProcessName] -> Process -> Bool
    aux _ Done = True
    -- If we encounter 'pname' after we have unfolded its definition once we
    -- declare that it is unbounded
    aux pnames (Call pname _) | pname `elem` pnames = False
    aux pnames (Call pname _) =
      case Map.lookup pname pmap of
          Nothing -> True -- an undefined process is assumed to be action bounded
          Just p -> aux (pname : pnames) p
    aux pnames (Wait _ p) = aux pnames p
    aux _ (Close _) = True
    aux pnames (Channel _ _ _ p) = aux pnames p
    -- The input/output of a label is action bounded if so is any of its
    -- branches
    aux pnames (Label _ _ cs) = any (aux pnames . snd) cs
    -- A new session is action bounded if so are the sub-processes using the two
    -- endpoints of the session
    aux pnames (New _ _ p q) = aux pnames p && aux pnames q
    aux pnames (Cast _ _ p) = aux pnames p
    aux pnames (Choice _ p) = aux pnames p

-- |Remove a channel from a context, returning the remaining context and the
-- session type associated with the channel.
remove :: Context -> ChannelName -> IO (Context, Tree Vertex)
remove ctx x =
  case Map.lookup x ctx of
    Nothing -> throw $ ErrorUnknownIdentifier "channel" (showWithPos x)
    Just t -> return (Map.delete x ctx, t)

-- |Compute the __process order__, which is the least preorder such that A ≤ B
-- implies that A is invoked along a termination path in the definition of B.
makeProcessOrder :: [ProcessDef] -> Set (ProcessName, ProcessName)
makeProcessOrder pdefs = closure (Set.fromList [ (x, y) | (y, _, Just p) <- pdefs
                                                        , x <- Set.elems (pn p)])

-- |Compute the map associating each process name with the list of session types
-- associated with its free channel names, in the order they appear in the
-- process definition.
makeProcessContext :: [ProcessDef] -> Map ProcessName [Tree Vertex]
makeProcessContext pdefs = Map.fromList [ (pname, map (Tree.fromType . snd) us) | (pname, us, _) <- pdefs ]

-- |Check whether all process definitions are __session bounded__ (Section 5.2)
-- and __cast bounded__ (Section 5.3).
checkRanks :: [ProcessDef] -> IO ()
checkRanks pdefs = do
  -- We only need to check for session/cast boundedness for recursive processes,
  -- namely those that invoke themselves.
  forM_ [ (x, p) | (x, _, Just p) <- pdefs, (x, x) `Set.member` order ] (uncurry check)
  where
    order :: Set (ProcessName, ProcessName)
    order = makeProcessOrder pdefs

    -- Check that no session creations and no casts are encountered along any
    -- termination path of the process. If this is the case, the process is
    -- session and/or cast unbounded.
    check :: ProcessName -> Process -> IO ()
    check pname = aux
      where
        aux Done = return ()
        aux (Call _ _) = return ()
        aux (Wait _ p) = aux p
        aux (Close _) = return ()
        aux (Channel _ _ _ p) = aux p
        aux (Label _ _ cs) = forM_ cs (aux . snd)
        aux (New x _ _ _) = throw $ ErrorSessionUnbounded pname x
        aux (Cast x _ _) = throw $ ErrorCastUnbounded pname x
        -- We implicitly assume that the branch of a non-deterministic choice
        -- leading to termination is the right one, therefore the rank of a
        -- non-deterministic choice is the rank of that branch.
        aux (Choice _ p) = aux p

-- |Check that all provided process definitions are well typed using the
-- algorithmic version of the type system (Section F.2). The first argument is
-- the subtyping relation being used, so that it is possible to choose among
-- fair and unfair subtyping.
checkTypes :: Subtype -> [ProcessDef] -> IO ()
checkTypes subt pdefs = forM_ pdefs auxD
  where
    -- Create the global assignment associating each process name with the list
    -- of session types of the channel names that occur free in its body.
    penv :: Map ProcessName [Tree Vertex]
    penv = makeProcessContext pdefs

    -- Check that a process definition is well typed.
    auxD :: ProcessDef -> IO ()
    auxD (_, us, Nothing) = return ()
    auxD (_, us, Just p) = do
      let ctx = Map.fromListWithKey (\x _ _ -> throw $ ErrorMultipleNameDeclarations x) [ (u, Tree.fromType t) | (u, t) <- us ]
      auxP ctx p

    -- Check that the context is empty. If not, there are some channels left
    -- unused.
    checkEmpty :: Context -> IO ()
    checkEmpty ctx = unless (Map.null ctx) $ throw $ ErrorLinearity (Map.keys ctx)

    -- Check whether two types are equal.
    checkTypeEq :: ChannelName -> Tree Vertex -> Tree Vertex -> IO ()
    checkTypeEq x g1 g2 = do
      unless (Relation.equality g1 g2) $ throw $ ErrorTypeMismatch x (show $ Tree.toType g1) (Tree.toType g2)

    -- Return the list of session types associated with the free names of a
    -- process name.
    checkProcess :: ProcessName -> IO [Tree Vertex]
    checkProcess pname =
      case Map.lookup pname penv of
        Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
        Just gs -> return gs

    -- Check that a process is well typed in a given context.
    auxP :: Context -> Process -> IO ()
    -- Rule [a-done]
    auxP ctx Done = checkEmpty ctx
    -- Rule [a-call]
    auxP ctx (Call pname us) = do
      -- Retrive the list of types of the arguments of 'pname'
      gs <- checkProcess pname
      -- If the expected and actual lists of arguments differ, the invocation is
      -- illegal.
      unless (length us == length gs) $ throw $ ErrorArityMismatch pname (length gs) (length us)
      -- Make sure that the actual arguments are exactly the names that occur in
      -- the context.
      let ctx' = Map.fromList (zip us gs)
      let uset = Map.keysSet ctx
      let vset = Map.keysSet ctx'
      unless (uset == vset) $ throw $ ErrorLinearity $ Set.elems $ Set.union (Set.difference uset vset) (Set.difference vset uset)
      -- Make sure that the expected and actual types of the argument match.
      forM_ (Map.toList (zipMap ctx' ctx)) $ \(x, (g1, g2)) -> checkTypeEq x g1 g2
    -- Rule [a-wait]
    auxP ctx (Wait x p) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Make sure that the type of x is ?end
      checkTypeEq x (Tree.fromType (Type.End In)) t
      -- Type check the continuation.
      auxP ctx p
    -- Rule [a-close]
    auxP ctx (Close x) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Make sure that the remaining context is empty.
      checkEmpty ctx
      -- Make sure that the type of x is !end
      checkTypeEq x (Tree.fromType (Type.End Out)) t
    -- Rule [a-channel-in]
    auxP ctx (Channel x In y p) = do
      -- If y already occurs in the context it shadows a linear name
      when (y `Map.member` ctx) $ throw $ ErrorLinearity [y]
      -- Remove the association for x from the context.
      (ctx, g) <- remove ctx x
      -- Check the shape of the type of x.
      case Tree.unfold g of
        -- If it is the input of a channel, insert the association for y in the
        -- context along with the updated type of x and type check the
        -- continuation.
        Node.Channel In g1 g2 -> auxP (Map.insert y g1 $ Map.insert x g2 ctx) p
        -- If it is any other type, signal the error.
        _ -> throw $ ErrorTypeMismatch x "channel input" (Tree.toType g)
    -- Rule [a-channel-out]
    auxP ctx (Channel x Out y p) = do
      -- Remove the association for x and y from the context.
      (ctx, g) <- remove ctx x
      (ctx, f) <- remove ctx y
      -- Check the shape of the type associated with x.
      case Tree.unfold g of
        -- If it is the output of a channel...
        Node.Channel Out g1 g2 -> do
          -- Check that the type of y matches the expected one.
          checkTypeEq y g1 f
          -- Update the type of x and type check the continuation.
          auxP (Map.insert x g2 ctx) p
        -- If it is any other type...
        _ -> throw $ ErrorTypeMismatch x "channel output" (Tree.toType g)
    -- Rule [a-label]
    auxP ctx (Label x pol cs) = do
      -- Remove the association for x from the context.
      (ctx, g) <- remove ctx x
      -- Check the shape of the type associated with x.
      case Tree.unfold g of
        -- If it is the input/output of a label with the right polarity...
        Node.Label pol' tgm | pol == pol' -> do
          -- Retrieve the set of labels from the type
          let labelset = Map.keysSet tgm
          -- Retrieve the set of labels from the process
          let labelset' = Set.fromList (map fst cs)
          -- If the two sets of labels differ, there is mismatch between type
          -- and process.
          unless (labelset == labelset') $ throw $ ErrorLabelMismatch x (Set.elems labelset) (Set.elems labelset')
          -- Type check each branch after updating the context.
          forM_ (Map.toList (zipMap tgm (Map.fromList cs))) $
            \(label, (f, p)) -> auxP (Map.insert x f ctx) p
        -- If it is the input/output of a label with the wrong polarity...
        Node.Label _ _ -> throw $ ErrorTypeMismatch x ("polarity " ++ show pol) (Tree.toType g)
        -- In all the other cases the type is just the wrong one
        _ -> throw $ ErrorTypeMismatch x "label input/output" (Tree.toType g)
    -- Rule [a-par]
    auxP ctx (New x t p q) = do
      -- If x already occurs in the context we throw an exception, because it
      -- would shadow a linear resource.
      when (x `Map.member` ctx) $ throw $ ErrorLinearity [x]
      let g = Tree.fromType t
      -- Unlike the typing rule shown in the paper, where it is required for the
      -- session types associated with the two endpoints to be compatible, here
      -- we use duality (Theorem 3.15). To be sure that duality implies
      -- compatibility, we require the provided session type to be bounded.
      unless (Predicate.bounded g) $ throw $ ErrorTypeUnbounded x
      -- Compute the channel names occurring free in p and q, excluding x.
      let pnameset = Set.delete x (fn p)
      let qnameset = Set.delete x (fn q)
      let uset = Set.union pnameset qnameset
      let iset = Set.intersection pnameset qnameset
      let eset = Map.keysSet ctx
      -- Only x may occur free in both p and q. If there is another name that
      -- occurs in both processes, then it is used non-linearly.
      unless (Set.null iset) $ throw $ ErrorLinearity (Set.elems iset)
      -- If the union of the channel names occurring free in p and q differs
      -- from the domain of the context, then there is a linearity violation.
      unless (uset == eset) $ throw $ ErrorLinearity (Set.elems (Set.union (Set.difference eset uset) (Set.difference uset eset)))
      -- Compute the contexts for typing p and q.
      let ctxp = Map.restrictKeys ctx pnameset
      let ctxq = Map.restrictKeys ctx qnameset
      auxP (Map.insert x g ctxp) p
      -- In q we compute the dual of the type of x.
      auxP (Map.insert x (Tree.remap $ Tree.dual g) ctxq) q
    -- Rule [a-choice]
    auxP ctx (Choice p q) = do
      -- Type check p and q using the same context.
      auxP ctx p
      auxP ctx q
    -- Rule [a-cast]
    auxP ctx (Cast x s p) = do
      (ctx, g1) <- remove ctx x
      let g2 = Tree.fromType s
      -- Use 'subt' to check that the cast is a valid one.
      unless (subt g1 g2) $ throw $ ErrorInvalidCast x (Tree.toType g1) (Tree.toType g2)
      -- Type check the continuation.
      auxP (Map.insert x g2 ctx) p
