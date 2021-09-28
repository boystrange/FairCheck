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

type Context = Map ChannelName (Tree Vertex)
type Subtype = Tree Vertex -> Tree Vertex -> Bool

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
    aux pnames (Call pname _) | pname `elem` pnames = False
    aux pnames (Call pname _) =
      case Map.lookup pname pmap of
          Nothing -> True -- an undefined process is assumed to be action bounded
          Just p -> aux (pname : pnames) p
    aux pnames (Wait _ p) = aux pnames p
    aux _ (Close _) = True
    aux pnames (Channel _ _ _ p) = aux pnames p
    aux pnames (Label _ _ cs) = any (aux pnames . snd) cs
    aux pnames (New _ _ p q) = aux pnames p && aux pnames q
    aux pnames (Cast _ _ p) = aux pnames p
    aux pnames (Choice _ p) = aux pnames p

remove :: Context -> ChannelName -> IO (Context, Tree Vertex)
remove ctx x =
  case Map.lookup x ctx of
    Nothing -> throw $ ErrorUnknownIdentifier "channel" (showWithPos x)
    Just t -> return (Map.delete x ctx, t)

makeProcessOrder :: [ProcessDef] -> Set (ProcessName, ProcessName)
makeProcessOrder pdefs = closure (Set.fromList [ (x, y) | (y, _, Just p) <- pdefs
                                                        , x <- Set.elems (pn p)])

makeProcessContext :: [ProcessDef] -> Map ProcessName [Tree Vertex]
makeProcessContext pdefs = Map.fromList [ (pname, map (Tree.fromType . snd) us) | (pname, us, _) <- pdefs ]

checkRanks :: [ProcessDef] -> IO ()
checkRanks pdefs = do
  forM_ [ (x, p) | (x, _, Just p) <- pdefs, (x, x) `Set.member` order ] (uncurry check)
  where
    order :: Set (ProcessName, ProcessName)
    order = makeProcessOrder pdefs

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
        aux (Choice _ p) = aux p

checkTypes :: Subtype -> [ProcessDef] -> IO ()
checkTypes subt pdefs = forM_ pdefs auxD
  where
    penv :: Map ProcessName [Tree Vertex]
    penv = makeProcessContext pdefs

    auxD :: ProcessDef -> IO ()
    auxD (_, us, Nothing) = return ()
    auxD (_, us, Just p) = do
      let ctx = Map.fromListWithKey (\x _ _ -> throw $ ErrorMultipleNameDeclarations x) [ (u, Tree.fromType t) | (u, t) <- us ]
      auxP ctx p

    checkEmpty :: Context -> IO ()
    checkEmpty ctx = unless (Map.null ctx) $ throw $ ErrorLinearity (Map.keys ctx)

    checkTypeEq :: ChannelName -> Tree Vertex -> Tree Vertex -> IO ()
    checkTypeEq x g1 g2 = do
      unless (Relation.equality g1 g2) $ throw $ ErrorTypeMismatch x (show $ Tree.toType g1) (Tree.toType g2)

    checkProcess :: ProcessName -> IO [Tree Vertex]
    checkProcess pname =
      case Map.lookup pname penv of
        Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
        Just gs -> return gs

    auxP :: Context -> Process -> IO ()
    auxP ctx Done = checkEmpty ctx
    auxP ctx (Call pname us) = do
      gs <- checkProcess pname
      unless (length us == length gs) $ throw $ ErrorArityMismatch pname (length gs) (length us)
      let ctx' = Map.fromList (zip us gs)
      let uset = Map.keysSet ctx
      let vset = Map.keysSet ctx'
      unless (uset == vset) $ throw $ ErrorLinearity $ Set.elems $ Set.union (Set.difference uset vset) (Set.difference vset uset)
      forM_ (Map.toList (zipMap ctx' ctx)) $ \(x, (g1, g2)) -> checkTypeEq x g1 g2
    auxP ctx (Wait x p) = do
      (ctx, t) <- remove ctx x
      checkTypeEq x (Tree.fromType (Type.End In)) t
      auxP ctx p
    auxP ctx (Close x) = do
      (ctx, t) <- remove ctx x
      checkEmpty ctx
      checkTypeEq x (Tree.fromType (Type.End Out)) t
    auxP ctx (Channel x In y p) = do
      when (y `Map.member` ctx) $ throw $ ErrorLinearity [y]
      (ctx, g) <- remove ctx x
      case Tree.unfold g of
        Node.Channel In g1 g2 -> auxP (Map.insert y g1 $ Map.insert x g2 ctx) p
        _ -> throw $ ErrorTypeMismatch x "channel input" (Tree.toType g)
    auxP ctx (Channel x Out y p) = do
      (ctx, g) <- remove ctx x
      (ctx, f) <- remove ctx y
      case Tree.unfold g of
        Node.Channel Out g1 g2 -> do
          checkTypeEq y g1 f
          auxP (Map.insert x g2 ctx) p
        _ -> throw $ ErrorTypeMismatch x "channel output" (Tree.toType g)
    auxP ctx (Label x pol cs) = do
      (ctx, g) <- remove ctx x
      case Tree.unfold g of
        Node.Label pol' tgm | pol == pol' -> do
          let labelset = Map.keysSet tgm
          let labelset' = Set.fromList (map fst cs)
          unless (labelset == labelset') $ throw $ ErrorLabelMismatch x (Set.elems labelset) (Set.elems labelset')
          forM_ (Map.toList (zipMap tgm (Map.fromList cs))) $
            \(label, (f, p)) -> auxP (Map.insert x f ctx) p
        Node.Label _ _ -> throw $ ErrorTypeMismatch x ("polarity " ++ show pol) (Tree.toType g)
        _ -> throw $ ErrorTypeMismatch x "label input/output" (Tree.toType g)
    auxP ctx (New x t p q) = do
      when (x `Map.member` ctx) $ throw $ ErrorLinearity [x]
      let g = Tree.fromType t
      unless (Predicate.bounded g) $ throw $ ErrorTypeUnbounded x
      let pnameset = Set.delete x (fn p)
      let qnameset = Set.delete x (fn q)
      let uset = Set.union pnameset qnameset
      let iset = Set.intersection pnameset qnameset
      let eset = Map.keysSet ctx
      unless (Set.null iset) $ throw $ ErrorLinearity (Set.elems iset)
      unless (uset == eset) $ throw $ ErrorLinearity (Set.elems (Set.union (Set.difference eset uset) (Set.difference uset eset)))
      let ctxp = Map.restrictKeys ctx pnameset
      let ctxq = Map.restrictKeys ctx qnameset
      auxP (Map.insert x g ctxp) p
      auxP (Map.insert x (Tree.remap $ Tree.dual g) ctxq) q
    auxP ctx (Choice p q) = do
      auxP ctx p
      auxP ctx q
    auxP ctx (Cast x s p) = do
      (ctx, g1) <- remove ctx x
      let g2 = Tree.fromType s
      unless (subt g1 g2) $ throw $ ErrorInvalidCast x (Tree.toType g1) (Tree.toType g2)
      auxP (Map.insert x g2 ctx) p

{-
type Env = Map ChannelName Type

inferCasts :: [ProcessDef] -> IO ()
inferCasts pdefs = forM_ pdefs auxD
  where
    penv :: Map ProcessName [Type]
    penv = Map.fromList [ (pname, map snd us) | (pname, us, _) <- pdefs ]

    order :: Set (ProcessName, ProcessName)
    order = makeProcessOrder pdefs

    checkLoop :: Maybe ProcessName -> ProcessName -> Bool
    checkLoop Nothing _ = False
    checkLoop (Just pname) pname' = (pname, pname') `Set.member` order

    extract :: Env -> ChannelName -> IO (Env, Type)
    extract ctx x =
      case Map.lookup x ctx of
        Nothing -> throw $ ErrorUnknownIdentifier "channel" (showWithPos x)
        Just t -> return (Map.delete x ctx, t)

    checkProcess :: ProcessName -> IO [Type]
    checkProcess pname =
      case Map.lookup pname penv of
        Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
        Just ts -> return ts

    checkSub :: ChannelName -> Type -> Type -> IO ()
    checkSub = undefined

    auxD :: ProcessDef -> IO ()
    auxD (_, _, Nothing) = return ()
    auxD (pname, us, Just p) = do
      (ctx, loop) <- auxP (Just pname) p
      -- se loop controllare us = ctx
      -- se not loop, controllare us <= ctx
      undefined
    
    auxP :: Maybe ProcessName -> Process -> IO (Env, Bool)
    auxP pname Done = return (Map.empty, False)
    auxP pname (Call pname' us) = do
      ts <- checkProcess pname'
      unless (length us == length ts) $ throw $ ErrorArityMismatch pname' (length ts) (length us)
      return (Map.fromList (zip us ts), checkLoop pname pname')
    auxP pname (Wait x p) = do
      (env, loop) <- auxP pname p
      when (x `Map.member` env) $ throw $ ErrorLinearity [x]
      return (Map.insert x (Type.End In) env, loop)
    auxP pname (Close x) = do
      return (Map.singleton x (Type.End Out), False)
    auxP pname (Channel x In y p) = do
      (env, loop) <- auxP pname p
      (env, tx) <- extract env x
      (env, ty) <- extract env y
      return (Map.insert x (Type.Channel In ty tx) env, loop)
    auxP pname (Channel x Out y p) = do
      (env, loop) <- auxP pname p
      when (y `Map.member` env) $ throw $ ErrorLinearity [y]
      (env, tx) <- extract env x
      undefined -- type of y?
    auxP pname (New x t p q) = do
      (penv, ploop) <- auxP pname p
      (qenv, qloop) <- auxP pname q
      when (ploop || qloop) $ throw $ ErrorSessionUnbounded (fromJust pname) x
      (penv, px) <- extract penv x
      (qenv, qx) <- extract qenv x
      unless (Map.disjoint penv qenv) $ throw $ ErrorLinearity $ Map.keys (Map.intersection  penv qenv)

      let g = Tree.Tree.fromType t
      unless (Predicate.bounded g) $ throw $ ErrorTypeUnbounded x
      let pnameset = Set.delete x (fn p)
      let qnameset = Set.delete x (fn q)
      let uset = Set.union pnameset qnameset
      let iset = Set.intersection pnameset qnameset
      let eset = Map.keysSet ctx
      unless (Set.null iset) $ throw $ ErrorLinearity (Set.elems iset)
      unless (uset == eset) $ throw $ ErrorLinearity (Set.elems (Set.union (Set.difference eset uset) (Set.difference uset eset)))
      let ctxp = Map.restrictKeys ctx pnameset
      let ctxq = Map.restrictKeys ctx qnameset
      auxP (Map.insert x g ctxp) p
      auxP (Map.insert x (Tree.remap $ Tree.dual g) ctxq) q
    auxP pname (Cast x t p) = do
      (env, loop) <- auxP pname p
      when loop $ throw $ ErrorCastUnbounded (fromJust pname) x
      (env, s) <- extract env x
      checkSub x t s
      return (Map.insert x t env, loop)

    auxP pname _ = undefined
-}
