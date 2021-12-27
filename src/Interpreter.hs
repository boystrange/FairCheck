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

module Interpreter where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomIO)
import qualified Data.List as List
import Data.IORef
import Control.Monad (when, unless)
import Control.Exception (throw)

import Atoms
import Process
import Exceptions
import Render

-- | State of the interpreter, consisting of a pair of references
-- respectively containing the index of the next session to be
-- created and the number of reductions performed so far.
type InterpreterS = (IORef Int, IORef Int)

-- | Map from process names to bound names and process body.
type ProcessMap = Map ProcessName ([ChannelName], Process)

-- | A __thread__ is a pair consisting of a channel name and a
-- process guarded by an action whose subject is precisely that
-- channel name.
type Thread = (ChannelName, Process)

-- | A __substitution__ is a map from channel names to channel
-- names.
type Substitution = Map ChannelName ChannelName

-- | Create a new session.
newSession :: InterpreterS -> IO ChannelName
newSession (ref, _) = do
  n <- readIORef ref
  writeIORef ref (n + 1)
  return $ Identifier Somewhere ("$" ++ show n)

-- | Increments the number of performed reductions.
tick :: InterpreterS -> IO ()
tick (_, ref) = modifyIORef ref succ

-- | Perform a name substitution within a process.
subst :: Substitution -> Process -> Process
subst = auxP
  where
    auxP :: Substitution -> Process -> Process
    auxP _ Done = Done
    auxP σ (Call pname us) = Call pname (map (auxN σ) us)
    auxP σ (Wait v p) = Wait (auxN σ v) (auxP σ p)
    auxP σ (Close v) = Close (auxN σ v)
    auxP σ (Channel v In y p) = Channel (auxN σ v) In y (auxP (Map.delete y σ) p)
    auxP σ (Channel v Out w p) = Channel (auxN σ v) Out (auxN σ w) (auxP σ p)
    auxP σ (Label v pol ws gs) = Label (auxN σ v) pol ws (map (auxG σ) gs)
    auxP σ (New y t p q) = New y t (auxP σ' p) (auxP σ' q)
      where
        σ' = Map.delete y σ
    auxP σ (Choice m p n q) = Choice m (auxP σ p) n (auxP σ q)
    auxP σ (Cast u t p) = Cast (auxN σ u) t (auxP σ p)

    auxN :: Substitution -> ChannelName -> ChannelName
    auxN σ v | Just u <- Map.lookup v σ = u
             | otherwise = v

    auxG :: Substitution -> (Label, Process) -> (Label, Process)
    auxG σ (l, p) = (l, auxP σ p)

runtimeError :: String -> a
runtimeError = throw . ErrorRuntime

-- | Flattens a process into two lists of threads, expanding process
-- invocations, creating new sessions, performing choices and
-- discarding casts.
threads :: Bool -> ProcessMap -> InterpreterS -> Process -> IO [Process]
threads logging pmap state = aux
  where
    aux Done = return []
    aux (Call pname us) = do
      case Map.lookup pname pmap of
        Nothing -> runtimeError $ "undefined process " ++ show pname
        Just (xs, p) -> do
          unless (length us == length xs) $ runtimeError $ "wrong number of arguments when invoking " ++ show pname
          let σ = Map.fromList (zip xs us)
          aux (subst σ p)
    aux (New x t p q) = do
      u <- newSession state
      when logging $ printWarning $ "=> creating new session " ++ show u
      let σ = Map.fromList [(x, u)]
      ps <- aux (subst σ p)
      qs <- aux (subst σ q)
      return (ps ++ qs)
    aux (Choice m p n q) = do
      i <- randomInt (m + n)
      tick state
      when logging $ printWarning $ "=> performing an internal choice " ++ show (i < m)
      if i < m then aux p else aux q
    aux (Cast _ _ p) = aux p
    aux p = return [p]

-- | Generate a random integer.
randomInt :: Int -> IO Int
randomInt n = (`mod` n) <$> randomIO

-- | Given a list ws of weights and a weight n picks a random index
-- with probability that is proportional to the weight at that
-- index.
pick :: Int -> [Int] -> Int
pick m (n : _) | m < n = 0
pick m (n : ns) = 1 + pick (m - n) ns

-- | Performs all the reductions from a list of processes guarded by
-- actions with the same subject.
reduce :: Bool -> InterpreterS -> Process -> Process -> IO (Either Process (Process, Process))
reduce logging state = aux
  where
    aux (Close u) (Wait _ p) = do
      when logging $ printWarning $ "=> closing session " ++ show u
      tick state
      return $ Left p
    aux (Channel u Out v p) (Channel _ In x q) = do
      when logging $ printWarning $ "=> delegation of " ++ show v ++ " on " ++ show u
      tick state
      let σ = Map.fromList [(x, v)]
      return $ Right (p, subst σ q)
    aux (Label u Out ws gs) (Label _ In _ fs) = do
      n <- randomInt (sum ws)
      let (tag, p) = gs!!pick n ws
      case lookup tag fs of
        Nothing -> runtimeError $ "communication error when sending " ++ show tag
        Just q -> do
          when logging $ printWarning $ "=> label " ++ show tag ++ " on " ++ show u
          tick state
          return $ Right (p, q)
    aux _ _ = runtimeError "protocol violation"

-- | Perform all possible reductions of a given list of threads.
reduceAll :: Bool -> InterpreterS -> [Process] -> IO [Process]
reduceAll logging state ps = do
  let xs = Map.elems $ Map.fromListWith pair [ (subject p, Left p) | p <- ps ]
  when (not (null xs) && all isLeft xs) $ runtimeError "deadlock"
  qs <- mapM tryReduce xs
  return $ concat $ map unpair qs
  where
    pair (Left p) (Left q) | Just (_, In) <- prefix p
                           , Just (_, Out) <- prefix q = Right (q, p)
    pair (Left p) (Left q) | Just (_, Out) <- prefix p
                           , Just (_, In) <- prefix q = Right (p, q)
    pair _ _ = runtimeError $ "linearity violation"

    isLeft (Left _) = True
    isLeft (Right _) = False

    unpair (Left p) = [p]
    unpair (Right (p, q)) = [p, q]

    tryReduce (Left p) = return $ Left p
    tryReduce (Right (p, q)) = reduce logging state p q

-- | Run a process.
run :: Bool -> [ProcessDef] -> Process -> IO ()
run logging pdefs p = do
  ref1 <- newIORef 0
  ref2 <- newIORef 0
  aux (ref1, ref2) [p]
  ns <- readIORef ref1
  nr <- readIORef ref2
  putStrLn $ show ns ++ " sessions, " ++ show nr ++ " reductions"
  where
    aux :: InterpreterS -> [Process] -> IO [Process]
    aux _ [] = return []
    aux state ps = do
      ts <- concat <$> mapM (threads logging pmap state) ps
      qs <- reduceAll logging state ts
      aux state qs

    pmap :: ProcessMap
    pmap = Map.fromList [ (pname, (map fst xs, p)) | (pname, xs, Just p) <- pdefs ]
