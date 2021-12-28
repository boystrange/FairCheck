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
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when, unless)
import Control.Exception (throw)
import Control.Concurrent

import Atoms
import Process
import Exceptions
import Render

-- | A session endpoint consists of a symbolic name, a boolean value
-- used to distinguish an endpoint from its peer, and a pair of
-- channels, the first one is used for receiving messages and the
-- second one for sending messages. The peer endpoint has the
-- opposite polarity and same two channels in the opposite order.
data Endpoint = Endpoint ChannelName Bool (Chan Message) (Chan Message)

instance Show Endpoint where
  show (Endpoint u pol _ _) = show u ++ if pol then "⁺" else "⁻"

-- | Representation of messages.
data Message
  -- | Termination signal.
  = CloseM
  -- | Delegation.
  | EndpointM Endpoint
  -- | Label message.
  | LabelM Label

-- | An __environment__ maps channel names to session endpoints.
type Environment = Map ChannelName Endpoint

-- | Map from process names to bound names and process body.
type ProcessMap = Map ProcessName ([ChannelName], Process)

-- | Throw a runtime error.
runtimeError :: String -> a
runtimeError = throw . ErrorRuntime

-- | Generate a random integer.
randomInt :: Int -> IO Int
randomInt n = (`mod` n) <$> randomIO

-- | Given a list ws of weights and a weight n picks a random index
-- with probability that is proportional to the weight at that
-- index.
pick :: Int -> [Int] -> Int
pick m (n : _) | m < n = 0
pick m (n : ns) = 1 + pick (m - n) ns

-- Run a process.
run :: Bool -> [ProcessDef] -> Process -> IO ()
run logging pdefs = aux Map.empty
  where
    find :: Environment -> ChannelName -> Endpoint
    find env u | Just c <- Map.lookup u env = c
               | otherwise = runtimeError $ "unbound channel name " ++ show u

    children :: MVar [MVar ()]
    children = unsafePerformIO (newMVar [])

    pmap :: ProcessMap
    pmap = Map.fromList [ (pname, (map fst xs, p)) | (pname, xs, Just p) <- pdefs ]

    newSession :: ChannelName -> IO (Endpoint, Endpoint)
    newSession u = do
      c <- newChan
      d <- newChan
      return (Endpoint u True c d, Endpoint u False d c)

    send :: Environment -> ChannelName -> Message -> IO ()
    send σ u | Endpoint _ _ _ x <- find σ u = writeChan x

    receive :: Environment -> ChannelName -> IO Message
    receive σ u | Endpoint _ _ x _ <- find σ u = readChan x

    forkChild :: IO () -> IO ThreadId
    forkChild io = do
      mvar <- newEmptyMVar
      childs <- takeMVar children
      putMVar children (mvar : childs)
      forkFinally io (\_ -> putMVar mvar ())

    waitForChildren :: IO ()
    waitForChildren = do
      cs <- takeMVar children
      case cs of
        [] -> return ()
        m : ms -> do
          putMVar children ms
          takeMVar m
          waitForChildren

    aux :: Environment -> Process -> IO ()
    aux σ Done = waitForChildren >> log "done"
    aux σ (Call pname us) =
      case Map.lookup pname pmap of
        Nothing -> runtimeError $ "undefined process " ++ show pname
        Just (xs, p) -> do
          unless (length us == length xs) $ runtimeError $ "wrong number of arguments when invoking " ++ show pname
          let σ' = foldr (uncurry Map.insert) σ (zip xs (map (find σ) us))
          aux σ' p
    aux σ (Wait u p) = do
      m <- receive σ u
      case m of
        CloseM -> do
          log $ "closed session " ++ show (find σ u)
          aux σ p
        _ -> runtimeError $ "wait: wrong message type from " ++ show u
    aux σ (Close u) = do
      log $ "closing session " ++ show (find σ u)
      send σ u CloseM
    aux σ (Channel u Out v p) = do
      log $ "sending endpoint " ++ show (find σ v) ++ " on " ++ show (find σ u)
      send σ u (EndpointM (find σ v))
      aux (Map.delete v σ) p
    aux σ (Channel u In x p) = do
      m <- receive σ u
      case m of
        EndpointM s -> do
          log $ "received endpoint " ++ show s ++ " from " ++ show (find σ u)
          aux (Map.insert x s σ) p
        _ -> runtimeError $ "endpoint input: wrong message type from " ++ show u
    aux σ (Label u Out ws gs) = do
      n <- randomInt (sum ws)
      let (tag, p) = gs!!pick n ws
      log $ "sending label " ++ show tag ++ " on " ++ show (find σ u)
      send σ u (LabelM tag)
      aux σ p
    aux σ (Label u In _ gs) = do
      m <- receive σ u
      case m of
        LabelM tag -> do
          log $ "received label " ++ show tag ++ " from " ++ show (find σ u)
          case lookup tag gs of
            Just p -> aux σ p
            Nothing -> runtimeError $ "label input: unexpected label " ++ show tag
        _ -> runtimeError $ "label input: wrong message type from " ++ show u
    aux σ (New u _ p q) = do
      (x, y) <- newSession u
      log $ "creating session " ++ show x
      forkChild (aux (Map.insert u x σ) p)
      aux (Map.insert u y σ) q
    aux σ (Choice m p n q) = do
      i <- randomInt (m + n)
      log $ "performing an internal choice " ++ show (i < m)
      aux σ (if i < m then p else q)
    aux σ (Cast _ _ p) = aux σ p

    log :: String -> IO ()
    log msg = do
      t <- myThreadId
      when logging $ printWarning $ show t ++ " " ++ msg
