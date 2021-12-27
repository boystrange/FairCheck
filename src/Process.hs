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

-- |Representation of processes (Section 4).
module Process where

import Atoms
import Type (Type)
import qualified Type
import Data.Set (Set)
import qualified Data.Set as Set

-- |Representation of processes.
data Process
  -- |Terminated process.
  = Done
  -- |Process invocation.
  | Call ProcessName [ChannelName]
  -- |Receive session termination signal.
  | Wait ChannelName Process
  -- |Send session termination signal.
  | Close ChannelName
  -- |Input/output of channel.
  | Channel ChannelName Polarity ChannelName Process
  -- |Input/output of label.
  | Label ChannelName Polarity [Int] [(Label, Process)]
  -- |Session creation.
  | New ChannelName Type Process Process
  -- |Non-deterministic choice. For the sake of simplicity and differently from
  -- the paper, the annotation indicating which branch of the choice leads to
  -- termination is always assumed to be 2.
  | Choice Int Process Int Process
  -- |Use of fair subtyping.
  | Cast ChannelName Type Process

-- |Set of channel names occurring free in a process.
fn :: Process -> Set ChannelName
fn Done = Set.empty
fn (Call _ us) = Set.fromList us
fn (Wait u p) = Set.insert u (fn p)
fn (Close u) = Set.singleton u
fn (Channel u pol v p) = Set.insert u $ (if pol == Out then Set.insert else Set.delete) v (fn p)
fn (Label u _ _ gs) = Set.insert u (Set.unions (map (fn . snd) gs))
fn (New u _ p q) = Set.delete u (Set.union (fn p) (fn q))
fn (Choice _ p _ q) = Set.union (fn p) (fn q)
fn (Cast u _ p) = Set.insert u (fn p)

-- |Set of process names occurring along the termination paths of a process.
pn :: Process -> Set ProcessName
pn Done = Set.empty
pn (Call pname _) = Set.singleton pname
pn (Wait _ p) = pn p
pn (Close _) = Set.empty
pn (Channel _ _ _ p) = pn p
pn (Label _ _ _ cs) = Set.unions (map (pn . snd) cs)
pn (New _ _ p q) = Set.union (pn p) (pn q)
-- Note that the set of process names of a non-deterministic choice coincides
-- with that of its right branch. This is because we implicitly assume that the
-- right branch is the one leading to termination.
pn (Choice _ _ _ p) = pn p
pn (Cast _ _ p) = pn p

-- | A __process definition__ is a triple made of a process name, a
-- list of name declarations and an optional process body. When the
-- body is 'Nothing' the process is declared and assumed to be well
-- typed but is left unspecified.
type ProcessDef = (ProcessName, [(ChannelName, Type)], Maybe Process)
