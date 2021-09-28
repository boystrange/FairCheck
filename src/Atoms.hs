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

module Atoms where

-- POSITIONS

data Pos = Somewhere
         | At (Int, Int)

instance Show Pos where
  show Somewhere = ""
  show (At (l, c)) = "@" ++ show l

-- LOCATED IDENTIFIERS

data LabelI
data TypeI
data ChannelI
data ProcessI

data Identifier k = Identifier { identifierPos :: Pos
                               , identifierText :: String }
instance Show (Identifier k) where
  show = identifierText

showWithPos :: Identifier k -> String
showWithPos u = identifierText u ++ show (identifierPos u)

instance Eq (Identifier k) where
  (==) u v = identifierText u == identifierText v

instance Ord (Identifier k) where
  compare u v = compare (identifierText u) (identifierText v)

type Label       = Identifier LabelI
type ChannelName = Identifier ChannelI
type TypeName    = Identifier TypeI
type ProcessName = Identifier ProcessI

-- POLARITIES

data Polarity = In | Out
  deriving (Eq, Ord)

dualP :: Polarity -> Polarity
dualP In = Out
dualP Out = In

instance Show Polarity where
  show In = "?"
  show Out = "!"
