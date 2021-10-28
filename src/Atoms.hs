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

-- |This module defines the representation of __identifiers__ and
-- __polarities__.
module Atoms where

-- |A position refers to line and column within a script. The constructor
-- 'Somewhere' builds an unknown position.
data Pos = Somewhere
         | At (Int, Int)

instance Show Pos where
  show Somewhere = ""
  show (At (l, c)) = " [line " ++ show l ++ "]"

-- |The kind of labels.
data LabelI

-- |The kind of type names.
data TypeI

-- |The kind of channel names.
data ChannelI

-- |The kind of process names.
data ProcessI

-- |The 'Identifier' data type represents the occurrence of an identifier within
-- a script. It is a phantom type whose type parameter indicates the kind of the
-- identifier.
data Identifier k = Identifier { identifierPos :: Pos
                               , identifierText :: String }
instance Show (Identifier k) where
  show = identifierText

-- |Show an identifier along with its position in the source code, if known.
showWithPos :: Identifier k -> String
showWithPos u = identifierText u ++ show (identifierPos u)

-- |Two identifiers are the same regardless of the position in which they occur.
instance Eq (Identifier k) where
  (==) u v = identifierText u == identifierText v

-- |Two identifiers are ordered regardless of the position in which they occur.
instance Ord (Identifier k) where
  compare u v = compare (identifierText u) (identifierText v)

-- |The type of labels.
type Label       = Identifier LabelI

-- |The type of channel names.
type ChannelName = Identifier ChannelI

-- |The type of type names.
type TypeName    = Identifier TypeI

-- |The type of process names.
type ProcessName = Identifier ProcessI

-- |A polarity describes the kind of action performed on a channel.
data Polarity
  -- |Input action (question mark in the paper)
  = In
  -- |Output action (exclamation mark in the paper)
  | Out
  deriving (Eq, Ord)

-- |Compute the dual or opposite polarity.
dualP :: Polarity -> Polarity
dualP In = Out
dualP Out = In

instance Show Polarity where
  show In = "?"
  show Out = "!"
