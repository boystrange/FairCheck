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

-- |Expansion of session types into closed recursive terms.
module Resolver (resolve) where

import Atoms
import Exceptions
import Type
import Process
import Control.Exception (throw)

-- |Given a list of type definitions and a possibly open type, create a closed
-- type.
resolveT :: [TypeDef] -> Type -> Type
resolveT tdefs = aux []
  where
    aux tnames (End p) = End p
    aux tnames (Var tname) | tname `elem` tnames = Var tname
    aux tnames (Var tname) =
      case lookup tname tdefs of
        Nothing -> throw (ErrorUnknownIdentifier "type" (showWithPos tname))
        Just t  -> Rec tname (aux (tname : tnames) t)
    aux tnames (Rec tname t) = Rec tname (aux (tname : tnames) t)
    aux tnames (Type.Channel p t s) = Type.Channel p (aux tnames t) (aux tnames s)
    aux tnames (Type.Label p bs) = Type.Label p (map (auxB tnames) bs)

    auxB tnames (label, t) = (label, aux tnames t)

-- |Given a list of type definitions and a process, close all types occurring in
-- the process.
resolveP :: [TypeDef] -> Process -> Process
resolveP tdefs = aux
  where
    aux Done = Done
    aux (Call pname xs) = Call pname xs
    aux (Wait x p) = Wait x (aux p)
    aux (Close x) = Close x
    aux (Process.Channel x pol y p) = Process.Channel x pol y (aux p)
    aux (Process.Label x pol ws bs) = Process.Label x pol ws (map auxB bs)
    aux (New x t p q) = New x (resolveT tdefs t) (aux p) (aux q)
    aux (Choice m p n q) = Choice m (aux p) n (aux q)
    aux (Cast x t p) = Cast x (resolveT tdefs t) (aux p)

    auxB (label, p) = (label, aux p)

-- |Given a list of type definitions and a list of process definitions, close
-- all process definitions.
resolve :: [TypeDef] -> [ProcessDef] -> [ProcessDef]
resolve tdefs = map auxD
  where
    auxD :: ProcessDef -> ProcessDef
    auxD (pname, xts, mp) = (pname, map auxT xts, auxM mp)

    auxT (x, t) = (x, resolveT tdefs t)

    auxM Nothing = Nothing
    auxM (Just p) = Just $ resolveP tdefs p
