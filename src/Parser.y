{
{-# OPTIONS -w #-}
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

-- |This module implements the parser for FairCheck scripts.
module Parser (parseProcess) where

import Lexer
import Atoms
import Type (Type)
import qualified Type
import Process
import Render

import Data.Either (partitionEithers)
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  TYPE      { Token _ TokenType }
  REC       { Token _ TokenRec }
  DONE      { Token _ TokenDone }
  WAIT      { Token _ TokenWait }
  CLOSE     { Token _ TokenClose }
  END       { Token _ TokenEnd }
  NEW       { Token _ TokenNew }
  IN        { Token _ TokenIn }
  CID       { $$@(Token _ (TokenCID _)) }
  LID       { $$@(Token _ (TokenLID _)) }
  INT       { $$@(Token _ (TokenINT _)) }
  '='       { Token _ TokenEQ }
  '.'       { Token _ TokenDot }
  ':'       { Token _ TokenColon }
  ';'       { Token _ TokenSemiColon }
  ','       { Token _ TokenComma }
  '('       { Token _ TokenLParen }
  ')'       { Token _ TokenRParen }
  '{'       { Token _ TokenLBrace }
  '}'       { Token _ TokenRBrace }
  '⌈'       { Token _ TokenLCeil }
  '⌉'       { Token _ TokenRCeil }
  '⟨'       { Token _ TokenLAngle }
  '⟩'       { Token _ TokenRAngle }
  '+'       { Token _ TokenPlus }
  '⊕'       { Token _ TokenOPlus }
  '?'       { Token _ TokenQMark }
  '!'       { Token _ TokenEMark }

%nonassoc '}' ']' IN ELSE

%right ','
%left '+' '⊕'
%nonassoc '⌉'
%left '.'

%%

-- PROGRAMS

Program
  : TypeDefList ProcessDefList { ($1, $2) }

TypeDefList
  : { [] }
  | TypeDef TypeDefList { $1 : $2 }

TypeDef
  : TYPE TypeName '=' Type { ($2, $4) }

ProcessDefList
  : { [] }
  | ProcessDef ProcessDefList { $1 : $2 }

ProcessDef
  : ProcessName Parameters '=' Process { ($1, $2, Just $4) }
  | ProcessName Parameters ';' { ($1, $2, Nothing) }

Parameters
  : { [] }
  | '(' ParameterList ')' { $2 }

ParameterList
  : { [] }
  | ParameterNeList { $1 }

ParameterNeList
  : Parameter { [$1] }
  | ParameterNeList ',' ParameterNeList { $1 ++ $3 }

Parameter
  : ChannelName ':' Type { ($1, $3) }

-- PROCESSES

Process
  : DONE { Done }
  | '(' Process ')' { $2 }
  | CLOSE ChannelName { Close $2 }
  | WAIT ChannelName '.' Process { Wait $2 $4 }
  | ChannelName Polarity '(' ChannelName ')' '.' Process { Channel $1 $2 $4 $7 }
  | ChannelName Polarity Label '.' Process { Label $1 $2 [($3, $5)] }
  | ChannelName Polarity Cases { Label $1 $2 $3 }
  | NEW '(' ChannelName ':' Type ')' Process IN Process { New $3 $5 $7 $9 }
  | Process '⊕' Process { Choice $1 $3 }
  | '⌈' ChannelName ':' Type '⌉' Process { Cast $2 $4 $6 }
  | ProcessName Names { Call $1 $2 }

Names
  : { [] }
  | '⟨' '⟩' { [] }
  | '⟨' NameNeList '⟩' { $2 }

NameNeList
  : ChannelName { [$1] }
  | ChannelName ',' NameNeList { $1 : $3 }

Cases
  : '{' CaseNeList '}' { $2 }

CaseNeList
  : Case { [$1] }
  | Case ',' CaseNeList { $1 : $3 }

Case
  : Label ':' Process { ($1, $3) }

-- IDENTIFIERS

ChannelName
  : LID { Identifier (At $ getPos $1) (getId $1) :: ChannelName }

TypeName
  : CID { Identifier (At $ getPos $1) (getId $1) :: TypeName }

ProcessName
  : CID { Identifier (At $ getPos $1) (getId $1) :: ProcessName }

Label
  : LID { Identifier (At $ getPos $1) (getId $1) :: Label }
  | INT { Identifier (At $ getPos $1) (getId $1) :: Label }

-- TYPES

Type
  : Polarity END { Type.End $1 }
  | Polarity Type '.' Type { Type.Channel $1 $2 $4 }
  | Polarity '{' BranchNeList '}' { Type.Label $1 $3 }
  | Polarity Label '.' Type { Type.Label $1 [($2, $4)] }
  | REC TypeName '.' Type { Type.Rec $2 $4 }
  | TypeName { Type.Var $1 }
  | '(' Type ')' { $2 }
  | Type '+' Type { external $1 $3 }
  | Type '⊕' Type { internal $1 $3 }

BranchNeList
  : Branch { [$1] }
  | Branch ',' BranchNeList { $1 : $3 }

Branch
  : Label ':' Type { ($1, $3) }

Polarity
  : '?' { In }
  | '!' { Out }

{
external :: Type -> Type -> Type
external (Type.Label In bs1) (Type.Label In bs2) = Type.Label In (bs1 ++ bs2)
external t s = error $ "cannot combine external choice " ++ show t ++ " and " ++ show s

internal :: Type -> Type -> Type
internal (Type.Label Out bs1) (Type.Label Out bs2) = Type.Label Out (bs1 ++ bs2)
internal t s = error $ "cannot combine internal choice " ++ show t ++ " and " ++ show s

getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x
getId (Token _ (TokenINT x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([Type.TypeDef], [ProcessDef])
parseProcess = runAlex' parse
}
