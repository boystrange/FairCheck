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

module Render
  ( printTitle
  , printWarning
  , printOK
  , printNO
  , printType )
where

import Atoms
import Prelude hiding ((<>))
import Type (Type)
import qualified Type
import Prettyprinter
import qualified Prettyprinter.Render.String as PR
import qualified Prettyprinter.Render.Terminal as PT
import qualified Data.Map as Map
import qualified Data.Set as S

-- PRETTY PRINTER COMPATIBILITY

type Document = Doc PT.AnsiStyle

keyword :: String -> Document
keyword = annotate (PT.color PT.Blue) . pretty

identifier :: String -> Document
identifier = pretty

constant :: String -> Document
constant = annotate (PT.color PT.Magenta) . pretty

operator :: String -> Document
operator = annotate PT.bold . pretty

emark :: Document
emark = operator "!"

qmark :: Document
qmark = operator "?"

dot :: Document
dot = operator "."

bar :: Document
bar = operator "|"

ampersand :: Document
ampersand = operator "&"

-- UTILITIES

embrace :: Document -> Document -> Document -> [Document] -> Document
embrace open close sep ds = align (encloseSep (open <> space) (space <> close) (sep <> space) ds)

sepembrace :: Document -> Document -> Document -> [Document] -> Document
sepembrace open close sep ds = embrace open close sep (map (<> space) (init ds) ++ [last ds])

-- LABELS

prettyLabel :: Label -> Document
prettyLabel = identifier . show

-- TYPES

prettyType :: Type -> Document
prettyType = annotate (PT.colorDull PT.Cyan) . aux
  where
    aux (Type.End pol) = operator (show pol) <> keyword "end"
    aux (Type.Var tname) = identifier (show tname)
    aux (Type.Rec tname t) = keyword "rec" <+> identifier (show tname) <> Render.dot <> aux t
    aux (Type.Channel pol s t) = operator (show pol) <> brackets (aux s) <> Render.dot <> aux t
    aux (Type.Label pol [(label, t)]) = operator (show pol) <> constant (show label) <> Render.dot <> aux t
    aux (Type.Label pol bs) = operator (show pol) <> embrace lbrace rbrace comma (map auxB bs)

    auxB (label, t) = constant (show label) <> colon <+> aux t

instance Show Type where
  show = PR.renderString . layoutPretty defaultLayoutOptions . prettyType

printType :: Type -> IO ()
printType = PT.putDoc . prettyType

-- AUXILIARY PRINTING OPERATIONS

printNewLine :: IO ()
printNewLine = putStrLn ""

printAnnotatedString :: [PT.AnsiStyle] -> String -> IO ()
printAnnotatedString anns msg = PT.putDoc (foldr annotate (pretty msg) anns)

printTitle :: String -> IO ()
printTitle msg = printAnnotatedString [PT.bold, PT.underlined] msg >> printNewLine

printWarning :: String -> IO ()
printWarning msg = printAnnotatedString [PT.color PT.Red] msg >> printNewLine

printNO :: String -> IO ()
printNO msg = do
  printAnnotatedString [PT.color PT.Red] "NO:"
  putStrLn $ " " ++ msg

printOK :: Maybe String -> IO ()
printOK msg = do
  printAnnotatedString [PT.bold, PT.color PT.Green] "OK"
  case msg of
    Nothing -> printNewLine
    Just m -> putStrLn $ " (" ++ m ++ ")"
