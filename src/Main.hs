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

-- |This module parses the command-line arguments and invokes the type checker.
module Main (main) where

import qualified Relation
import qualified Resolver
import qualified Checker
import Render
import Exceptions (MyException)
import Parser (parseProcess)
import Process (ProcessDef)
import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception (catch)
import qualified Data.Version
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName)

-- |Version of the program.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 1]

-- |Entry point.
main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  let result = parseProcess file source
  case result of
    Left msg -> printWarning msg
    Right (tdefs, pdefs) ->
      let pdefs' = Resolver.resolve tdefs pdefs in
        catch (check file args pdefs') (handler args)
  where
    check :: FilePath -> [Flag] -> [ProcessDef] -> IO ()
    check file args pdefs = do
      let verbose  = Verbose `elem` args
      let logging  = Logging `elem` args
      let no_action = NoAction `elem` args
      let no_bounds = NoBounds `elem` args
      let no_checks = NoChecks `elem` args
      let unfair = Unfair `elem` args
      when logging
        (do putStr $ takeFileName file ++ " ... "
            hFlush stdout)
      start <- getCurrentTime
      let subt = if unfair then Relation.subtype else Relation.fairSubtype
      unless no_action $ Checker.checkActionBoundedness pdefs
      unless no_bounds $ Checker.checkRanks pdefs
      unless no_checks $ Checker.checkTypes subt pdefs
      stop <- getCurrentTime
      printOK (if logging then Just (show (diffUTCTime stop start)) else Nothing)
      when verbose $ forM_ pdefs (printRank pdefs)

    printRank :: [ProcessDef] -> ProcessDef -> IO ()
    printRank pdefs (pname, _, Just p) = putStrLn $ "process " ++ show pname ++ " has rank " ++ show (Checker.rank pdefs p)
    printRank pdefs (pname, _, Nothing) = return ()

    handler :: [Flag] -> MyException -> IO ()
    handler _ e = printNO (show e)

-- |Representation of supported flags.
data Flag = Verbose  -- -v --verbose
          | Version  -- -V --version
          | Logging  --    --log
          | Help     --    --help
          | NoAction -- -a
          | NoBounds -- -b
          | NoChecks -- -c
          | Unfair   -- -u
            deriving (Eq, Ord)

-- |List of supported flags.
flags :: [OptDescr Flag]
flags =
   [ Option []  ["log"]      (NoArg Logging)     "Log type checking time"
   , Option "a" []           (NoArg NoAction)    "Disable action boundedness checking"
   , Option "b" []           (NoArg NoBounds)    "Disable session and cast boundedness checking"
   , Option "c" []           (NoArg NoChecks)    "Disable session type checking"
   , Option "u" []           (NoArg Unfair)      "Use unfair subtyping"
   , Option "v" ["verbose"]  (NoArg Verbose)     "Print type checking activities"
   , Option "V" ["version"]  (NoArg Version)     "Print version information"
   , Option "h" ["help"]     (NoArg Help)        "Print this help message" ]

-- |The information displayed when the verbose option is specified.
versionInfo :: String -> String
versionInfo progName =
  "FairCheck " ++ Data.Version.showVersion version ++ " Copyright © 2021 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

-- |Parse command-line arguments.
parse :: String -> [String] -> IO ([Flag], String)
parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
