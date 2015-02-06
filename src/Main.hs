{-# Language QuasiQuotes #-}
module Main where

import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit

import CreateGrid
import DataTypes
import Verbatim
import VerbatimParser



main :: IO()
main = do
  gotArgs <- getArgs
  if gotArgs == []
     then do
       useMessage
     else do
       (flags,args,_) <- return $ getOpt RequireOrder options gotArgs
       when (Help `elem` flags) $ useMessage >> exitSuccess
       if flags == []
          then putStrLn "\nI cannot understand ... Why r u overestimating me ? To get some help:\n\n$ GriDDer -h\n\n "
          else mapM_ getExpression flags

useMessage = putStrLn $ usageInfo (printVerbatim startMessage) options

startMessage = [verbatim|

   **************************
   * Welcome to GriDDer !!! *
   **************************
   |]

-- avaiable command line direct option in this program. Each option has
-- 1) "LETTER"
-- 2) ["quick help string"]
-- 3) (some options)
-- 4) "a long explanation line that appears in printout"


options :: [OptDescr Flag]
options = [
   Option "h" ["help"]
   (NoArg Help)
   "display this message",
   Option "c" ["createGrid"]
   (ReqArg CreateGrid "Input List")
   "This option creates a grid file and a corrector file for Molcas program GRID_IT"
   ]

getExpression :: Flag -> IO ()
getExpression flag =
   case flag of
       CreateGrid xs -> do
         createGrid xs

