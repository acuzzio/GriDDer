{-# Language QuasiQuotes #-}
module Main where

import Control.Monad
import Data.List
import Data.List.Split
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit

import CreateGrid
import CombineCube
import DataTypes
import Functions
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
   (ReqArg CreateGrid "Box")
   optionLowCHelp,
   Option "d" ["diff"]
   (ReqArg Diff "file1,file2")
   optionLowDHelp
   ]

getExpression :: Flag -> IO ()
getExpression flag =
 case flag of
      CreateGrid st -> do
        let list = words st
        case length list of
             6 -> do 
                let [a,b,c,d,e,f] = list
                    corner = map read2 [a,b,c]
                    xPoints = read3 d
                    yPoints = read3 e
                    zPoints = read3 f
                createGrid corner xPoints yPoints zPoints 
                putStrLn "Files NEWGRID and CorrectionToGrids created."
             otherwise -> do putStrLn "ERROR, you should write 6 argouments"
      Diff st -> do
        let fileList = splitWhen (== ',') st
        case length fileList of
             2 -> do makeDifference (fileList!!0) (fileList!!1) 
             otherwise -> do putStrLn "ERROR, for option -d you should write a single argoument (no spaces) like this file1.cube,file2.cube"


optionLowCHelp = printVerbatim [verbatim|
This option creates a grid file and a corrector file 
for Molcas program GRID_IT. It must be launched as:

$ GriDDer -c "x y z resX resY resZ"

$ GriDDer -c "-10 -10 -10 60 60 60"

Where x,y,z are box's corner coords and res are
points along that direction.

|]

optionLowDHelp = printVerbatim [verbatim|
This option calculates the difference between
two grid files.

$ Gridder -d file1.cube,file2.cube

You have to use comma without space to give one
argument to the d flag. One day I'll figure out 
how to do it.
|]
