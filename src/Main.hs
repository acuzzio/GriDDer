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

    ------------------------ 
   | Welcome to GriDDer !!! |
    ------------------------ 
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
   Option "C" ["createGrid2corners"]
   (ReqArg CreateGrid2 "Box")
   optionCapCHelp,
   Option "d" ["diff"]
   (ReqArg Diff "file1,file2")
   optionLowDHelp,
   Option "D" ["diffDx"]
   (ReqArg DiffDx "file1,file2")
   optionCapDHelp,
   Option "w" ["weight"]
   (ReqArg Weight "file,atomN")
   optionLowWHelp
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
      CreateGrid2 st -> do
        let list = words st
        case length list of
             9 -> do 
                let [a,b,c,d,e,f,g,h,i] = list
                    corner  = map read2 [a,b,c]
                    corner2 = map read2 [d,e,f]
                    xPoints = read3 g
                    yPoints = read3 h
                    zPoints = read3 i
                createGrid2 corner corner2 xPoints yPoints zPoints 
                putStrLn "File NEWGRID.txt created."
             otherwise -> do putStrLn "ERROR, you should write 9 argouments"
      Diff st -> do
        let fileList = splitWhen (== ',') st
        case length fileList of
             2 -> do makeDifference (fileList!!0) (fileList!!1) 
             otherwise -> do putStrLn "ERROR, for option -d you should write a single argoument (no spaces) like this file1.cube,file2.cube"
      DiffDx st -> do
        let fileList = splitWhen (== ',') st
        case length fileList of
             2 -> do makeDxDifference (fileList!!0) (fileList!!1) 
             otherwise -> do putStrLn "ERROR, for option -D you should write a single argoument (no spaces) like this file1.dx,file2.dx"
      Weight st -> do
        let fileList = splitWhen (== ',') st
        case length fileList of
             2 -> do 
                  let atomNumber = read3 (fileList!!1) 
                  weightCubeByDistance (fileList!!0) atomNumber
             otherwise -> do putStrLn "ERROR, for option -w you should write a single argoument (no spaces) like this file,atomN -> 'blabla.cube,3' "

optionLowCHelp = printVerbatim [verbatim|
This option creates a grid file (origin-symmetric)
and a corrector file for Molcas program GRID_IT. 
It must be launched as:

$ GriDDer -c "x y z resX resY resZ"

$ GriDDer -c "-10 -10 -10 60 60 60"

Where x,y,z are box's corner coords and res are
points along that direction.

|]

optionCapCHelp = printVerbatim [verbatim|
This option creates a grid file between 2 corners.
It outputs an xyz to view this box in VMD
It must be launched as:

$ GriDDer -c "x1 y1 z1 x2 y2 z2 resX resY resZ"

$ GriDDer -c "-10 -10 -10 10 10 10 60 60 60"

Where x1,y1,z1 is one corner and x2,y2,z2 is the other
res are the number of points along that direction.
|]

optionLowDHelp = printVerbatim [verbatim|
This option calculates the difference between
two grid files.

$ Gridder -d file1.cube,file2.cube

You have to give just one argument to the d flag. 
Two file names separated by a comma. I was lazy.
|]

optionCapDHelp = printVerbatim [verbatim|
This option calculates the difference between
two dx files.

$ Gridder -D file1.dx,file2.dx

You have to give just one argument to the d flag. 
Two file names separated by a comma. I was lazy.
|]

optionLowWHelp = printVerbatim [verbatim|
This option weight the grid based on an Atom 
coordinate. In this way that you get a grid that
is weighted 1/(r^2) with respect to this atom

$ Gridder -w file,3

Yeah, comma separated again... I still don't feel 
like I want to change the command line parser 
: )

|]
