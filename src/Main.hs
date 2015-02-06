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

read2 x = read x :: Double
read3 x = read x :: Int

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
   (ReqArg CreateGrid "List")
   optionLowCHelp
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
         


optionLowCHelp = printVerbatim [verbatim|
This option creates a grid file and a corrector file 
for Molcas program GRID_IT. It must be launched as:
$ GriDDer -c "x y z resX resY resZ"
$ GriDDer -c "-10 -10 -10 60 60 60"
Where x,y,z are box's corner coords and res are
points along that direction|]
