module CombineCube where
import Data.List.Split

import DataTypes
import Functions
import Verbatim
import VerbatimParser


makeDifference :: FilePath -> FilePath -> IO ()
makeDifference a b = do
      s0 <- readCube a
      s1 <- readCube b
      let difference = zipWith (-) (getGrid s1) (getGrid s0) 
          cube       = Grid (getHead s0) (getMatDim s0) difference
          aa         = trimExtension a
          bb         = trimExtension b
      writeCube (aa ++ bb ++ "Difference.cube")cube

readCube :: FilePath -> IO (Grid)
readCube fn = do
    contents <- readFile fn
    let splitdContent = lines contents
        (header,takeHeaderOut) = splitAt 2 splitdContent
        atomnumber    = read (head $ head $ map words takeHeaderOut) :: Int
        (restOfHeader,restOfFile) = splitAt (atomnumber + 4) takeHeaderOut
        floats        = concat $ map (map (\x -> read x :: Double)) $ map words restOfFile
    return $ Grid header restOfHeader floats

writeCube :: FilePath -> Grid -> IO ()
writeCube fn grid = do
           let formHeader = unlines (getHead grid) ++ unlines (getMatDim grid)
               formFloats = unlines $ map unwords $ chunksOf 6 (map show (getGrid grid))
               cubeOut = formHeader ++ formFloats
           writeFile fn cubeOut


