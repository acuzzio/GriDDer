module CombineCube where

import Control.Parallel.Strategies
import Data.List.Split
import Text.Printf

import CreateGrid
import DataTypes
import Functions
import Verbatim
import VerbatimParser

weightCubeByDistance :: FilePath -> Int -> IO()
weightCubeByDistance file atomN = do
  s0 <- readCube file
  let grid      = getGrid s0
      matDim    = getMatDim s0
      gridCoord = generateGrid2 matDim
      rightLab  = atomN + 3 -- 4 lines above, but lists start from 0 (-1). That is +3
      [x,y,z]   = map read2 $ tail2 $ words $ matDim!!rightLab
      weighted  = weightAGridOverACoord grid gridCoord [x,y,z]
      outCube   = Grid (getHead s0) (getMatDim s0) weighted
      outName   = (trimExtension file) ++ "WeightedToAtom" ++ show atomN ++ ".cube"
  putStrLn $ "Weighting with respect to :" ++ (show $ fmap (*bohr2Ang) [x,y,z])
  writeCube outName outCube

weightAGridOverACoord :: [Double] -> [[Double]] -> [Double] -> [Double]
weightAGridOverACoord grid gridCoord atom = let
  coefficients = parMap rdeepseq (oneOverDistSquared atom) gridCoord
  in parZipWith rdeepseq (*) grid coefficients

oneOverDistSquared :: [Double] -> [Double] -> Double
oneOverDistSquared xs ys = let 
  denom = sum $ map (\x -> x ** 2 ) $ zipWith (-) ys (fmap (*(-1)) xs) 
  in 1.0 / denom

generateGrid2 matDim = let
      i  = take 4 $ map words matDim
      [xPoints,yPoints,zPoints] = map read3 [i!!1!!0,i!!2!!0,i!!3!!0] 
      [a,b,c] = map read2 [i!!0!!1,i!!0!!2,i!!0!!3]
      [d,e,f] = otherCorn [a,b,c]
      listX  = listar a d xPoints
      listY  = listar b e yPoints
      listZ  = listar c f zPoints
      in [[x,y,z] | x<-listX, y<-listY, z<-listZ]

makeDifference :: FilePath -> FilePath -> IO ()
makeDifference a b = do
  s0 <- readCube a
  s1 <- readCube b
  let difference = parZipWith rdeepseq (-) (getGrid s0) (getGrid s1) 
      cube       = Grid (getHead s0) (getMatDim s0) difference
      aa         = trimExtension a
      bb         = trimExtension b
  writeCube (aa ++ bb ++ "Difference.cube") cube

readCube :: FilePath -> IO (Grid)
readCube fn = do
  contents <- readFile fn
  let splitdContent = lines contents
      (header,takeHeaderOut) = splitAt 2 splitdContent
      atomnumber    = read (head $ head $ map words takeHeaderOut) :: Int
      (restOfHeader,restOfFile) = splitAt (atomnumber + 4) takeHeaderOut
      floats        = concat $ map (map (\x -> read x :: Double)) $ map words restOfFile
  return $ Grid header restOfHeader floats

fn="Sphere5.Zero.pqr.dx"

makeDxDifference :: FilePath -> FilePath -> IO ()
makeDxDifference a b = do
  s0 <- readDxGrid a
  s1 <- readDxGrid b
  let difference = parZipWith rdeepseq (-) (getDxGrid s0) (getDxGrid s1) 
      dx         = Dx (getDxHead s0) difference (getDxFoot s0)
      aa         = trimExtension a
      bb         = trimExtension b
  writeDx (aa ++ bb ++ "Difference.cube") dx

--readDxGrid :: FilePath -> IO (Dx)
readDxGrid fn = do
  contents <- readFile fn
  let splitcontent  = lines contents
      (header,rest) = splitAt 11 splitcontent
      ln            = length rest
      (grid,footer) = splitAt (ln-5) rest
      gridF         = concat $ map (map (\x -> read x :: Double)) $ map words grid
  return $ Dx header gridF footer 

writeDx :: FilePath -> Dx -> IO ()
writeDx fn dx = do
  let formHeader = unlines (getDxHead dx)
      formGrid   = unlines $ map unwords $ chunksOf 3 (map show (getDxGrid dx))
      formFooter = unlines (getDxFoot dx)
      dxOut      = formHeader ++ formGrid ++ formFooter
  writeFile fn dxOut

writeCube :: FilePath -> Grid -> IO ()
writeCube fn grid = do
  let formHeader = unlines (getHead grid) ++ unlines (getMatDim grid)
      formFloats = unlines $ map unwords $ chunksOf 6 (map show (getGrid grid))
      cubeOut = formHeader ++ formFloats
  writeFile fn cubeOut


