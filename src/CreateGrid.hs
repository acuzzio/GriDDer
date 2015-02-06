{-# Language QuasiQuotes #-}
module CreateGrid where

import Text.Printf

import Verbatim
import VerbatimParser

---- I need those to be called from outside
--corner    = [-10.0,-10.0,-10.0]
--xPoints = 60
--yPoints = 60
--zPoints = 60

otherCorn x = fmap (*(-1)) x

outF = "NEWGRID"
outCorrr = "CorrectionToGrids"

createGrid corner xPoints yPoints zPoints = do
       let a = generateGrid xPoints yPoints zPoints (otherCorn corner) corner
           n = length a
       writeFile outF $ show n
       putStrLn $ show n
       writeFile outF $ unlines $ concat a
       writeFile outCorrr $ liftNumbersToVerb xPoints yPoints zPoints corner (otherCorn corner)
       putStrLn $ liftNumbersToVerb xPoints yPoints zPoints corner (otherCorn corner)

liftNumbersToVerb xPoints yPoints zPoints (c1:c2:c3:[]) (c4:c5:c6:[]) = let
  pr   = printf "%.3f"
  x    = xPoints - 1
  y    = yPoints - 1
  z    = zPoints - 1
  dim1 = c4 - c1
  dim2 = c5 - c2
  dim3 = c6 - c3
  l    = liftS . show
  l2   = liftS . pr
  in printVerbatim $ correction (l x) (l y) (l z) (l2 c1) (l2 c2) (l2 c3) (l2 dim1) (l2 dim2) (l2 dim3)
  

--generateGrid :: [Double] -> [Double] -> [[String]]
generateGrid xPoints yPoints zPoints (a:b:c:[]) (d:e:f:[]) = let
         p      = printf "%.5f"
         listX  = map p $ listar a d xPoints 
         listY  = map p $ listar b e yPoints
         listZ  = map p $ listar c f zPoints
         in [[x ++ " " ++ y ++ " " ++ z] | x<-listX, y<-listY, z<-listZ]

listar :: Double -> Double -> Int -> [Double]
listar max min res = let 
                    deno = fromIntegral res 
                    step  = (max-min)/(deno-1.0)
                    in take (res) $ iterate (+step) min

correction x y z c1 c2 c3 dim1 dim2 dim3 = [verbatim|
Net=             %x          %y          %z
Origin=      %c1      %c2      %c3
Axis_1=       %dim1       0.000       0.000
Axis_2=        0.000      %dim2       0.000
Axis_3=        0.000       0.000      %dim3
|]
