import Text.Printf

corner    = [-10.5,-7,-7]
otherCorn = fmap (*(-1)) corner
resolution = 35

main = do
       let a = generateGrid otherCorn corner
           n = length a
       putStrLn $ show n
       putStrLn $ unlines $ concat a

generateGrid :: [Double] -> [Double] -> [[String]]
generateGrid (a:b:c:[]) (d:e:f:[]) = let
         p      = printf "%.5f"
         listX  = map p $ listar a d 60
         listY  = map p $ listar b e 40
         listZ  = map p $ listar c f 40
         in [[x ++ " " ++ y ++ " " ++ z] | x<-listX, y<-listY, z<-listZ]

listar :: Double -> Double -> Int -> [Double]
listar max min res = let 
                    deno = fromIntegral res 
                    step  = (max-min)/(deno-1.0)
                    in take (res) $ iterate (+step) min
