import System.ShQQ
import Data.List
import Data.List.Split
import Control.Monad
import Control.Concurrent.Async

data Grid = Grid {
          getHead   :: [String],
          getMatDim :: [String],
          getGrid   :: [Double]
          } deriving Show

type Population = Double

main = do main4

-- correct the stupid format of molcas CUBE into something that makes sense
main4 = do
        cubesS <- readShell "ls *.cube"
        let cubeN = lines cubesS
        zipWithM_ correctCube cubeN [1..]

correctCube :: FilePath -> Int -> IO()
correctCube cube x = do
                   grid <- readCube cube
                   let cosaquecambio = getMatDim grid
                       megusta=["   17  -11.000000   -7.000000   -5.000000","   67    0.333333    0.000000    0.000000","   43    0.000000    0.333333    0.000000","   31    0.000000    0.000000    0.333333"]
                       coordPart = tail ( tail ( tail  (tail cosaquecambio)))
                       swapped   = map (unwords . swapThings . words) coordPart
                       newMatDim = megusta ++ swapped
                       newCube   = Grid (getHead grid) newMatDim (getGrid grid)
                   writeGrid ("out" ++ (printRightDigits x 3) ++ ".cube") newCube 

swapThings :: [String] -> [String]
swapThings (atype:zeros:z:y:x:[]) = [atype,zeros,x,y,z]


-- Parallel code
-- compile with :    ghc -O2 combineCube.hs -threaded
-- launch with  :    ./combineCube +RTS -s -N6          <- 6 processors
launchJob :: (FilePath,FilePath) -> Int -> IO(Async ())
launchJob (a,b) c = do
               async $ makeDifference (a,b) c
              
main99 = do
        cubesS <- readShell "ls out*.cube"
        let cubeN = lines cubesS
            tuplas= zip cubeN (tail cubeN)
        aS <- zipWithM launchJob tuplas [1..]
        mapM_ wait aS

-- from a list of labeled cubes, this calculates the derivative
main2 = do
        cubesS <- readShell "ls out*.cube"
        let cubeN = lines cubesS
            tuplas= zip cubeN (tail cubeN)
        zipWithM_ makeDifference tuplas [1..]

-- from a list of cubes and a file of population (of 2 roots) this main write the weighted cubes.
main3 = do
       pop <- readFile "populations" 
       cubesS0S <- readShell "ls Szero*"
       cubesS1S <- readShell "ls Sunoo*"
       let cubesS0N = lines cubesS0S
           cubesS1N = lines cubesS1S
           popTrans = transpose $ map words $ lines pop
           tuplas   =  zip4 cubesS0N (popTrans!!0) cubesS1N (popTrans!!1)
       zipWithM_ weightNewtonStep2roots tuplas [1..]

makeDifference :: (FilePath, FilePath) -> Int -> IO ()
makeDifference (a,b) filenumber = do
      s0 <- readCube a
      s1 <- readCube b
      let difference = zipWith (-) (getGrid s1) (getGrid s0)
          cube       = Grid (getHead s0) (getMatDim s0) difference
      writeGrid ("derOut" ++ (printRightDigits filenumber 3) ++ ".cube")cube

-- from a tupla of states and wight, this gives out a weighted cube
weightNewtonStep2roots  :: (FilePath, String, FilePath, String) -> Int -> IO ()
weightNewtonStep2roots (a,b,c,d) filenumber = do
      s0 <- readCube a
      s1 <- readCube c
      let weightS0 = read b :: Double
          weightS1 = read d :: Double
          a        = weightOrbital [s0,s1] [weightS0,weightS1]
      writeGrid ("out" ++ (printRightDigits filenumber 3) ++ ".cube") a

weightGrid :: [Grid] -> [[Population]] -> [Grid]
weightGrid grids popu = zipWith weightOrbital (repeat grids) popu

weightOrbital :: [Grid] -> [Population] -> Grid
weightOrbital grids popu = let weighted    = zipWith (\x y ->map (*x) (getGrid y)) popu grids
                               sumWeighted = map sum $ transpose weighted 
                          in Grid (getHead $ grids!!0) (getMatDim $grids!!0) (sumWeighted)

readCube :: FilePath -> IO (Grid)
readCube fn = do
    contents <- readFile fn
    let splitdContent = lines contents
        (header,takeHeaderOut) = splitAt 2 splitdContent
        atomnumber    = read (head $ head $ map words takeHeaderOut) :: Int
        (restOfHeader,restOfFile) = splitAt (atomnumber + 4) takeHeaderOut
        floats        = concat $ map (map (\x -> read x :: Double)) $ map words restOfFile
    return $ Grid header restOfHeader floats

writeGrid :: FilePath -> Grid -> IO ()
writeGrid fn grid = do
           let formHeader = unlines (getHead grid) ++ unlines (getMatDim grid) 
               formFloats = unlines $ map unwords $ chunksOf 6 (map show (getGrid grid))
               cubeOut = formHeader ++ formFloats
           writeFile fn cubeOut

printRightDigits :: Int -> Int -> String
printRightDigits n nDigits =
     let
     leadingZeroes = concat $ take (nDigits - length(show n))(repeat "0")
     in leadingZeroes ++ show n



