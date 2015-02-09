module Functions where

read2 x = read x :: Double
read3 x = read x :: Int

printRightDigits :: Int -> Int -> String
printRightDigits n nDigits =
     let
     leadingZeroes = concat $ take (nDigits - length(show n))(repeat "0")
     in leadingZeroes ++ show n 

trimExtension :: String -> String
trimExtension a = init $ reverse $ dropWhile (/= '.') $ reverse a

tail2 = tail . tail

bohr2Ang = 0.529177249
