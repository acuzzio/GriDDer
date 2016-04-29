module DataTypes where

data Flag = Help
          | CreateGrid String
          | Diff       String
          | DiffDx     String
          | Weight     String
          deriving (Show, Eq)

data Grid = Grid {
          getHead   :: [String],
          getMatDim :: [String],
          getGrid   :: [Double]
          } deriving Show

data Dx = Dx {
          getDxHead   :: [String],
          getDxGrid   :: [Double],
          getDxFoot   :: [String]
          } deriving Show
