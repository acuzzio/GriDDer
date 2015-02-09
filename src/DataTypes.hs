module DataTypes where

data Flag = Help
          | CreateGrid String
          | Diff       String
          | Weight     String
          deriving (Show, Eq)

data Grid = Grid {
          getHead   :: [String],
          getMatDim :: [String],
          getGrid   :: [Double]
          } deriving Show
