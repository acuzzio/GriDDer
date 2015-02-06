module DataTypes where

data Flag = Help
          | CreateGrid String
          deriving (Show, Eq)

