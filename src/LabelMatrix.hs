module LabelMatrix where

-- import Data.Function    ((&))
-- import Foreign.Storable (Storable)

import           Data.Map              (Map)
-- import qualified Data.Map              as M
-- import qualified Data.Vector.Storable  as V
import           Numeric.LinearAlgebra

{-
Matrices with labeled rows and columns.

Construction functions handle uniqueness & correct length of labels.

Provides similar convenience functions to pandas DataFrames. Contrary to the latter, all
elements in the matrix contain only one type.
-}

data LabelMatrix a r c =
    LabelMatrix
        { matrix :: Matrix a
        , rows   :: Vector r
        , cols   :: Vector c
        } deriving (Show)

groupByRow :: LabelMatrix a r c -> r -> Map r (LabelMatrix a r c)
groupByRow = undefined

groupByColumn :: LabelMatrix a r c -> c -> Map c (LabelMatrix a r c)
groupByColumn = undefined