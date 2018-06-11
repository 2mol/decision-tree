module LabelMatrix where

-- import Data.Function    ((&))
import Foreign.Storable (Storable)

import           Data.Map              (Map)
-- import qualified Data.Map              as M
import qualified Data.Vector  as V
-- import qualified Data.Vector.Storable  as VS
import           Numeric.LinearAlgebra hiding (fromList)

{-
Matrices with labeled rows and columns.

Construction functions handle uniqueness & correct length of labels.

Provides similar convenience functions to pandas DataFrames. Contrary to the latter, all
elements in the matrix contain only one type.
-}

--bla = ((12><12) [0..] :: Matrix I) #> fromList [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] :: Vector I

data LabelMatrix a r c =
    LabelMatrix
        { matrix :: Matrix a
        , rows   :: V.Vector r
        , cols   :: V.Vector c
        } deriving (Show)

fromMatrix :: Matrix a -> [r] -> [c] -> LabelMatrix a r c
fromMatrix = undefined

fromList :: Storable a => [a] -> [r] -> [c] -> LabelMatrix a r c
fromList content rowLabels columnLabels =
    let
        m = length rowLabels
        n = length columnLabels
        mat = (m><n) content
    in
        LabelMatrix mat (V.fromList rowLabels) (V.fromList columnLabels)

{-
v = m ¿ [0] -- extract column(s)

flatten v -- makes it into a vector, especially nice if the matrix is nx1 anyway.
-}

groupBy :: LabelMatrix a r c -> r -> Map a (LabelMatrix a r c)
groupBy = groupByRow

groupByRow :: LabelMatrix a r c -> r -> Map a (LabelMatrix a r c)
groupByRow = undefined

groupByColumn :: LabelMatrix a r c -> c -> Map a (LabelMatrix a r c)
groupByColumn = undefined