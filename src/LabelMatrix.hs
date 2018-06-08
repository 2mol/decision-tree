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

groupBy :: LabelMatrix a r c -> r -> Map a (LabelMatrix a r c)
groupBy = undefined

groupByRow :: LabelMatrix a r c -> r -> Map a (LabelMatrix a r c)
groupByRow = undefined

-- groupByVector :: (Ord a, Storable a, Storable b) => Vector a -> Vector b -> Map a (Vector b)
-- groupByVector groupingVector targetVector =
--     let
--         groupedIndices =
--             indicesMap groupingVector

--         getAtIndices vec indices =
--             V.ifilter (\i _ -> elem i indices) vec
--     in
--         groupedIndices
--             & M.map (getAtIndices targetVector)

groupByColumn :: LabelMatrix a r c -> c -> Map a (LabelMatrix a r c)
groupByColumn = undefined