module DecisionTree where

import Data.Function    ((&))
import Foreign.Storable (Storable)

import           Data.Map              (Map)
import qualified Data.Map              as M
-- import           Data.Set              (Set)
-- import qualified Data.Set              as S
import qualified Data.Vector.Storable  as V
import           Numeric.LinearAlgebra

--bla = ((12><12) [0..] :: Matrix I) #> fromList [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] :: Vector I

entropy :: (Ord a, Storable a) => Vector a -> Double
entropy vec =
    let
        n =
            fromIntegral $ V.length vec

        proportions =
            count vec
                & M.elems
                & map fromIntegral
                & map (\x -> x / n)

        entropies =
            [ -p * logBase 2 p | p <- proportions ]
    in
        sum entropies

countHelper :: Ord a => Map a Int -> a -> Map a Int
countHelper countDict el = M.insertWith (+) el 1 countDict

count :: (Ord a, Storable a) => Vector a -> Map a Int
count vec = V.foldl countHelper M.empty vec

informationGain :: (Ord a, Storable a) => Vector a -> Vector a -> Double
informationGain featureVector resultVector =
    let
        groupedResults =
            groupByVector featureVector resultVector

        n =
            fromIntegral $ V.length featureVector

        vecLen vec = fromIntegral (V.length vec)

        weightedEntropies =
            groupedResults
                & M.elems
                & map (\v -> (vecLen v / n) * entropy v)
        in
            sum weightedEntropies

groupByVector :: (Ord a, Storable a, Storable b) => Vector a -> Vector b -> Map a (Vector b)
groupByVector groupingVector targetVector =
    let
        groupedIndices =
            indicesMap groupingVector

        getAtIndices vec indices =
            V.ifilter (\i _ -> elem i indices) vec
    in
        groupedIndices
            & M.map (getAtIndices targetVector)

indicesMap :: (Ord a, Storable a) => Vector a -> Map a [Int]
indicesMap vec =
    V.ifoldl indicesHelper M.empty vec

indicesHelper :: Ord a => Map a [Int] -> Int -> a -> Map a [Int]
indicesHelper indicesDict i el = M.insertWith (++) el [i] indicesDict
