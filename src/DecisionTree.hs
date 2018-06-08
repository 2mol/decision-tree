module DecisionTree where

import Data.Function    ((&))
-- import Foreign.Storable (Storable)

import           Data.Map              (Map)
import qualified Data.Map              as M
import qualified Data.Vector  as V
-- import           Numeric.LinearAlgebra

{-
let v = fromList [0, 0, 1, 0, 0, 1, 0, 0, 2, 2, 0, 2] :: Vector I

entropy v
-}

entropy :: Ord a => V.Vector a -> Double
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

count :: Ord a => V.Vector a -> Map a Int
count vec = V.foldl countHelper M.empty vec

{-
let v1 = fromList [0, 0, 1, 0, 0, 1, 0, 0, 2, 2, 0, 2] :: Vector I
let v2 = fromList [1, 2, 3, 4, 5, 5, 3, 5, 6, 8, 9, 5] :: Vector I

informationGain v1 v2
-}

informationGain :: Ord a => V.Vector a -> V.Vector a -> Double
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

groupByVector :: Ord a => V.Vector a -> V.Vector b -> Map a (V.Vector b)
groupByVector groupingVector targetVector =
    let
        groupedIndices =
            indicesMap groupingVector

        getAtIndices vec indices =
            V.ifilter (\i _ -> elem i indices) vec
    in
        groupedIndices
            & M.map (getAtIndices targetVector)

indicesMap :: Ord a => V.Vector a -> Map a [Int]
indicesMap vec =
    V.ifoldl indicesHelper M.empty vec

indicesHelper :: Ord a => Map a [Int] -> Int -> a -> Map a [Int]
indicesHelper indicesDict i el = M.insertWith (++) el [i] indicesDict
