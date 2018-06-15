module DecisionTree where

import Data.Function    ((&))
import Foreign.Storable (Storable)

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Storable       as V
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Data as N

{-
let v = fromList [0, 0, 1, 0, 0, 1, 0, 0, 2, 2, 0, 2] :: Vector I

entropy v
-}

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

        componentEntropies =
            [ -p * logBase 2 p | p <- proportions ]
    in
        sum componentEntropies

countHelper :: Ord a => Map a Int -> a -> Map a Int
countHelper countDict el = M.insertWith (+) el 1 countDict

count :: (Ord a, Storable a) => Vector a -> Map a Int
count vec = V.foldl countHelper M.empty vec

{-
let v1 = fromList [0, 0, 1, 0, 0, 1, 0, 0, 2, 2, 0, 2] :: Vector I
let v2 = fromList [1, 2, 3, 4, 5, 5, 3, 5, 6, 8, 9, 5] :: Vector I

informationGain v1 v2
-}

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


informationGains :: Matrix I -> [Double]
informationGains mat =
    let
        columns = N.toColumns mat
        featureVector = lastColumn mat
    in
        map (informationGain featureVector) columns


groupByVector :: (Storable a, Storable b, Ord a) => Vector a -> Vector b -> Map a (Vector b)
groupByVector groupingVector targetVector =
    let
        groupedIndices =
            indicesMap groupingVector

        getAtIndices vec indices =
            V.ifilter (\i _ -> elem i indices) vec
    in
        groupedIndices
            & M.map (getAtIndices targetVector)

groupBy :: (Ord a, Element a) => Matrix a -> Int -> Map a (Matrix a)
groupBy mat columnIndex =
    let
        groupingVector =
            mat ¿ [columnIndex]
                & flatten

        groupedIndices =
            indicesMap groupingVector
    in
        groupedIndices
            & M.map (\v -> removeColumn (mat ? v) columnIndex)

indicesMap :: (Ord a, Storable a) => Vector a -> Map a [Int]
indicesMap vec =
    V.ifoldl indicesHelper M.empty vec

indicesHelper :: Ord a => Map a [Int] -> Int -> a -> Map a [Int]
indicesHelper indicesDict i el =
    M.insertWith (++) el [i] indicesDict

removeColumn :: (Element a) => Matrix a -> Int -> Matrix a
removeColumn m i =
    let
        m1 = m ?? (All, Take i)
        m2 = m ?? (All, Drop (i+1))
    in
    m1 ||| m2

lastColumn :: Matrix I -> Vector I
lastColumn m =
    let (_, width) = size m
    in flatten $ m ¿ [width-1]
