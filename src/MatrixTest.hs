module MatrixTest where

import Data.Function    ((&))
import Foreign.Storable (Storable)

import           Data.Map              (Map)
import qualified Data.Map              as M
import qualified Data.Vector.Storable  as V
import           Numeric.LinearAlgebra

--bla = ((12><12) [0..] :: Matrix I) #> fromList [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] :: Vector I

entropy :: (Ord a, Storable a) => Vector a -> Double
entropy vec =
    let
        n = fromIntegral $ V.length vec

        proportions =
            V.foldl counter M.empty vec
                & M.elems
                & map fromIntegral
                & map (\x -> x / n)

        entropies =
            [ -p * logBase 2 p | p <- proportions ]
    in
        sum entropies

counter :: Ord a => Map a Int -> a -> Map a Int
counter countDict el = M.insertWith (+) el 1 countDict

-- informationGain :: (Ord a, Storable a) => Vector a -> Vector a -> Double
-- informationGain = undefined
