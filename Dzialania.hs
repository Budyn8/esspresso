module Dzialania (combinations, countBits, rmdoup) where

import Data.List (subsequences)

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k ==) . length) $ subsequences ns

countBits :: Int -> Int
countBits 0 = 1
countBits 1 = 1
countBits x = 1 + countBits (x `div` 2)

rmdoup :: (Eq a) => [a] -> [a]
rmdoup [] = []
rmdoup [x] = [x]
rmdoup (x : xs) = x : [k | k <- rmdoup xs, k /= x]
