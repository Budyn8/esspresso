module Minimizer (heurystyczna, wszystRozw) where

import Data.Bits (Bits (xor))
import Dzialania (rmdoup)
import Implikanty

heurystyczna :: Int -> [Int] -> [Int] -> [Implikant]
heurystyczna bitNum offSet [] = []
heurystyczna bitNum offSet (x : xs) =
  let implikant' = naImplikant x . head . minPoKol bitNum $ blokada x offSet
   in implikant' : heurystyczna bitNum offSet (filter (not . flip pokrywa implikant') xs)

wszystRozw :: Int -> [Int] -> [Int] -> [[Implikant]]
wszystRozw bitNum offSet onSet =
  let implikanty' = rmdoup . concatMap (\x -> implikanty bitNum x $ blokada x offSet) $ onSet
   in map (map (implikanty' !!)) . minPoKol' $ pokryciaImpl onSet implikanty'

blokada :: Int -> [Int] -> [Int]
blokada kostka = map (`xor` kostka)

pokryciaImpl :: [Int] -> [Implikant] -> [[Bool]]
pokryciaImpl kostki implikanty' = [[pokrywa x y | y <- implikanty'] | x <- kostki]

implikanty :: Int -> Int -> [Int] -> [Implikant]
implikanty bitNum kostka blok = map (naImplikant kostka) $ minPoKol bitNum blok
