module Implikanty (Implikant, minPoKol, minPoKol', naImplikant, pokrywa) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (shiftR, xor, (.&.)))
import Data.List (transpose)
import Dzialania (combinations)

type Vector = (Int, Bool)

type Implikant = [Vector]

minPoKol :: Int -> [Int] -> [[Int]]
minPoKol bitNum blokada =
  map fst . head . filter (not . null) $
    map (filter snd . map minPok . flip combinations [0 .. bitNum - 1]) [1 .. bitNum]
  where
    minPok :: [Int] -> ([Int], Bool)
    minPok x = (x, foldl (\acc x -> acc && x /= 0) True $ map (.&. sum [2 ^ temp | temp <- x]) blokada)

minPoKol' :: [[Bool]] -> [[Int]]
minPoKol' blokada =
  let tblokada = transpose blokada
   in map fst . head . filter (not . null) $
        map (filter (\(_, x) -> and x) . map or' . flip combinations (zip [0 ..] tblokada)) [1 ..]
  where
    or' [(xi, xn)] = ([xi], xn)
    or' ((xi, xn) : xs) = bimap (xi :) (xn .||) (or' xs)
      where
        (.||) [] [] = []
        (.||) (x : xs) (y : ys) = (x || y) : xs .|| ys
        (.||) _ _ = []

naImplikant :: Int -> [Int] -> Implikant
naImplikant kostka = map (\x -> (x, (\y -> toEnum y :: Bool) . flip mod 2 . shiftR kostka $ x))

pokrywa :: Int -> Implikant -> Bool
pokrywa wektor = foldl (\acc (index, znak) -> acc && (getBit wektor index == znak)) True
  where
    getBit :: Int -> Int -> Bool
    getBit wektor = (\x -> toEnum x :: Bool) . flip mod 2 . shiftR wektor
