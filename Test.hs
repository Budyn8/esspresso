import Data.Bits (Bits (shiftL, (.&.), (.|.)))
import Data.List (nub)
import GHC.Integer (orInteger)
import Implikanty (Implikant)
import Minimizer
import System.Random (StdGen, newStdGen, randomRs)

main :: IO ()
main = do
  g <- newStdGen
  let numOf = 3
      (onSet, offSet) = generateValidExample g numOf
      rozwiazania = wszystRozw numOf offSet onSet
  print (onSet, offSet)
  print rozwiazania
  print $ all (\x -> rozwDob x True onSet && rozwDob x False offSet) rozwiazania

rozwDob :: [Implikant] -> Bool -> [Int] -> Bool
rozwDob rozwiazanie = checkIfGood (map implikantToInts rozwiazanie)

generateValidExample :: StdGen -> Int -> ([Int], [Int])
generateValidExample g numOf =
  let possible = 2 ^ numOf
   in splitAt (possible `div` 3) . take (possible - 3) . nub $ (randomRs (0, possible - 1) g :: [Int])

implikantToInts :: Implikant -> (Int, Int)
implikantToInts [] = (0, 0)
implikantToInts ((index, sgn) : xs) =
  let (maska, prison) = implikantToInts xs
   in (shiftL 1 index .|. maska, shiftL (fromEnum sgn :: Int) index .|. prison)

checkIfGood :: [(Int, Int)] -> Bool -> [Int] -> Bool
checkIfGood implikanty state = all (\x -> (state ==) . any (\(maska, prison) -> (maska .&. x) == (maska .&. prison)) $ implikanty)
