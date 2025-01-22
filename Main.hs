import Data.List.Split (splitOn)
import Dzialania (countBits)
import Implikanty (Implikant)
import Minimizer (heurystyczna, wszystRozw)

main :: IO ()
main = do
  input <- readFile "input"
  let (onSet : offSet : _) = parseInput input
      -- onSet = [34, 32, 14, 35, 59, 2, 62, 13, 42, 23, 9, 55, 3, 51, 4, 11, 22, 10, 45, 57, 53]
      -- offSet = [40, 44, 29, 26, 5, 0, 27, 49, 38, 15, 61, 8, 39, 16, 52, 25, 20, 60, 43, 24, 18]
      bitNum = maximum [countBits x | x <- onSet ++ offSet]
  putStr "Pełne rozwiązanie:\n"
  mapM_ (putStr . parseImplikanty) $ wszystRozw bitNum offSet onSet
  putStr "heurystyczne rozwiązanie:\n"
  putStr $ parseImplikanty $ heurystyczna bitNum offSet onSet

-- parseArgs ::

parseInput :: String -> [[Int]]
parseInput input = map (map (\x -> read x :: Int) . splitOn " ") (splitOn "\n\n" input)

parseImplikanty :: [Implikant] -> String
parseImplikanty [] = "\n"
parseImplikanty [x] = concatMap (\(index, znak) -> (if znak then '\0' else '~') : 'x' : show index) x ++ "\n"
parseImplikanty (x : xs) = concatMap (\(index, znak) -> (if znak then '\0' else '~') : 'x' : show index) x ++ " + " ++ parseImplikanty xs

goodInput :: [Int] -> [Int] -> Bool
goodInput onSet = any (`elem` onSet)
