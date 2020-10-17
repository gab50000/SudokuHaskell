empty_sudoku = take 9 $ repeat $ take 9 $ repeat 0

StrToSudoku :: String -> [Int]
StrToSudoku str = map read str

isCompleted :: [[Int]] -> Bool
isCompleted (row : rest) = isCompletedRow row && isCompleted rest

isCompletedRow :: [Int] -> Bool
isCompletedRow (x : xs) = x /= 0 && isCompletedRow xs

addElem :: [[Int]] -> Maybe [[Int]]
addElem (row : rest)
  | isCompleted (row : rest) = Just (row : rest)
  | isCompletedRow row = Just row : Maybe (addElem num rest)

addElemToRow :: Int -> [Int] -> [Int]
addElemToRow num (x : xs)
  | isCompletedRow (x : xs) = (x : xs)
  | x == 0 = (1 : xs)

main = do
  print $ isCompleted empty_sudoku