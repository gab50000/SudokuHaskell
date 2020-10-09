empty_sudoku = take 9 $ repeat $ take 9 $ repeat 0

is_completed :: [[Int]] -> Bool
is_completed (row : rest) = is_completed_row row && is_completed rest

is_completed_row :: [Int] -> Bool
is_completed_row (x : xs) = x /= 0 && is_completed_row xs

addElem :: Int -> [[Int]] -> [[Int]]
addElem num sudoku
  | is_completed sudoku = sudoku
  | otherwise = [[100]]

main = do
  print $ is_completed empty_sudoku