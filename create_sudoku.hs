import qualified Data.Set as Set

type Field = Int

type Sudoku = [[Field]]

getValidValues :: Sudoku -> (Int, Int) -> [Int] -> [Int]
getValidValues sudoku (i, j) excluded_values = Set.toList $ foldl Set.difference one_to_nine [numbers_in_row, numbers_in_col, numbers_in_box, excluded]
  where
    excluded = Set.fromList excluded_values
    one_to_nine = Set.fromList [1 .. 9]
    numbers_in_row = Set.fromList $ getRow sudoku i
    numbers_in_col = Set.fromList $ getCol sudoku j
    numbers_in_box = Set.fromList $ getBox sudoku (i, j)

getBox :: Sudoku -> (Int, Int) -> [Int]
getBox sudoku (i, j)
  | i < 0 || i > 8 || j < 0 || j > 8 = []
  | otherwise = [sudoku !! ii !! jj | ii <- row_indices, jj <- col_indices]
  where
    row_indices = [3 * (div i 3) .. 3 * ((div i 3) + 1) - 1]
    col_indices = [3 * (div j 3) .. 3 * ((div j 3) + 1) - 1]

getRow :: Sudoku -> Int -> [Int]
getRow sudoku i
  | i < 0 || i > 8 = []
  | otherwise = sudoku !! i

getCol :: Sudoku -> Int -> [Int]
getCol sudoku j
  | j < 0 || j > 8 = []
  | otherwise = [row !! j | row <- sudoku]

insertValue :: Sudoku -> (Int, Int) -> Int -> Sudoku
insertValue sudoku (i, j) k = beginning ++ [row_head ++ [k] ++ row_tail] ++ end
  where
    (row_head, _ : row_tail) = splitAt j row
    (beginning, row : end) = splitAt i sudoku

createSudoku :: Sudoku -> (Int, Int) -> [Int] -> Maybe Sudoku
createSudoku sudoku (i, j) excluded
  | i < 0 || j < 0 || i > 8 || j > 8 = Nothing
  | valid_values == [] = Nothing
  | i == 8 && j == 8 = Just $ insertValue sudoku (i, j) val
  | next_sudoku /= Nothing = next_sudoku
  | otherwise = createSudoku sudoku (i, j) (val : excluded)
  where
    next_sudoku = createSudoku (insertValue sudoku (i, j) val) (next_i, next_j) []
    (val : xs) = valid_values
    valid_values = getValidValues sudoku (i, j) excluded
    next_i = i + div (j + 1) 9
    next_j = mod (j + 1) 9

empty_sudoku :: [[Int]]
empty_sudoku = take 9 $ repeat $ take 9 $ repeat 0

printSudoku :: Maybe Sudoku -> IO ()
printSudoku Nothing = return ()
printSudoku (Just []) = return ()
printSudoku (Just (row : rest)) = print row >> printSudoku (Just rest)

main = do
  let sudoku = createSudoku empty_sudoku (0, 0) []
  printSudoku sudoku
