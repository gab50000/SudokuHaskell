import qualified Data.Set as Set

type Field = Int

type Sudoku = [[Field]]

-- isValidRow :: Sudoku -> Int -> bool
-- isValidCol :: Sudoku -> Int -> bool
-- isValidBox :: Sudoku -> Int -> Int -> bool
getValidValues :: Sudoku -> (Int, Int) -> [Int]
getValidValues sudoku (i, j) = foldl Set.difference one_to_nine [numbers_in_row, numbers_in_col, numbers_in_box]
  where
    one_to_nine = Set.fromList [1 .. 9]
    numbers_in_row = Set.fromList $ getRow sudoku i
    numbers_in_col = Set.fromList $ getCol sudoku j

-- getBox :: Sudoku -> Int -> Int -> [[Int]]
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

createSudoku :: Sudoku -> (Int, Int) -> Maybe Sudoku
createSudoku _ (i, j)
  | i < 0 || j < 0 || i > 8 || j > 8 = Nothing

--   | otherwise = head [x | x <- [0 .. 8]]