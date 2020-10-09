empty_sudoku = take 9 $ repeat $ take 9 $ repeat 0

main = do
  print empty_sudoku