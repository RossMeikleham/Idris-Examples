
-- Sums all numbers read in the given line
main : IO()
main = do
  line <- getLine
  print $ sum $ map cast $ words line
