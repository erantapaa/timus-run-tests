
main = do
  a <- fmap read getLine :: IO Int
  b <- fmap read getLine
  print $ a + b

