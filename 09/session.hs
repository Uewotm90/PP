main :: IO ()
main = do
  putStrLn "Hello"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello " ++ name)
  return ()

-- sequence :: [IO ()] -> IO ()
-- sequence [] = return ()
a = sequence_ [putStr "rip", putStr "rap", return ()]

-- a' = sequence_ [putStr "rip", putStr "rap", getChar] -- getChar returns a value wrapped in IO

-- gets user input, tries to read it as an integer
-- starts collatz sequence from input number
main' = do
  w <- getLine
  loop ((read w) :: Int)
  where
    loop 1 = putStr (show 1)
    loop x = do
      putStr (show x)
      if even x
        then loop (x `div` 2)
        else loop (3 * x + 1)

letter = do
  word <- getLine
  decompose word
  where
    decompose [a] = putStrLn [a]
    decompose (a : as) = do
      putStrLn [a]
      decompose as

letter' = do
  word <- getLine
  sequence_ [putStrLn [ch] | ch <- word]

-- hugorm :: IO ()
hugorm = do
  putStr "How many numbers would you like to add?"
  choice <- fmap (\x -> read x :: Int) getLine
  inputs <- sequence (replicate choice getLine)
  putStrLn ("The sum is "++show (sumof inputs))
  return ()
    where
        sumof [] = 0
        sumof (a:as) = (read a :: Int) + sumof as


--   inputs <- loop choice
--   return ()
--     where 
--         loop 0 = []
--         loop x = do
--                 input <- getLine 
--                 rest <- loop x-1
--                 input:rest