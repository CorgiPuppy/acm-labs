module File where
writeToFile = do    
   file <- openFile "test.txt" WriteMode
   hPutStrLn file "Random"
   hClose file
readFromFile = do
   file2 <- openFile "test.txt" ReadMode
   contents <- hGetContents file2
   putStr contents
   hClose file2