module Main

readToBlank : IO (List String)
readToBlank = do
                x <- getLine
                if x == "" then pure []
                           else do xs <- readToBlank
                                   pure (x :: xs)


readAndSave : IO ()
