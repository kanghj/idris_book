module Main

import Data.Vect

readToBlank : IO (List String)
readToBlank = do
                x <- getLine
                if x == "" then pure []
                           else do xs <- readToBlank
                                   pure (x :: xs)

printableList : List String -> String
printableList [] = ""
printableList (x :: xs) = x ++ "\n" ++ printableList xs


readAndSave : IO ()
readAndSave = do
                xs <- readToBlank
                fname <- getLine
                -- eitherF <- openFile fname WriteTruncate
                -- case eitherF of
                --   (Left ferr) => putStrLn "Errored"
                --   (Right f) => writeFile f

                do Right r <- writeFile fname (printableList xs) | Left ferr => putStrLn ("Errored" ++ show ferr)
                   pure ()

readLineUntilEnd : File -> IO (n ** Vect n String)
readLineUntilEnd f = do
                        isEnd <- fEOF f
                        if isEnd then
                                          pure (_ ** [])
                                     else
                                          do Right line <- fGetLine f | Left ferr => do putStrLn ("Errored" ++ show ferr)
                                                                                        pure (_ ** [])
                                             (_ ** xs) <- readLineUntilEnd f
                                             let x = trim line
                                             if x == "" then
                                               pure (_ ** xs)
                                             else
                                               pure (_ ** x :: xs)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
                          Right f <- openFile filename Read | Left ferr => do putStrLn ("Errored" ++ show ferr)
                                                                              pure (_ ** [])
                          xs <- readLineUntilEnd f
                          closeFile f
                          pure xs
