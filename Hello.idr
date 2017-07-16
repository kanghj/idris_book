module Main



printLength : IO ()
printLength = putStr "Input String: " >>=
              \_ => getLine >>=
              \input => let len = length input in
                          putStrLn (show len)

printLengthDo: IO ()
printLengthDo = do
  putStr "Input String: "
  input <- getLine
  let len = length input
  putStrLn (show len)


printInput : IO ()
printInput = do
    x <- getLine
    putStrLn x



main : IO ()
main = do
  putStr "Enter your name : "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")
