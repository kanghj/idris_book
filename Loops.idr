module Main

import System
import ReadNum

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs


countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- ReadNum.readNumber
                      | Nothing => do putStrLn "Invalid Input"
                                      countdowns
                countdown startNum
                putStr "Another (y/n)?"
                yn <- getLine
                if yn == "y" then countdowns
                             else pure ()

msgLessOrGreater : Ord a => (target : a) -> (answer : a) ->  String
msgLessOrGreater target answer = case target < answer of
                                      False => "The correct answer is greater than your guess"
                                      True => "The correct answer is less than your guess"

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr "Enter your guess! "
                          Just answer <- ReadNum.readNumber
                              | Nothing => do putStrLn "Use only numbers!"
                                              guess target guesses
                          if answer == target then do putStrLn ("Guess is correct. You took " ++ show guesses ++ " guesses")
                                                      pure ()
                                              else do putStrLn ("Try again, Guess number " ++ show guesses)
                                                      putStrLn (msgLessOrGreater target answer)
                                                      guess target (S guesses)

my_repl : String -> (String -> String) -> IO ()
my_repl prompt onInput = do
                            input <- getLine
                            putStrLn (onInput input)
                            pure ()

my_replWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
my_replWith state prompt onInput = do
                                      putStrLn prompt
                                      input <- getLine
                                      let nxt = onInput state input
                                      case nxt of
                                        Nothing => pure ()
                                        (Just (text, nextState)) => my_replWith nextState text onInput


sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs x y = let cy = cast y in
          if cy < 0
            then Nothing
            else let newState = x + cy in
              Just ("Subtotal: " ++ show newState ++ "\n", newState)




test : IO ()
test = my_replWith 0 "Value: " sumInputs


main : IO ()
main = do
        random <- time
        let small = mod random 50
        guess (cast small) 0
