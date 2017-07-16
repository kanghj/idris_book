sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs x y = let cy = cast y in
                  if cy < 0
                    then Nothing
                    else let newState = x + cy in
                      Just ("Subtotal: " ++ show newState ++ "\n", newState)




main : IO ()
main = replWith 0 "Value: " sumInputs
