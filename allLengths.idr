import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs


xor: Bool -> Bool -> Bool
xor False y = y
xor True y = not y


mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k
  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k


fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]
sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]
tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts
