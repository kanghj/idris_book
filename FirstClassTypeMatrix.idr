import Data.Vect


Matrix : (n : Nat) -> (m : Nat) -> Type -> Type
Matrix n m a = Vect n (Vect m a)

testMatrix : Matrix 2 3 Double
testMatrix = [[0, 0, 0], [0, 0, 0]]
