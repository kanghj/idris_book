import Data.Vect

addMatrix : Num numType =>
  Vect rows (Vect cols numType) ->
  Vect rows (Vect cols numType) ->
  Vect rows (Vect cols numType)
addMatrix xs ys = zipWith addMatrixRow xs ys where
                    addMatrixRow : (x : Vect cols numType) -> (y : Vect cols numType) -> (Vect cols numType)
                    addMatrixRow [] [] = []
                    addMatrixRow (a :: as) (b :: bs) = (a + b) :: addMatrixRow as bs

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []


transpostHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transpostHelper [] [] = []
transpostHelper (x :: xs) (y :: ys) = (x :: y) :: transpostHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                          zipWith (::) x xsTrans

multRow : Num numType => (x : Vect m numType) -> (y : Vect m numType) -> numType
multRow [] [] = 0
multRow (x :: xs) (y :: ys) = x * y + multRow xs ys


hole : Num numType => (x : Vect m numType) -> (ys : Vect len1 (Vect m numType)) -> Vect len1 numType
hole x ys = map (multRow x) ys


multMatrixRhs2 : Num numType => (xs : Vect n (Vect m numType)) -> (ysTranspose : Vect p (Vect m numType)) -> Vect n (Vect p numType)
multMatrixRhs2 [] [] = []
multMatrixRhs2 (x :: xs) [] = [] :: multMatrixRhs2 xs []
multMatrixRhs2 [] (y :: ys) = []
multMatrixRhs2 (x :: xs) (y :: ys) = (multRow x y :: (hole x ys) ) :: multMatrixRhs2 xs (y :: ys)

multMatrix : Num numType =>
  Vect n (Vect m numType) -> Vect m (Vect p numType) ->
  Vect n (Vect p numType)
multMatrix xs ys = let ysTranspose = transposeMat ys in
                   multMatrixRhs2 xs ysTranspose
