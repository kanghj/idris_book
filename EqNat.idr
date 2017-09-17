import Data.Vect

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num

SameS : (k : Nat) -> (j : Nat) -> (x : EqNat k j) -> EqNat (S k) (S j)
SameS k k (Same k) = Same (S k)


checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                            Nothing => Nothing
                            (Just x) => Just (SameS _ _ x)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of 
                                Nothing => Nothing
                                (Just (Same len)) => Just input


checkEqNat1 : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat1 Z Z = Just Refl
checkEqNat1 Z (S k) = Nothing
checkEqNat1 (S k) Z = Nothing
checkEqNat1 (S k) (S j) = case checkEqNat1 k j of
                                Nothing => Nothing
                                Just prf => Just (cong prf)
