import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position



StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y = cast y

valToStringCase : (isInt : Bool) -> (case isInt of
                                        False => String
                                        True => Int) -> String
valToStringCase False x = trim x
valToStringCase True x = cast x
