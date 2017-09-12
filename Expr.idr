
data Expr num = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

Show ty => Show (Expr ty) where
  show x = matchOp x
      where matchOp : (Expr ty) -> String
            matchOp (Val y) = show y
            matchOp (Add y z) = "(" ++ show y ++ " + " ++ show z ++ ")"
            matchOp (Sub y z) = "(" ++ show y ++ " - " ++ show z ++ ")"
            matchOp (Mul y z) = "(" ++ show y ++ " * " ++ show z ++ ")"
            matchOp (Div y z) = "(" ++ show y ++ " / " ++ show z ++ ")"
            matchOp (Abs y) = "|" ++ show y ++ "|"

  showPrec = ?holeeee


(Neg num, Integral num, Eq num) => Eq (Expr num) where
  (==) a b = eval (a) == eval (b)

(Neg num, Integral num) => Cast (Expr num) num where
  cast from to = case (eval from) of
                   case_val => ?holeeeeee
