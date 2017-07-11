import Data.Vect


data Direction = North
              | East
              | South
              | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

data Shape = ||| Triangle, with base length and height
            Triangle Double Double
          | ||| Rectangle, length and height
          Rectangle Double Double
          |  |||  Radius
          Circle Double

area: Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius


data Picture = Primitive Shape
              | Combine Picture Picture
              | Rotate Double Picture
              | Translate Double Double Picture


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol


wheels : Vehicle power -> Nat
wheels  Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible


append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip1 : Vect n a -> Vect n b -> Vect n (a, b)
zip1 [] ys = []
zip1 (x :: xs) (y :: ys) = (x, y) :: zip1 xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                     Nothing => Nothing
                     (Just idx) => Just (index idx xs)
