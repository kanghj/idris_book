
expandLists : (f : List a -> List a) -> (xs: List a) -> (ys : List a) -> xs = ys -> f xs = f ys
expandLists f xs ys prf = cong prf

cons : (thing : a) -> List a -> List a
cons thing xs = thing :: xs

sameConsWithX : (x: a) -> {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
sameConsWithX x {xs} {ys} prf = expandLists (cons x) xs ys prf


same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons {xs} {ys} prf = ?hole --expandLists (cons) xs ys prf


same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists prf prf1 = ?cong11

