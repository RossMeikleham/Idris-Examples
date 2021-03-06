
import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix m n t = Vect m (Vect n t)

multiply : Num t => Matrix m1 n t -> Matrix n m2 t -> Matrix m1 m2 t
multiply a b = multiply' a (transpose b)
  where 
        dot : Num t => Vect n t -> Vect n t -> t
        dot v1 v2 = sum $ map (\(s1, s2) => (s1 * s2)) (zip v1 v2)

        multiply' : Num t => Matrix m1 n t -> Matrix m2 n t -> Matrix m1 m2 t
        multiply' (a::as) b = map (dot a) b :: multiply' as b
        multiply' [] _ = []

