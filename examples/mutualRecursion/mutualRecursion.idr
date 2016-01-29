-- Defines Hofstadter Female and Male sequences
mutual {

  F : Nat -> Nat
  F Z = (S Z)
  F (S n) = (S n) `minus` M(F(n))

  M : Nat -> Nat 
  M Z = Z
  M (S n) = (S n) `minus` F(M(n))
}


