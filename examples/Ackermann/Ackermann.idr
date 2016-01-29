-- Ackermann Function
A : Nat -> Nat -> Nat
A Z n = S n
A (S m) Z = A m (S Z)
A (S m) (S n) = A m (A (S m) n)

main : IO()
main = do
  print $ A 0 0
  putStrLn "" 
  print $ A 3 4
  putStrLn ""
