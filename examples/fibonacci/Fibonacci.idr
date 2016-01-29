
-- Analytic Method Of Fibonacci
fibAnalytic : Nat -> Double
fibAnalytic n = 
    floor $ ((pow goldenRatio n) - (pow (-1.0/goldenRatio) n))  / sqrt(5)
  where goldenRatio : Double 
        goldenRatio = (1.0 + sqrt(5)) / 2.0

-- Recursive Method Of Fibonacci
fibRecursive : Nat -> Nat
fibRecursive Z = Z
fibRecursive (S Z) = (S Z)
fibRecursive (S (S n)) = fibRecursive (S n) + fibRecursive n

-- Iterative Method Of Fibonacci
fibIterative : Nat -> Nat
fibIterative n = fibIterative' n Z (S Z)
  where fibIterative' : Nat -> Nat -> Nat -> Nat
        fibIterative' Z a _ = a
        fibIterative' (S n) a b = fibIterative' n b (a + b)

-- Lazy Method Of Fibonacci
fibLazy : Lazy (List Nat)
fibLazy = 0 :: 1 :: zipWith (+) fibLazy (
              case fibLazy of
                (x::xs) => xs
                [] => []) 
