{-
  The AKS algorithm for testing whether a number is prime is a polynomial-time algorithm based on an elementary theorem about Pascal triangles.

The theorem on which the test is based can be stated as follows:

a number p  is prime if and only if all the coefficients of the polynomial expansion of
 (x-1)^{p}-(x^{p}-1)
are divisible by p..

For example, trying p=3:

(x-1)^{3}-(x^{3}-1)=(x^{3}-3x^{2}+3x-1)-(x^{3}-1)=-3x^{2}+3x
And all the coefficients are divisible by 3 so 3 is prime.

Note:
This task is not the AKS primality test. It is an inefficient exponential time algorithm discovered in the late 1600s and used as an introductory lemma in the AKS derivation.

The task

Create a function/subroutine/method that given p generates the coefficients of the expanded polynomial representation of (x-1)^{p}.

Use the function to show here the polynomial expansions of (x-1)^{p}  for p in the range 0 to at least 7, inclusive.

Use the previous function in creating another function that when given p returns whether p is prime using the theorem.

Use your test to generate a list of all primes under 35.

As a stretch goal, generate all primes under 50 (Needs greater than 31 bit integers).

-}

import Data.Vect

-- Computes Binomial Coefficients
binCoef : Nat -> Nat -> Nat
binCoef _ Z = (S Z)
binCoef (S n) (S k) = 
    if n == k then (S Z) else ((S n) * (binCoef n k)) `div` (S k)

-- Binomial Expansion Of (x - 1)^p
expansion : (n : Nat) -> Vect (S n) Integer
expansion n = expansion' n 1
  where 
    expansion' : (n : Nat) -> Integer -> Vect (S n) Integer
    expansion' (S m) s = s * (toIntegerNat $ binCoef n (n `minus` (S m))) :: 
                            expansion' m (s * -1)
    expansion' Z s = [s]


showExpansion : Vect n Integer -> String
showExpansion [] = " "
showExpansion (x::xs) {n = S k} = (if x < 0 then "-" else "") ++ 
        term x k ++ showExpansion' xs
  where 
        term : Integer -> Nat -> String
        term x n = if n == 0 then (show (abs x)) else
                      (if (abs x) == 1 then "" else 
                          (show (abs x))) ++ "x" ++ 
                            (if n == 1 then "" else "^" ++ show n)
        
        sign : Integer -> String
        sign x = if x >= 0 then " + " else " - "

        showExpansion' : Vect m Integer -> String 
        showExpansion' [] = ""
        showExpansion' (y::ys) {m = S k} = sign y ++ term y k ++ 
                                                showExpansion' ys


natToFin' : (m : Nat) -> Fin (S m) 
natToFin' n with (natToFin n (S n))
    natToFin' n | Just y = y


isPrime : Nat -> Bool
isPrime Z = False
isPrime (S Z ) = False
isPrime n = foldl (\divs, term => divs && (term `mod` (toIntegerNat n)) == 0) 
              True (fullExpansion $ expansion n)  

    -- (x - 1)^p - ((x^p) - 1)
    where fullExpansion : Vect (S m) Integer -> Vect (S m) Integer
          fullExpansion (x::xs) {m} = updateAt (natToFin' m) (+1) $ (x-1)::xs 


printExpansions : Nat -> IO ()
printExpansions n = do 
      putStrLn "-- p: (x-1)^p for small p"
      sequence_ $ map printExpansion [0..n]
  where printExpansion : Nat -> IO ()
        printExpansion n = do
            print n
            putStr ": "
            putStrLn $ showExpansion $ expansion n


main : IO()
main = do 
  printExpansions 10
  putStrLn "\n-- Primes Up To 100:"
  putStrLn $ show $ filter isPrime [0..100]

