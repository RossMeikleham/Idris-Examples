-- Implementation of sorting algorithms in Idris

module Sorting

import Data.Vect

-- | Simple Mergesort for Lists 

mergeSort : Ord a => List a -> List a

-- Empty or lists with a single element are already sorted
mergeSort Nil = Nil
mergeSort (x::Nil) = [x] 

mergeSort xs = 
  let (left, right) = splitAt (length xs `div` 2) xs in
    merge (mergeSort left) (mergeSort right)

  where
        -- Merge two sorted lists into a single sorted list
        merge : Ord a => List a -> List a -> List a
        merge Nil ys = ys 
        merge xs Nil = xs 
        merge (x::xs) (y::ys) = if x < y 
                                  then (x::(merge xs (y::ys))) 
                                  else (y::(merge (x::xs) ys))





-- | Mergesort implementation for Vectors (List with Size)

-- First define merge operation on 2 sorted vectors
mergeV : Ord a => Vect q a -> Vect r a -> Vect (q + r) a
mergeV Nil ys = ys 
mergeV (x::xs) Nil ?= x::xs 
mergeV (x::xs) (y::ys) with (x < y) 
  | True =   (x :: (mergeV xs (y :: ys))) 
  | False ?=  (y :: (mergeV (x :: xs) ys))


mergeV_lemma_1 = proof
  intros
  rewrite sym (plusZeroRightNeutral k)
  trivial

mergeV_lemma_2 = proof
  intros
  rewrite (plusSuccRightSucc k k1)
  trivial


splitNatThm : (m : Nat) -> (n : Fin m) -> (finToNat n + (m - finToNat n)) = m
splitNatThm = ?todothm

-- | Given a natural number n and a natural number k <= n, 
--   returns a dependent pair containing tuple (k, l) s.t. k + l = n and
--   a proof that this holds. The idea is to use this to transform a vector
--   of size n "Vec n a" into a Vec of size k + l "Vec (k + l)" in which
--   we can split and rejoin for mergesorting
splitNat : (n : Nat) -> (Fin n) -> Sigma (Nat, Nat) (\(a, b) => n = a + b) 
splitNat x f = MkSigma (finToNat f, x - finToNat f) (rewrite splitNatThm x f in Refl)
  



{-
mergeSortV : Vect n Nat -> Vect n Nat
mergeSortV Nil = Nil
mergeSortV (x::Nil) = [x]
mergeSortV {n} xs = ?ffddifd -- mergeV (mergeSortV (fst lr)) (mergeSortV (snd lr))
  where
    parition : (Vect n Nat) -> Sigma (Nat, Nat) (\(c, d) => c + d = n) -> (Vect a Nat, Vect b Nat) 
    parition xs (MkSigma (l, r) p) = Data.VectType.Vect.splitAt l newV
      where newV : Vect (a + b) Nat
            newV ?= xs
    
    --splitNat n
    
    
    --x = ln
    --lr : (Vect a Nat, Vect b Nat)
    --lr = rewrite p in (split ln xs)
    
-}   
   
   
    
