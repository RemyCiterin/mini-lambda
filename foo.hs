module Foo where

infixl 6 +
infixl 6 -
infixl 7 *

infix 4 ==
infix 4 /=
infix 4 <
infix 4 >
infix 4 <=
infix 4 >=
infixr 3 &&
infixr 2 ||
infixl 6 `xor`
infixl 7 .&.
infixl 5 .|.

foreign int_add :: Int -> Int -> Int
foreign int_sub :: Int -> Int -> Int
foreign int_leq :: Int -> Int -> Int
foreign int_geq :: Int -> Int -> Int
foreign int_neq :: Int -> Int -> Int
foreign int_eq :: Int -> Int -> Int
foreign int_lt :: Int -> Int -> Int
foreign int_gt :: Int -> Int -> Int
foreign int_and :: Int -> Int -> Int
foreign int_band :: Int -> Int -> Int
foreign int_or :: Int -> Int -> Int
foreign int_bor :: Int -> Int -> Int
foreign int_xor :: Int -> Int -> Int
foreign int_div :: Int -> Int -> Int
foreign int_rem :: Int -> Int -> Int
foreign int_bnot :: Int -> Int
foreign int_not :: Int -> Int
foreign int_neg :: Int -> Int

(+) x y = int_add x y
(-) x y = int_sub x y
(<=) x y = int_leq x y
(>=) x y = int_geq x y
(/=) x y = int_neq x y
(==) x y = int_eq x y
(<) x y = int_lt x y
(>) x y = int_gt x y
(&&) x y = int_and x y
(.&.) x y = int_band x y
(.|.) x y = int_bor x y
(||) x y = int_or x y
xor x y = int_xor x y
rem x y = int_rem x y
div x y = int_div x y

not = int_not
bitwiseNot = int_bnot

fibo x =
  if x < 2 then x else fibo xm1 + fibo xm2
    where
      xm2 = x - 2
      xm1 = x - 1
