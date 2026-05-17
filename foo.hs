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
foreign int_mul :: Int -> Int -> Int
foreign int_rem :: Int -> Int -> Int
foreign int_bnot :: Int -> Int
foreign int_not :: Int -> Int
foreign int_neg :: Int -> Int

(*) x y = int_mul x y
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

not x = int_not x
bitwiseNot x = int_bnot x

data List a
 = Nil
 | Cons a (List a)

cons a l = Cons a l
nil = Nil

map f list =
  case list of
    Cons a l -> Cons (f a) (map f l)
    Nil -> Nil

arange n = if n == 0 then Nil else Cons 0 (map (\ x -> x + 1) (arange (n-1)))
-- arange n = go 0 n
--   where
--     go x y =
--       if x >= y
--       then Nil
--       else Cons x (go (x+1) y)

sum list =
  case list of
    Cons a b -> a + sum b
    Nil -> 0

data Maybe a
  = Just a
  | Nothing

option x =
  case x of
    Just (Just x) -> x
    _ -> 0

identity = \ x -> option (Just (Just x))

facto x =
  if x == 0 then 1 else x * facto (x-1)

fibo x =
  case x of
    0 -> 0
    1 -> 1
    _ -> fibo xm1 + fibo xm2
  where
    xm1 = x - 1
    xm2 = identity (xm1 - 1)
