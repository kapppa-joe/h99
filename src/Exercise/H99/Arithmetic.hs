module Exercise.H99.Arithmetic where

import Data.List (group)

isPrime :: Integral a => a -> Bool
isPrime x
  | x < 2 = False
  | even x = x == 2
  | otherwise = all (\d -> x `mod` d /= 0) [3, 5 .. squareRoot]
 where
  squareRoot = ceiling $ sqrt $ fromIntegral x

myGCD :: Integral a => a -> a -> a
myGCD x y =
  case reminder of
    0 -> abs y
    _ -> myGCD y reminder
 where
  reminder = x `rem` y

coprime :: Integral a => a -> a -> Bool
coprime x y = myGCD x y == 1

totient :: Integral a => a -> a
totient 1 = 1
totient x = sum [1 | y <- [1 .. (pred x)], coprime x y]

primeFactors :: Integral a => a -> [a]
primeFactors x
  | x < 2 = []
  | otherwise = factor : primeFactors (x `div` factor)
 where
  factor = head [y | y <- [2 .. x], x `rem` y == 0, isPrime y]

primeFactorsMult :: Integral a => a -> [(a, a)]
primeFactorsMult x = map packMultiplicity $ group $ primeFactors x
 where
  packMultiplicity xs = (head xs, fromIntegral $ length xs)

phi :: Integral a => a -> a
phi x = product $ map f $ primeFactorsMult x
 where
  f (fac, mult) = (fac - 1) * fac ^ (mult - 1)


primesR :: Integral a => a -> a -> [a]
primesR x y 
  | x <= 2 && y >= 2 = 2 : primesR 3 y
  | even x = filter isPrime [x + 1, x + 3 .. y]
  | otherwise = filter isPrime [x, x + 2 .. y]

goldbach :: Integral a => a -> (a, a)
goldbach x
  | odd x || x < 4 = error "must be an even number >= 4"
  | x == 4 = (2, 2)
  | otherwise = head [(prime, x - prime) | prime <- primesR 3 (x `div` 2), isPrime (x - prime)]

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList x y = [goldbach num | num <- [x'..y], even num]
  where x' = max x 4

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' x y k = [pair | pair <- goldbachList x y, fst pair > k]