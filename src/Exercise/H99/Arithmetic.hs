module Exercise.H99.Arithmetic where

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