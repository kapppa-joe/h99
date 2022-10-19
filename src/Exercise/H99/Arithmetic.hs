module Exercise.H99.Arithmetic where

isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | even x = x == 2
  | otherwise = all (\d -> x `mod` d /= 0) [3, 5 .. squareRoot]
 where
  squareRoot :: Integer = fromIntegral $ ceiling $ sqrt $ fromIntegral x

myGCD :: Integer -> Integer -> Integer
myGCD x y =
  case reminder of
    0 -> abs y
    _ -> myGCD y reminder
 where
  reminder = x `rem` y