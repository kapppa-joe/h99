module Exercise.Ex01 (myLast, myButLast, elementAt, myLength) where

myLast :: [a] -> a
myLast lst =
  case lst of
    [] -> error "Exception: empty list"
    [x] -> x
    (_ : xs) -> myLast xs

myButLast :: [a] -> a
myButLast lst = 
  case lst of 
    [] -> error "Exception: list has less than 2 elements"
    [_] -> error "Exception: list has less than 2 elements"
    [x, _] -> x
    (_ : xs) -> myButLast xs

elementAt :: [a] -> Int -> a
elementAt lst index
  | null lst    = error "Exception: index too large"
  | index == 1  = head lst
  | index < 1   = error "Exception: non-positive index"
  | otherwise   = elementAt (tail lst) (index - 1)

myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (_:xs) = 1 + myLength xs
myLength = foldl (\acc _ -> 1 + acc) 0
