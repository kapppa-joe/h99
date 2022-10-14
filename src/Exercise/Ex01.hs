module Exercise.Ex01 (myLast, myButLast, elementAt) where

myLast :: Ord a => [a] -> a
myLast lst =
  case lst of
    [] -> error "Exception: empty list"
    [x] -> x
    (_ : xs) -> myLast xs

myButLast :: Ord a => [a] -> a
myButLast lst = 
  case lst of 
    [] -> error "Exception: list has less than 2 elements"
    [_] -> error "Exception: list has less than 2 elements"
    [x, _] -> x
    (_ : xs) -> myButLast xs

elementAt :: Ord a => [a] -> Int -> a
elementAt lst index
  | null lst    = error "Exception: index too large"
  | index == 1  = head lst
  | index < 1   = error "Exception: non-positive index"
  | otherwise   = elementAt (tail lst) (index - 1)