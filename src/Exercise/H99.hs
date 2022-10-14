module Exercise.H99 where

import Control.Monad (liftM2)

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
  | null lst = error "Exception: index too large"
  | index == 1 = head lst
  | index < 1 = error "Exception: non-positive index"
  | otherwise = elementAt (tail lst) (index - 1)

myLength :: [a] -> Int
myLength = foldl (\acc _ -> 1 + acc) 0

-- myLength [] = 0
-- myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = liftM2 (==) myReverse id

-- isPalindrome xs =
--   case xs of
--     []  -> True
--     [_] -> True
--     _   -> firstElem == lastElem && isPalindrome middlePart
--       where firstElem = head xs
--             lastElem = last xs
--             middlePart = tail . init $ xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : y : xs) =
  if x == y
    then compress (y : xs)
    else x : compress (y : xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = firstGroup : pack remaining
  where
    (firstGroup, remaining) = span (== head xs) xs

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = (length firstGroup, headChar) : encode remaining
  where
    headChar = head xs
    (firstGroup, remaining) = span (== head xs) xs

data SubList a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [SubList a]
encodeModified ([] :: [a]) = [] :: [SubList a]
encodeModified xs = firstGroupEncoded : encodeModified remaining
  where
    headChar = head xs
    (firstGroup, remaining) = span (== headChar) xs
    firstGroupEncoded = 
      if length firstGroup == 1 
        then Single headChar 
        else Multiple (length firstGroup) headChar