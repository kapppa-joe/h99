module Exercise.H99.List where

import Control.Monad (liftM2)
import System.Random (randomRs, getStdGen)

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
myLength xs = sum [1 | _ <- xs]

-- myLength = foldl (\acc _ -> 1 + acc) 0

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
encodeModified [] = []
encodeModified xs = firstGroupEncoded : encodeModified remaining
 where
  headChar = head xs
  (firstGroup, remaining) = span (== headChar) xs
  firstGroupEncoded =
    if length firstGroup == 1
      then Single headChar
      else Multiple (length firstGroup) headChar

decodeModified :: Eq a => [SubList a] -> [a]
decodeModified [] = []
decodeModified (x : xs) = decodedFirstGroup ++ decodeModified xs
 where
  decodedFirstGroup =
    case x of
      Single char -> [char]
      Multiple count char -> replicate count char

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli _ 0 = []
repli [] _ = []
-- repli (x:xs) n = x : repli [x] (n-1) ++ repli xs n
repli (x : xs) n = replicatedX ++ repli xs n
 where
  replicatedX = (take n . repeat) x {- HLINT ignore "Use replicate" -}

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split l@(x : xs) n
  | n <= 0 = ([], l)
  | otherwise = (x : firstPart, secondPart)
 where
  (firstPart, secondPart) = split xs (n - 1)


slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop (i - 1) xs

rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotate xs (length xs + n)
  | otherwise = zs ++ ys
    where (ys, zs) = split xs n

removeAt ::  Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt n xs = (x, ys' ++ zs)
  where (ys, zs) = split xs n
        x = if null zs then Nothing else Just $ last ys
        ys' = if null zs then ys else init ys

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = ys ++ x : zs
  where ys = take (n - 1) xs
        zs = drop (n - 1) xs

range :: Int -> Int -> [Int]
range from to
  | to < from  = []
  | otherwise = from : range (from + 1) to

-- rndSelect :: [a] -> Int -> IO [a]
-- rndSelect xs n = do
--     gen <- getStdGen
--     return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]