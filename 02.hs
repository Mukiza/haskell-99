module Haskell9911to20 where
import           Control.Applicative
import           Data.List           (group)

data ItemCount a = Multiple Int a | Single a deriving (Show)

encodeMultiple :: Eq a => [a] -> [ItemCount a]
encodeMultiple = map isSingleOrMultiple . group
       where isSingleOrMultiple xs =
               let hd = head xs
               in if length xs > 1
                  then Multiple (length xs)  hd
                  else Single hd

decodeModified :: [ItemCount a] -> [a]
decodeModified = concatMap decode
                  where
                    decode (Multiple x c) = replicate x c
                    decode (Single c) = [c]

encode :: [a] -> ItemCount a
encode xs
  | length xs > 1 = Multiple (length xs) (head xs)
  | otherwise = Single (head xs)

encodeDirect :: Eq a => [a] -> [ItemCount a]
encodeDirect [] = []
encodeDirect  (x:xs) = encode matched : encodeDirect rest
                       where (matched, rest) = span (== x) (x:xs)

duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

-- silliness from https://www.haskell.org/haskellwiki/99_questions/Solutions/14
dupli :: [a] -> [a]
dupli = foldr ((.) <$> (:) <*> (:)) []

replicate' :: [a] -> Int -> [a]
replicate' xs n = concatMap repeatN xs
                  where repeatN = (take n . repeat)

dropEvery :: Show a => [a] -> Int -> [a]
dropEvery xs n =
  let (start, rest) = splitAt (n-1) xs
  in if null rest
     then start
     else start ++ dropEvery (tail rest) n

split :: [a] -> Int -> ([a], [a])
split xs n = splitAt n  xs

slice :: [a] -> Int -> Int -> [a]
slice xs n k = take (k - n + 1) (drop (n - 1) xs)

rotate :: [a] -> Int -> [a]
rotate xs n
  | n > 0 =
      let (first, rest) = splitAt n xs
      in rest ++ first
  | otherwise =
      let (first, rest) = splitAt (abs n) (reverse xs)
      in reverse first ++ reverse rest

removeAt :: [a] -> Int -> (a, [a])
removeAt xs n =
  let (hd, tl) = splitAt (n - 1) xs
  in (head tl, hd ++ tail tl)

range' :: (Eq a, Enum a) => a -> a -> [a]
range' x y
  | y == x = [y]
  | otherwise = x :range' (succ x) y
