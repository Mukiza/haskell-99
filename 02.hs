module Haskell9911to20 where
import           Data.List (group)
import Control.Applicative

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

-- silliness copied from https://www.haskell.org/haskellwiki/99_questions/Solutions/14
dupli :: [a] -> [a]
dupli = foldr ((.) <$> (:) <*> (:)) []
