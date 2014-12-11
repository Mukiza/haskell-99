module Haskell9901To10 where
import Data.List
import Control.Arrow

myLast :: [a] -> a
myLast [] = error "You cannot get last of an empty list you dummy"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "This list empty"
myButLast [_] = error "This list has one element"
myButLast xs =
  let len = length xs
  in xs !! (len - 2)

elementAt :: Int -> [a] -> a
elementAt n xs = xs !! n

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = if  x `notElem` xs then x:compress xs else compress xs

compress' :: Eq a => [a] -> [a]
compress' = map head . group

pack :: Eq a =>  [a] -> [[a]]
pack [] = []
pack (x:xs) = let (hd, tl) = span (== x) xs
              in (x:hd) : pack tl

encode ::  Eq a => [a] -> [(Int, a)]
encode = map (length &&& head) . group
