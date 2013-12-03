module Chap01.FreeNumber () where

import Data.Array
import qualified Data.List as List

doesNotBelong :: Eq a => [a] -> a -> Bool
doesNotBelong (x:xs) y = if (x == y)
                            then False
                            else doesNotBelong xs y
doesNotBelong _ _ = True

firstfree :: [Int] -> Int
firstfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (doesNotBelong vs) us

--search the first false entry
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, length xs) [(x, True) | x <- xs, inRange (0, length xs) x]

minfree = search . checklist

minfrom :: Int -> [Int] -> Int
minfrom a xs
  | null xs = a
  | length us == b - a = minfrom b vs
  | otherwise = minfrom a us
  where (us, vs) = List.partition (< b) xs
        b = a + 1 + fromIntegral (truncate (fromIntegral (length xs) / 2))
