module Day02 where

import           Control.Monad ((>=>))
import           Data.List     (inits, tails)
import           System.IO     (IOMode (ReadMode), hGetContents, withFile)

isDecreasing :: Ord a => [a] -> Bool
isDecreasing xs = and $ zipWith (>) xs (tail xs)

isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

hasLargeSteps :: (Ord a, Num a) => [a] -> Bool
hasLargeSteps xs = any ((> 3) . abs) $ zipWith (-) xs (tail xs)

removeOneEach :: (Ord a, Num a) => [a] -> [[a]]
removeOneEach xs = [ l ++ r | (l, _ : r) <- zip (inits xs) (tails xs) ]

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe xs = (isIncreasing xs || isDecreasing xs) && not (hasLargeSteps xs)

canBeMadeSafe :: (Ord a, Num a) => [a] -> Bool
canBeMadeSafe xs = isSafe xs || any isSafe (removeOneEach xs)

partOne :: IO ()
partOne = withFile "./Input.txt" ReadMode (hGetContents >=>
    print . length
          . filter isSafe
          . fmap (fmap read . words)
          . lines)

partTwo :: IO ()
partTwo = withFile "./Input.txt" ReadMode (hGetContents >=>
    print . length
          . filter canBeMadeSafe
          . fmap (fmap read . words)
          . lines)

