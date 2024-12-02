module Day01 where

import           Control.Monad       ((>=>))
import           Data.Bifunctor      (bimap)
import           Data.List           (sort)
import           System.IO           (IOMode (ReadMode), hGetContents, withFile)

partOne :: IO ()
partOne = withFile "./Input.txt" ReadMode (hGetContents >=> ( print
                                                            . sum
                                                            . fmap abs
                                                            . uncurry (zipWith (-))
                                                            . bimap sort sort
                                                            . unzip
                                                            . fmap ((\ [x, y] -> (read x, read y)) . words)
                                                            . lines))

partTwo :: IO ()
partTwo = withFile "./Input.txt" ReadMode (hGetContents >=> ( print
                                                            . sum
                                                            . (\ (xs, ys) -> fmap (\ x -> (* x) . length . filter (== x) $ ys) xs)
                                                            . unzip
                                                            . fmap ((\ [x, y] -> (read x, read y)) . words)
                                                            . lines ))

