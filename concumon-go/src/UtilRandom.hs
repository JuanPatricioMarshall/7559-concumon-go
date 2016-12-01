module UtilRandom
    ( UtilRandom.getRandomElem,
      UtilRandom.getRandomInt
    ) where

import System.Random

getRandomElem :: [a] -> IO a
getRandomElem xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

getRandomInt :: Int -> Int -> IO Int
getRandomInt a b = randomRIO (a, b)