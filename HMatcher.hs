module HMatcher (getNMatches) where

import Data.Maybe
import Data.List

data Line = Line {
  matchIndexes :: [Int],
  original :: String,
  current :: String
} deriving (Show, Eq)

instance Ord Line where
  l1 `compare` l2 = [dispersion m1, position m1] `compare` [dispersion m2, position m2]
    where 
      m1 = matchIndexes l1
      m2 = matchIndexes l2
      dispersion = foldr1 (-) -- should be standard deviation
      position = foldr1 (+) -- mean?

makeLine :: String -> Line
makeLine s = Line {matchIndexes = [], current = s, original = s}

updateLine :: Line -> Int -> Line
updateLine l x = l {matchIndexes = x:(matchIndexes l), current = drop (x+1) (current l)}

matchIndexesForChar :: Char -> Line -> Maybe Int
matchIndexesForChar c line = elemIndex c $ current line

matchChar :: Char -> Line -> Maybe Line
matchChar c line = fmap (updateLine line) (matchIndexesForChar c line)

charFilter :: Char -> [Line] -> [Line]
charFilter c lines = map fromJust $ filter isJust $ map (matchChar c) lines

getMatches :: String -> [Line] -> [Line]
getMatches [] lines = lines
getMatches (c:cs) lines = getMatches cs $ charFilter c lines

getNMatches :: String -> Int -> [String] -> [String]
getNMatches pattern n corpus = take n $ map original $ sort $ getMatches pattern $ map makeLine corpus

