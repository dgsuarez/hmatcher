module HMatcher (getNMatches) where

import Data.Maybe
import Data.List

data Line = Line {
  score :: Int,
  original :: String,
  current :: String
} deriving (Show, Eq)

instance Ord Line where
  l1 `compare` l2 = (score l1) `compare` (score l2)

makeLine :: String -> Line
makeLine s = Line {score = 0, current = s, original = s}

updateLine :: Line -> Int -> Line
updateLine l x = l {score = (score l + x), current = drop (x+1) (current l)}

scoreForChar :: Char -> Line -> Maybe Int
scoreForChar c line = elemIndex c $ current line

matchChar :: Char -> Line -> Maybe Line
matchChar c line = fmap (updateLine line) (scoreForChar c line)

charFilter :: Char -> [Line] -> [Line]
charFilter c lines = map fromJust $ filter isJust $ map (matchChar c) lines

getMatches :: String -> [Line] -> [Line]
getMatches [] lines = lines
getMatches (c:cs) lines = getMatches cs $ charFilter c lines

getNMatches :: String -> Int -> [String] -> [String]
getNMatches pattern n corpus = take n $ map original $ sort $ getMatches pattern $ map makeLine corpus

