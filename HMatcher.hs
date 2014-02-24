module HMatcher (getNMatches) where

import Data.Maybe
import Data.List

data Match = Match {
  matchIndexes :: [Int],
  original :: String,
  current :: String
} deriving (Show, Eq)

instance Ord Match where
  l1 `compare` l2 = (index l1) `compare` (index l2)
    where 
      index l = ((sum $ matchIndexes l) + 1) * (length $ original l)

makeMatch :: String -> Match
makeMatch s = Match {matchIndexes = [], current = (reverse s), original = s}

updateMatch :: Match -> Int -> Match
updateMatch l x = l {matchIndexes = x:(matchIndexes l), current = drop (x+1) (current l)}

matchIndexesForChar :: Char -> Match -> Maybe Int
matchIndexesForChar c line = elemIndex c $ current line

matchChar :: Char -> Match -> Maybe Match
matchChar c line = fmap (updateMatch line) (matchIndexesForChar c line)

charFilter :: Char -> [Match] -> [Match]
charFilter c lines = map fromJust $ filter isJust $ map (matchChar c) lines

getMatches :: String -> [Match] -> [Match]
getMatches [] lines = lines
getMatches (c:cs) lines = getMatches cs $ charFilter c lines

getNMatches :: String -> Int -> [String] -> [String]
getNMatches pattern n corpus = take n $ map original $ sort $ getMatches (reverse pattern) $ map makeMatch corpus

