module HMatcher (getNMatches) where

import Data.Maybe
import Data.List
import Data.Char

data Match = Match {
  matchIndexes :: [Int],
  original :: String,
  current :: String
} deriving (Show, Eq)

instance Ord Match where
  l1 `compare` l2 = index l1 `compare` index l2
    where
      index l = sumIndices l * chunckCount l * originalLength l
      sumIndices l = (+ 1) $ sum $ matchIndexes l
      originalLength l = length $ original l
      chunckCount l = (+ 1) $ length $ filter (> 0) (matchIndexes l)

makeMatch :: String -> Match
makeMatch s = Match {
  matchIndexes = [],
  current = reverse s,
  original = s
}

addIndexToMatch :: Match -> Int -> Match
addIndexToMatch match idx = match {
  matchIndexes = idx:matchIndexes match,
  current = drop (idx+1) (current match)
}

matchIndexForChar :: Char -> Match -> Maybe Int
matchIndexForChar c possibleMatch = elemIndex c $ current possibleMatch

matchChar :: Char -> Match -> Maybe Match
matchChar c possibleMatch = do
  idx <- matchIndexForChar c possibleMatch
  return $ addIndexToMatch possibleMatch idx

charFilter :: Char -> [Match] -> [Match]
charFilter c = mapMaybe (matchChar c)

getMatches :: String -> [Match] -> [Match]
getMatches searchPattern possibleMatches = foldl (flip charFilter) possibleMatches searchPattern

getNMatches :: String -> Int -> [String] -> [String]
getNMatches searchPattern limit corpus = take limit $
                                         map original $
                                         sort $
                                         getMatches (reverse searchPattern) $
                                         map makeMatch corpus

