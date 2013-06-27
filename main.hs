import System.Environment

import HMatcher

perform (pattern:n:xs) input = getNMatches pattern (read n :: Int) (lines input)
perform (pattern:xs) input = getNMatches pattern 10 (lines input)

main = do
  input <- getContents
  args <- getArgs
  mapM_ putStrLn $ perform args input
