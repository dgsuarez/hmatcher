import System.Environment

import HMatcher

perform (searchPattern:limit:xs) input = getNMatches searchPattern (read limit :: Int) (lines input)
perform (searchPattern:xs) input = getNMatches searchPattern 10 (lines input)

main = do
  input <- getContents
  args <- getArgs
  mapM_ putStrLn $ perform args input
