module BuildLargeTestData
  ( makeManySubjectsTestData
  ) where

import Text.Printf
import Text.Regex

nReplicates :: Int
-- nReplicates = 1000
nReplicates = 10

nDigits :: Int
nDigits = length $ show (nReplicates - 1)

formatNum :: Int -> String
formatNum = printf ("%0" ++ show nDigits ++ "s") . show

updateIds :: Regex -> String -> [String] -> [String]
updateIds pattern replacement =
  map subRegexPtl where
  subRegexPtl = \line -> subRegex pattern line replacement

constructReplacement :: String -> String
constructReplacement label = "\"\\1-" ++ label ++ "\""

generateLines :: Regex -> [String] -> [[String]]
generateLines re lines =
  map updateIdsPtl replacements where
  updateIdsPtl = \replacement -> updateIds re replacement lines
  replacements = map (constructReplacement . formatNum) [0..nReplicates]

readLines :: String -> IO [String]
readLines = fmap lines . readFile

makeManySubjectsTestData :: String -> String -> IO ()
makeManySubjectsTestData infilepath outfilepath = do
  let re = mkRegex "\"(a|b|c)\""
  infilelines <- readLines infilepath
  let newLines = concat $ generateLines re infilelines
  let newStr = foldr (\x y -> x ++ "\n" ++ y) "" newLines
  writeFile outfilepath newStr
