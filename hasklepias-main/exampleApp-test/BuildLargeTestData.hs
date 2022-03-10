module BuildLargeTestData
  ( makeManySubjectsTestData
  , makeManyEventsTestData
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

generateLines ::
     (String -> Bool)
  -> ([String] -> [String])
  -> [String]
  -> [String]
generateLines pred generateFcn = generateFcn . filter pred

generateNewSubjs :: Regex -> [String] -> [String]
generateNewSubjs re lines =
  concatMap updateIdsPtl replacements where
  updateIdsPtl = \replacement -> updateIds re replacement lines
  replacements = map (constructReplacement . formatNum) [0..nReplicates]

generateNewEvents :: [String] -> [String]
generateNewEvents = concat . replicate nReplicates

readLines :: String -> IO [String]
readLines = fmap lines . readFile

generateData :: String -> String -> ([String] -> [String]) -> IO ()
generateData infilePath outfilePath transform = do
  infileLines <- readLines infilePath
  let updatedLines = transform infileLines
  let concatLines = foldr (\x y -> x ++ "\n" ++ y) "" updatedLines
  writeFile outfilePath concatLines

makeManySubjectsTestData :: String -> String -> IO ()
makeManySubjectsTestData infilepath outfilepath = do
  generateData infilepath outfilepath (generateNewSubjs $ mkRegex "\"(a|b|c)\"")

makeManyEventsTestData :: String -> String -> IO ()
makeManyEventsTestData infilepath outfilepath = do
  generateData infilepath outfilepath transform where
    checkIfSubjA s = (s !! 2 == 'a') && (s !! 3 == '"')
    transform = generateNewEvents . filter checkIfSubjA
