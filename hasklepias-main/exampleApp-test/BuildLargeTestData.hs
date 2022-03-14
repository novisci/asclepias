module BuildLargeTestData
  ( makeManySubjectsTestData
  , makeManyEventsTestData
  ) where

import Text.Printf
import Text.Regex

nReplicates :: Int
nReplicates = 1000

nDigits :: Int
nDigits = length $ show (nReplicates - 1)

formatNum :: Int -> String
formatNum = printf ("%0" ++ show nDigits ++ "s") . show

patientRe = mkRegex "\"(a|b|c)\""

updateIds :: Regex -> String -> [String] -> [String]
updateIds pattern replacement =
  map subRegexPtl where
  subRegexPtl = \line -> subRegex pattern line replacement

constructReplacement :: String -> String
constructReplacement label = "\"\1-" ++ label ++ "\""

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
  replacements = map (constructReplacement . formatNum) [0..(nReplicates - 1)]

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
  generateData infilepath outfilepath (generateNewSubjs patientRe)

makeManyEventsTestData :: String -> String -> IO ()
makeManyEventsTestData infilepath outfilepath = do
  generateData infilepath outfilepath transform where
    checkIfSubjA s = (s !! 2 == 'a') && (s !! 3 == '"')
    transform = generateNewEvents . filter checkIfSubjA

outputStart :: String
outputStart = "{\"example\":[{\"attritionInfo\":[[{\"tag\":\"SubjectHasNoIndex\"},0],[{\"contents\":[1,\"dummy\"],\"tag\":\"ExcludedBy\"},0],[{\"tag\":\"Included\"},2]],\"totalSubjectsProcessed\":2,\"totalUnitsProcessed\":2},{\"contents\":{\"attributes\":[{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"another label\",\"getPurpose\":{\"getRole\":[\"Outcome\"],\"getTags\":[]},\"getShortLabel\":\"somelabel\"},\"name\":\"myVar1\",\"type\":\"Count\"},{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\"getShortLabel\":\"\"},\"name\":\"myVar2\",\"type\":\"Bool\"}],\"cohortData\":["

rwPatientEntries :: [String]
rwPatientEntries =
  [ "[[\"a\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  , "[[\"b\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  ]

rwPatientManys :: [String]
rwPatientManys = generateNewSubjs patientRe rwPatientEntries

cwVarEntries :: [String]
cwVarEntries =
  [ "5"
  , "true"
  , "[\"a\",[\"2010-07-06\",\"2010-07-07\"]]"
  ]

outputEnd :: String
outputEnd = "]},\"tag\":\"RW\"}]}"
