module BuildLargeTestData
  ( generateGoldenManySubjectsRw
  , generateGoldenManySubjectsCw
  , generateTestDataManySubjects
  , generateTestDataManyEvents
  ) where

import Data.List ( sort
                 , intercalate
                 )
import Text.Printf
import Text.Regex

-- The number of subjects with at least 1 valid entry in the test data (note
-- that subject "c" does not have any valid entries)
nSubj :: Int
nSubj = 2

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
  replacements = map (constructReplacement . formatNum) [0..(nReplicates - 1)]

generateNewEvents :: [String] -> [String]
generateNewEvents = concat . replicate nReplicates

readLines :: String -> IO [String]
readLines = fmap lines . readFile

generateTestDataBase :: String -> String -> ([String] -> [String]) -> IO ()
generateTestDataBase infilePath outfilePath transform = do
  infileLines <- readLines infilePath
  let updatedLines = transform infileLines
  let concatLines = foldr (\x y -> x ++ "\n" ++ y) "" updatedLines
  writeFile outfilePath concatLines

generateTestDataManySubjects :: String -> String -> IO ()
generateTestDataManySubjects infilepath outfilepath = do
  generateTestDataBase infilepath outfilepath (generateNewSubjs patientRe)

generateTestDataManyEvents :: String -> String -> IO ()
generateTestDataManyEvents infilepath outfilepath = do
  generateTestDataBase infilepath outfilepath transform where
    checkIfSubjA s = (s !! 2 == 'a') && (s !! 3 == '"')
    transform = generateNewEvents . filter checkIfSubjA

generateGoldenManySubjectsRw :: String -> IO ()
generateGoldenManySubjectsRw outfilepath =
  writeFile outfilepath concatLines where
    concatLines = (
      concat outputStart
      ++ intercalate "," rwPatientManys
      ++ concat outputRwEnd)

generateGoldenManySubjectsCw :: String -> IO ()
generateGoldenManySubjectsCw outfilepath =
  writeFile outfilepath concatLines where
    concatLines = (
      concat outputStart
      ++ intercalate "," cwVarManys
      ++ "],\"ids\":["
      ++ intercalate "," cwPatientManys
      ++ concat outputCwEnd)

outputStart :: [String]
outputStart =
  [ "{\"example\":[{\"attritionInfo\":[[{\"tag\":\"SubjectHasNoIndex\"},0],[{\"contents\":[1,\"dummy\"],\"tag\":\"ExcludedBy\"},0],[{\"tag\":\"Included\"},"
  , show (2 * nReplicates)
  , "]],\"totalSubjectsProcessed\":"
  , show (2 * nReplicates)
  , ",\"totalUnitsProcessed\":"
  , show (2 * nReplicates)
  , "},{\"contents\":{\"attributes\":[{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"another label\",\"getPurpose\":{\"getRole\":[\"Outcome\"],\"getTags\":[]},\"getShortLabel\":\"somelabel\"},\"name\":\"myVar1\",\"type\":\"Count\"},{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\"getShortLabel\":\"\"},\"name\":\"myVar2\",\"type\":\"Bool\"}],\"cohortData\":["
  ]

rwPatientEntries :: [String]
rwPatientEntries =
  [ "[[\"a\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  , "[[\"b\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  ]

rwPatientManys :: [String]
rwPatientManys = sort $ generateNewSubjs patientRe rwPatientEntries

cwPatientEntries :: [String]
cwPatientEntries =
  [ "[\"a\",[\"2010-07-06\",\"2010-07-07\"]]"
  , "[\"b\",[\"2010-07-06\",\"2010-07-07\"]]"
  ]

cwPatientManys :: [String]
cwPatientManys = sort $ generateNewSubjs patientRe cwPatientEntries

cwVarEntries :: [String]
cwVarEntries =
  [ "5,5"
  , "true,true"
  ]

-- Replicate each element once for each patient in the input data, and then
-- surround the replicated entries by "[]" (i.e. making them a JSON array).
-- Thus, each element in the return list is a string representing a JSON array
cwVarManys :: [String]
cwVarManys = map (\s -> "[" ++ replicateEntries s ++ "]") cwVarEntries where
  replicateEntries = intercalate "," . replicate nReplicates

outputRwEnd :: [String]
outputRwEnd = ["]},\"tag\":\"RW\"}]}"]

outputCwEnd :: [String]
outputCwEnd = ["]},\"tag\":\"CW\"}]}"]
