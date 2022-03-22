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

-- The number of times to replicate the data to create the "large data" tests
largeInputSize :: Int
largeInputSize = 1000

-- Generates new test data by taking each subject and replicating them
-- `largeInputSize` number of times, and where each subject ID is postfixed with
-- a number so as to make the replicated subject IDs unique
generateTestDataManySubjects :: String -> String -> IO ()
generateTestDataManySubjects infilepath outfilepath = do
  generateTestDataBase infilepath outfilepath (generateNewSubjs patientRe)

-- Generates new test data by dropping all subjects except the subject with
-- subject ID `"a"`, and replicating that subject's events `largeInputSize`
-- number of times
generateTestDataManyEvents :: String -> String -> IO ()
generateTestDataManyEvents infilepath outfilepath = do
  generateTestDataBase infilepath outfilepath transform where
    checkIfSubjA s = (s !! 2 == 'a') && (s !! 3 == '"')
    transform = generateNewEvents . filter checkIfSubjA

-- Generates a new golden testing file corresponding to the test data produced
-- by `generateTestDataManySubjects` for a cohort using the row-wise version of
-- the cohort-building application
generateGoldenManySubjectsRw :: String -> IO ()
generateGoldenManySubjectsRw outfilepath =
  writeFile outfilepath concatLines where
    concatLines = (
      concat goldenManySubjectsStart
      ++ intercalate "," goldenRwPatientManys
      ++ concat goldenRwEnd)

-- Generates a new golden testing file corresponding to the test data produced
-- by `generateTestDataManySubjects` for a cohort using the column-wise version
-- of the cohort-building application
generateGoldenManySubjectsCw :: String -> IO ()
generateGoldenManySubjectsCw outfilepath =
  writeFile outfilepath concatLines where
    concatLines = (
      concat goldenManySubjectsStart
      ++ intercalate "," goldenCwVarManys
      ++ "],\"ids\":["
      ++ intercalate "," goldenCwPatientManys
      ++ concat goldenCwEnd)

-- A helper function to read in test data from file, perform a transformation on
-- the data, and write out the updated data to file
generateTestDataBase :: String -> String -> ([String] -> [String]) -> IO ()
generateTestDataBase infilePath outfilePath transform = do
  infileLines <- readLines infilePath
  let updatedLines = transform infileLines
  let concatLines = foldr (\x y -> x ++ "\n" ++ y) "" updatedLines
  writeFile outfilePath concatLines

-- Create `largeInputSize` copies of each subject, and where each subject ID is
-- postfixed with a number so as to make the replicated subject IDs unique
generateNewSubjs :: Regex -> [String] -> [String]
generateNewSubjs re lines =
  concatMap updateIdsPtl replacements where
  updateIdsPtl = \replacement -> updateIds re replacement lines
  replacements = map (constructReplacement . formatNum) [0..(largeInputSize - 1)]

-- Given a regex `pattern` and `replacement` inputs, perform a search and
-- replace on every entry in a list of strings
updateIds :: Regex -> String -> [String] -> [String]
updateIds pattern replacement =
  map subRegexPtl where
  subRegexPtl = \line -> subRegex pattern line replacement

-- Create `largeInputSize` copies of each event
generateNewEvents :: [String] -> [String]
generateNewEvents = concat . replicate largeInputSize

-- The initial fragment of the "many subjects" golden files
goldenManySubjectsStart :: [String]
goldenManySubjectsStart =
  [ "{\"example\":[{\"attritionInfo\":[[{\"tag\":\"SubjectHasNoIndex\"},0],[{\"contents\":[1,\"dummy\"],\"tag\":\"ExcludedBy\"},0],[{\"tag\":\"Included\"},"
  , show (nValidSubj * largeInputSize)
  , "]],\"totalSubjectsProcessed\":"
  , show (nValidSubj * largeInputSize)
  , ",\"totalUnitsProcessed\":"
  , show (nValidSubj * largeInputSize)
  , "},{\"contents\":{\"attributes\":[{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"another label\",\"getPurpose\":{\"getRole\":[\"Outcome\"],\"getTags\":[]},\"getShortLabel\":\"somelabel\"},\"name\":\"myVar1\",\"type\":\"Count\"},{\"attrs\":{\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\"getShortLabel\":\"\"},\"name\":\"myVar2\",\"type\":\"Bool\"}],\"cohortData\":["
  ]

-- The row-wise data to be replicated (and where the subject IDs are to be
-- changed)
goldenRwData :: [String]
goldenRwData =
  [ "[[\"a\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  , "[[\"b\",[\"2010-07-06\",\"2010-07-07\"]],[5,true]]"
  ]

-- The replicated row-wise data (and where the subject IDs are changed)
goldenRwPatientManys :: [String]
goldenRwPatientManys = sort $ generateNewSubjs patientRe goldenRwData

-- The column-wise patietn information to be replicated (and where the subject
-- IDs are changed)
goldenCwPatientEntries :: [String]
goldenCwPatientEntries =
  [ "[\"a\",[\"2010-07-06\",\"2010-07-07\"]]"
  , "[\"b\",[\"2010-07-06\",\"2010-07-07\"]]"
  ]

-- The replicated column-wise patient information (and where the subject IDs are
-- changed)
goldenCwPatientManys :: [String]
goldenCwPatientManys = sort $ generateNewSubjs patientRe goldenCwPatientEntries

-- The column-wise data to be replicated (and where the subject IDs are changed)
goldenCwVarEntries :: [String]
goldenCwVarEntries =
  [ "5,5"
  , "true,true"
  ]

-- Replicate each element once for each patient in the input data, and then
-- surround the replicated entries by "[]" (i.e. making them a JSON array).
-- Thus, each element in the return list is a string representing a JSON array
goldenCwVarManys :: [String]
goldenCwVarManys = map (\s -> "[" ++ replicateEntries s ++ "]") goldenCwVarEntries where
  replicateEntries = intercalate "," . replicate largeInputSize

-- The concluding fragment for the "many subjects" row-wise golden file
goldenRwEnd :: [String]
goldenRwEnd = ["]},\"tag\":\"RW\"}]}"]

-- The concluding fragment for the "many subjects" column-wise golden file
goldenCwEnd :: [String]
goldenCwEnd = ["]},\"tag\":\"CW\"}]}"]

-- The number of subjects with at least one valid entry in
-- hasklepias-main/exampleApp-test/test/testData.jsonl (note that subject "c"
-- does not have any valid entries)
--
-- TODO: add a test to ensure that the test data filepath doesn't change and
-- that the number of valid subjects also doesn't change
nValidSubj :: Int
nValidSubj = 2

-- Create a 0-padded string representation of a number
formatNum :: Int -> String
formatNum = printf ("%0" ++ show numWidth ++ "d")

-- The number of characters in a string representation of `largeInputSize`
numWidth :: Int
numWidth = length $ show (largeInputSize - 1)

-- Constructs the replacement pattern where the captured string has `label`
-- appended to it
constructReplacement :: String -> String
constructReplacement label = "\"\\1-" ++ label ++ "\""

-- Read a file into a list of strings
readLines :: String -> IO [String]
readLines = fmap lines . readFile

-- A regular expression for the subject IDs in the
-- hasklepias-main/exampleApp-test/test/testData.jsonl data
--
-- TODO: add a test to ensure that this regex captures all of the subjects IDs
patientRe = mkRegex "\"(a|b|c)\""
