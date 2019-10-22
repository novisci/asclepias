{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module StagedFlowData (
) where 

import Hasklepias.IntervalAlgebra
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Text.Printf
import System.CPUTime


data State = 
     A 
   | B
   | C
   | AB
   | None
    deriving (Eq, Show)

instance ToJSON State where
    toJSON A    = "A"
    toJSON B    = "B"
    toJSON C    = "C"
    toJSON AB   = "AB"
    toJSON None = "None"


-- | Make a State from a String
makeState :: Int -> State
makeState x
 | x == 0 = A
 | x == 1 = B
 | x == 3 = C
 | otherwise = None

newtype Event = Event { unEvent :: (Period, State) }
  deriving (Eq, Show)

parseEvent :: Value -> Parser Event
parseEvent = withObject "event" $ \o -> do
    b <- o .: "s" 
    e <- o .: "e"
    s <- o .: "state"
    return $ Event (period b e, makeState s)

instance FromJSON Event where
    parseJSON = parseEvent

newtype Events = Events { unEvents :: [Event] }
  deriving (Show)

instance FromJSON Events where
  parseJSON = \case
    Object o -> (o .: "events") >>= fmap Events . parseJSON
    x -> fail $ "unexpected json: " ++ show x

newtype Subjects = Subjects { unSubjects :: [Events] }
  deriving (Show)

instance FromJSON Subjects where
  parseJSON = \case
    Object o -> (o .: "subjects") >>= fmap Subjects . parseJSON
    x -> fail $ "unexpected json: " ++ show x

getState :: Int -> Event -> State
getState t e 
    | fst (unEvent e) `contains` point t = snd $ unEvent e
    | otherwise = None

collapseStates :: [State] -> State
collapseStates l
  | A `elem` l &&  B `elem` l = AB
  | A `elem` l = A
  | B `elem` l = B
  | C `elem` l = C
  | otherwise = None

collectStates :: [Int] -> [Event] -> [State]
collectStates ts l = map (\t -> collapseStates $ map (getState t) l) ts

collectSubjectStates :: [Int] ->    -> [[State]]
collectSubjectStates ts l = map (\s -> collectStates ts $ unEvents s) (unSubjects l)


--- IO ---


inFile :: FilePath
inFile = "StageFlowData/example_data.json"


getJSON :: IO B.ByteString
getJSON = B.readFile inFile

-- Taken from : https://wiki.haskell.org/Timing_computations
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ^ 12)
    printf "Computation time: %0.6f sec\n" (diff :: Double)
    return v

main :: IO ()
main = do
    putStrLn "Starting..."
    d <- (eitherDecode <$> getJSON) :: IO (Either String Subjects)
    case d of
        Left  err -> putStrLn err
        Right ps  -> time $ I.writeFile "out_data.json" (encodeToLazyText $ collectSubjectStates [5, 10, 15, 20] ps)
    putStrLn "Done"