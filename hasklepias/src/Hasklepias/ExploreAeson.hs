{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


--module Hasklepias.ExploreAeson() 
--   where

--import qualified Data.Map.Strict() as M
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Types
import Hasklepias.IntervalAlgebra
import Hasklepias.Events

eventDomainfromJSON :: String -> Value -> Parser EventDomain
eventDomainfromJSON d x
    | d == "lab" = (withObject "lab" $ \o -> labDomain lab <$> o .: "loinc" <*> o .: "value_numeric" <*> o .: "units") x 
    | d == "diagnosis" = (withObject "diagnosis" $ \o -> DxDomain $ Diagnosis <$> o .: "location" <*> o .: "code" <*> o .: "codebook") x
    | d == "enrollment" = (withObject "enrollment" $ \o -> EnDomain Enrollment <$> o .: "plan") x
    | otherwise = fail "what did you expect?"

parseEventContext :: Value -> Parser EventContext
parseEventContext = withObject "context" $ \o -> do
    d <- o .: "domain"
    let info = HM.lookup "info" o
    iField <- case info of 
        Just x  -> return x
        Nothing -> fail "xxx"

    i <- eventDomainfromJSON d iField
    s <- o .:? "source"    
    return $ eventContext i s

parseEvent :: Value -> Parser Event
parseEvent = withObject "event" $ \o -> do
    c <- o .: "context" 
    ctxt <- parseEventContext c
    p <- o .: "period"
    per <- parsePeriod p
    return $ event per ctxt

--newtype Event = Event (Period, EventContext)
--  deriving (Show)

instance FromJSON Event where
    parseJSON = parseEvent

--event :: Period -> EventContext -> Event
--event p c = Event (p, c)


parsePeriod :: Value -> Parser Period
parsePeriod = withObject "period" $ \o -> do
    b <- o .: "begin"
    e <- o .: "end"
    return $ period b e


jsonFile :: FilePath
jsonFile = "Hasklepias/ExampleData0.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


main :: IO ()
main = do
 -- Get JSON data and decode it
 -- d <- (eitherDecode <$> getJSON) :: IO (Either String EventContext)
 d <- (eitherDecode <$> getJSON) :: IO (Either String Event)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
