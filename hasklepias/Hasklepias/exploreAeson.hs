{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-
module Hasklepias.Test(
     Source
   , jsonFile
   , getJSON
) where
-}
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Types

type Source = Maybe (M.Map String String)

data Context a = Context {
 --     getDomain :: String
      getInfo   :: a 
    , getSource :: Source}
    deriving (Eq, Show) 

data EventDomain = 
    Lab  
    { getLoinc :: String, getValue :: Float, getUnits :: String }
  | Diagnosis 
    { getLocation :: String, getCode :: String, getCodebook  :: String }
  | Enrollment 
    { getPlan :: String }
  deriving (Show)

eventDomainfromJSON :: String -> Value -> Parser EventDomain
eventDomainfromJSON d x
    | d == "lab" = (withObject "lab" $ \o -> Lab <$> o .: "loinc" <*> o .: "value_numeric" <*> o .: "units") x
    | d == "diagnosis" = (withObject "diagnosis" $ \o -> Diagnosis <$> o .: "location" <*> o .: "code" <*> o .: "codebook") x
    | d == "enrollment" = (withObject "enrollment" $ \o -> Enrollment <$> o .: "plan") x
    | otherwise = fail "what did you expect?"
--data Lab = InfoLab  deriving (Show)

{-
instance FromJSON Lab where
  parseJSON = withObject "lab" $ \o ->
    InfoLab <$> o .: "loinc" <*> o .: "value_numeric" <*> o .: "units"
-}

--data Enrollment = InfoEnrollment  deriving (Show)
{-}
instance FromJSON Enrollment where
  parseJSON = withObject "enrollment" $ \o ->
    InfoEnrollment <$> o .: "plan" 
-}
--data Diagnosis = InfoDiagnosis  deriving (Show)
{-

instance FromJSON Diagnosis where
  parseJSON = withObject "diagnosis" $ \o ->
    InfoDiagnosis <$> o .: "location" <*> o .: "code" <*> o .: "codebook"
-}
type EventContext = Context EventDomain

--newtype EventContext = EventContext (Context EventDomain)


{- 
data Context = Context { 
     getDomain :: String
   , getInfo   :: Lab
   , getSource :: Source 
   } deriving (Show)
-}


instance FromJSON EventContext where
    parseJSON = parseEventContext

eventContext :: EventDomain -> Source -> EventContext
eventContext d s = Context d s

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




jsonFile :: FilePath
jsonFile = "Hasklepias/ExampleData0.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String EventContext)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
