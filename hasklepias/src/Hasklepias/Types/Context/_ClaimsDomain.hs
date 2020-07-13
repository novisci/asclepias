{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.Types.Context.ClaimsDomain(
   ClaimsDomain
 , domain
 , get
 , Claim(..)
 , Code(..)
 , Codebook(..)
 , Demographics(..)
 , Diagnosis(..)
 , Insurance(..)
 , Lab(..)
 , Provider(..)
 , ServiceLocation(..) 
 , Value(..)
) where

import Data.Text
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM

-- | makeTextParser creates a function that converts an Object
--   to a Parser for parsing Text from JSON. This function takes 
--   the key and a function that modifies the Text value.
--
--   TODO: This could probably be vastly simplified with a better
--         understanding of Aeson
--   TODO: Get a better understanding of Data.Text. Should we use
--         the Text type instead of String within our data objects? 
makeTextParser :: String -> (Text -> a) -> (AT.Object -> AT.Parser a)
makeTextParser key f o = do 
      let hold = HM.lookup (pack key) o
      out <- case hold of
        Just (AT.String x) -> return x
        _ -> fail $ key ++ " must be a string"
      return $ f out

-- | A ServiceLocation specifies the location at which an event occurred
data ServiceLocation = 
     Inpatient
   | Outpatient
   | LocationUnknown
  deriving (Show, Eq)

instance AT.FromJSON ServiceLocation where
  parseJSON = AT.withObject "ServiceLocation" $ 
    makeTextParser "location" makeLocation

-- | Convert a String to a ServiceLocation
makeLocation :: Text -> ServiceLocation
makeLocation "Inpatient"  = Inpatient
makeLocation "Outpatient" = Outpatient
makeLocation _            = LocationUnknown

-- | Define available Codebooks
data Codebook =
    ICD9
  | ICD9CM
  | ICD10
  | ICD10CM
  | HCPCS
  | CPT
  | NDC
  | MEDDRA
  | CodebookUnknown
  deriving (Show, Eq)

instance AT.FromJSON Codebook where
  parseJSON = AT.withObject "Codebook" $ 
    makeTextParser "codebook" makeCodebook

-- | Make a Codebook from a String
makeCodebook :: Text -> Codebook
makeCodebook "ICD9"    = ICD9
makeCodebook "ICD9CM"  = ICD9CM
makeCodebook "ICD10"   = ICD10
makeCodebook "ICD10CM" = ICD10CM
makeCodebook "HCPCS"   = HCPCS
makeCodebook "CPT"     = CPT
makeCodebook "NDC"     = NDC
makeCodebook "MEDDRA"  = MEDDRA
makeCodebook _         = CodebookUnknown

-- | A Code is a string from a Codebook that makes to some
--   concept within that codebook\
data Code = Code 
  {   code       :: String
    , codebook   :: Codebook }
  deriving (Show, Eq)

instance AT.FromJSON Code where
  parseJSON = AT.withObject "Code" $ \o -> do
    cd <- makeTextParser "codebook" makeCodebook o
    co <- makeTextParser "code" id o
    return $ Code (unpack co) cd    

-- | A Value is something that is measured. It can have a 
--   numeric part and/or a text part and/or associated units
data Value = Value 
  {   numValue   :: Maybe Float
    , txtValue   :: Maybe String
    , units      :: Maybe String }
   deriving (Show, Eq)

instance AT.FromJSON Value where
  parseJSON = AT.withObject "Value" $ \o -> do
     nval <- o AT..:? "value"
     tval <- o AT..:? "text"
     unit <- o AT..:? "units"
     return $ Value nval tval unit

-- | Claim 
--   TODO: describe this
--   TODO: what other information do we need? I included Cost here
--         but this may not be useful or the right place.
data Claim = Claim 
  {    claimID    :: String
     , claimCost  :: Maybe Float }
    deriving (Show, Eq)

instance AT.FromJSON Claim where
  parseJSON = AT.withObject "Claim" $ \o -> do
    ci <- makeTextParser "id" id o
    ct <- o AT..:? "cost"
    return $ Claim (unpack ci) ct 

-- | Provider
--   TODO: describe this
data Provider = Provider
  {   providerNPI :: String
    , providerTaxonomy    :: String }
   deriving (Show, Eq)

instance AT.FromJSON Provider where
  parseJSON = AT.withObject "Provider" $ \o -> do
    npi <- makeTextParser "npi" id o
    tax <- makeTextParser "taxonomy" id o
    return $ Provider (unpack npi) (unpack tax)

-- | The Demographics type contains (obviously) demographic 
--   information. The kind of information is held in getDemoField,
--   and the actually information is in getDemoInfo.
--
--   TODO: I don't like this way of handling demographic data,
--         but it's a place to start.
data Demographics = Demographics
  {   demoField  :: String
    , demoInfo   :: String } 
  deriving (Show, Eq)

instance AT.FromJSON Demographics where
  parseJSON = AT.withObject "Demographics" $ \o -> do
    f <- makeTextParser "field" id o
    i <- makeTextParser "info" id o
    return $ Demographics (unpack f) (unpack i)

instance ClaimDomain Demographics where
  domain = DmDomain
  get f (DmDomain x) = f x


-- | Diagnosis types contains information about medical diagnoses

data Diagnosis = Diagnosis
  {   dxLocation :: ServiceLocation
    , dxCode     :: Code
    , dxClaim    :: Maybe Claim
    , dxProvider :: Maybe Provider }
  deriving (Show, Eq)

instance AT.FromJSON Diagnosis where
  parseJSON = AT.withObject "DX" $ \o -> do
    l <- AT.parseJSON (AT.Object o)
    c <- AT.parseJSON (AT.Object o)
    m <- (o AT..:? "claim")    :: AT.Parser (Maybe Claim)
    p <- (o AT..:? "provider") :: AT.Parser (Maybe Provider)
    return $ Diagnosis l c m p

instance ClaimDomain Diagnosis where
  domain = DxDomain
  get f (DxDomain x) = f x

-- | Procedure types contain information about medical procedures
--
--   TODO: Diagnoses, Procedures, Labs, and Medications have similar
--         structures. Should they? Or can a sum type be used?
data Procedure = Procedure
  {   prLocation :: ServiceLocation
    , prCode     :: Code
    , prClaim    :: Maybe Claim
    , prProvider :: Maybe Provider }
  deriving (Show, Eq)

instance AT.FromJSON Procedure where
  parseJSON = AT.withObject "PR" $ \o -> do
    l <- AT.parseJSON (AT.Object o)
    c <- AT.parseJSON (AT.Object o)
    m <- (o AT..:? "claim")    :: AT.Parser (Maybe Claim)
    p <- (o AT..:? "provider") :: AT.Parser (Maybe Provider)
    return $ Procedure l c m p

instance ClaimDomain Procedure where
  domain = PrDomain
  get f (PrDomain x) = f x

-- | Lab types contain information about laboratory results
data Lab = Lab
  {   labLocation :: ServiceLocation
    , labCode     :: Code
    , labClaim    :: Maybe Claim
    , labProvider :: Maybe Provider 
    , labValue    :: Value } 
  deriving (Show, Eq)

instance AT.FromJSON Lab where
  parseJSON = AT.withObject "Lab" $ \o -> do
    l <- AT.parseJSON (AT.Object o)
    c <- AT.parseJSON (AT.Object o)
    m <- (o AT..:? "claim")    :: AT.Parser (Maybe Claim)
    p <- (o AT..:? "provider") :: AT.Parser (Maybe Provider)
    v <- AT.parseJSON (AT.Object o)
    return $ Lab l c m p v

instance ClaimDomain Lab where
  domain = LbDomain
  get f (LbDomain x) = f x

-- | Medication types contain information about medication events 
--   including dosage (as a Value)

data Medication = Medication
  {   rxLocation  :: ServiceLocation
    , rxCode      :: Code
    , rxClaim     :: Maybe Claim
    , rxProvider  :: Maybe Provider 
    , rxDose      :: Value }
   deriving (Show, Eq)

instance AT.FromJSON Medication where
  parseJSON = AT.withObject "RX" $ \o -> do
    l <- AT.parseJSON (AT.Object o) -- TODO clean up duplication
    c <- AT.parseJSON (AT.Object o)
    m <- (o AT..:? "claim")    :: AT.Parser (Maybe Claim)
    p <- (o AT..:? "provider") :: AT.Parser (Maybe Provider)
    v <- AT.parseJSON (AT.Object o)
    return $ Medication l c m p v

instance ClaimDomain Medication where
  domain = RxDomain
  get f (RxDomain x) = f x

-- | Insurance types contain information about insurance events
--   such as enrollment
data Insurance = Insurance
  {   plan        :: String
    , insurer     :: String }
  deriving (Show, Eq)

instance AT.FromJSON Insurance where
  parseJSON = AT.withObject "Demographics" $ \o -> do
    p <- o AT..: "plan"
    i <- o AT..: "insurer"
    return $ Insurance (unpack p) (unpack i)

instance ClaimDomain Insurance where
  domain = InDomain
  get f (InDomain x) = f x


-- | TODO

data ClaimsDomain = 
    DmDomain Demographics
  | DxDomain Diagnosis
  | PrDomain Procedure
  | LbDomain Lab
  | RxDomain Medication
  | InDomain Insurance
  deriving (Eq, Show)

-- | TODO 
class ClaimDomain a where
  domain :: a -> ClaimsDomain
  get    :: (a -> b) -> ClaimsDomain -> b





