module Hasklepias.Events(
   Event
 , EventDomain(..)
 , EventContext
 , labDomain
 , makeDemographics
 , makeLocation
 , makeCode
 , makeDiagnosis
 , makeInsurance
 , makeLab
 , makeMedication
 , makeNumValue
 , makeProcedure
 , event
 , eventContext
 , events
) where

import Hasklepias.Context
import Hasklepias.IntervalAlgebra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | TODO

--newtype ProtoEvent a = ProtoEvent (Period, Context a)
-- deriving (Show)

-- | TODO

newtype Event = Event (Period, EventContext)
  deriving (Show)

-- | TODO

event :: Period -> EventContext -> Event
event p c = Event (p, c)

-- | A ServiceLocation specifies the location at which an event occurred

data ServiceLocation = 
     Inpatient
   | Outpatient
   | LocationUnknown
  deriving (Show, Eq)

-- | Convert a String to a ServiceLocation

makeLocation :: String -> ServiceLocation
makeLocation "Inpatient"  = Inpatient
makeLocation "Outpatient" = Outpatient
makeLocation _            = LocationUnknown

-- | Define available codebooks

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

makeCodebook :: String -> Codebook
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
--   concept within that codebook

data Code = Code 
  {   getCode       :: String
    , getCodebook   :: Codebook }
  deriving (Show, Eq)

-- | Convert a String to a Code

makeCode :: String -> String -> Code
makeCode x y = Code x $ makeCodebook y

-- | A Value is something that is measured. It can have a 
--   numeric part and/or a text part and/or associated units

data Value = Value 
  {   getNumValue   :: Maybe Float
    , getTxtValue   :: Maybe String
    , getUnits      :: Maybe String }
  deriving (Show, Eq)

-- | Create a numeric value type

makeNumValue :: Float -> String -> Value
makeNumValue x y = Value (Just x) Nothing (Just y)

-- | The Demographics type contains (obviously) demographic 
--   information. The kind of information is held in getDemoField,
--   and the actually information is in getDemoInfo.

data Demographics = Demographics
  {   getDemoField  :: String
    , getDemoInfo   :: String } 
  deriving (Show, Eq)

-- | Create a Demographic type from String inputs

makeDemographics :: String -> String -> Demographics
makeDemographics = Demographics

-- | Diagnosis types contains information about medical diagnoses

data Diagnosis = Diagnosis
  {   getDXLocation :: ServiceLocation
    , getDXcode     :: Code }
  deriving (Show, Eq)

-- | Create a Diagnosis from String inputs

makeDiagnosis :: String -> String -> String -> Diagnosis
makeDiagnosis x y z = Diagnosis (makeLocation x) (makeCode y z)

-- | Procedure types contain information about medical procedures

data Procedure = Procedure
  {   getPRLocation :: ServiceLocation
    , getPRCode     :: Code }
  deriving (Show, Eq)

-- | Create a Procedure from String inputs

makeProcedure :: String -> String -> String -> Procedure
makeProcedure x y z = Procedure (makeLocation x) (makeCode y z)

-- | Lab types contain information about laboratory results

data Lab = Lab
  {   getLabLocation :: ServiceLocation
    , getLabCode     :: Code
    , getLabValue    :: Value } 
  deriving (Show, Eq)

-- | Create a Lab type from a String, Code and Value

makeLab :: String -> Code -> Value -> Lab
makeLab x = Lab (makeLocation x)

-- | Medication types contain information about medication events 
--   including dosage (as a Value)

data Medication = Medication
  {   getRXLocation  :: ServiceLocation
    , getRXCode      :: Code
    , getRXDose      :: Value }
   deriving (Show, Eq)

-- | Create a Medication type from a String, Code and Value

makeMedication :: String -> Code -> Value -> Medication
makeMedication x = Medication (makeLocation x)

-- | Insurance types contain information about insurance events

data Insurance = Insurance
  {   getPlan        :: String
    , getInsurer     :: String }
  deriving (Show, Eq)

-- | Create an Insurance type from a String

makeInsurance :: String -> String -> Insurance
makeInsurance = Insurance

-- | TODO

data EventDomain = 
    DmDomain Demographics
  | DxDomain Diagnosis
  | PrDomain Procedure
  | LbDomain Lab
  | RxDomain Medication
  | InDomain Insurance
  deriving (Eq, Show)



labDomain :: ServiceLocation -> Code -> Value -> EventDomain
labDomain x y z = LbDomain ( Lab x y z )






-- | TODO 

type EventContext = Context EventDomain


-- | TODO

eventContext :: EventDomain -> Source -> EventContext
eventContext = Context


-- TODO

newtype Events = Events (Seq Event)
  deriving (Show)

-- TODO

events :: [Event] -> Events
events l = Events $ Seq.fromList l
