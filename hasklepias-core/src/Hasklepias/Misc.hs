{-|
Module      : Misc types and functions 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Hasklepias.Misc
  ( Occurrence(..)
  , makeOccurrence
  , getOccurrenceReason
  , getOccurrenceTime
  , CensoringReason(..)
  , OccurrenceReason(..)
  , CensoredOccurrence(..)
  , adminCensor
  , Location(..)
  , readData
  , getS3Object
  ) where

import           Data.Bool                      ( (&&)
                                                , otherwise
                                                )
import           Data.Eq                        ( Eq(..) )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Maybe                     ( Maybe )
import           Data.Ord                       ( Ord(..)
                                                , Ordering(..)
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Features.Compose               ( Definition
                                                , Feature
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show(..) )
import           Stype.Numeric.Censored         ( MaybeCensored(..) )
import           Stype.Numeric.Continuous       ( EventTime )

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Conduit.Binary            ( sinkLbs )
import           GHC.IO
import           Lens.Micro                     ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Network.AWS
import           Network.AWS.S3
import           System.IO                      ( stderr )


-- | A simple typeclass for making a type a "reason" for an event.
class (Ord a, Show a) => OccurrenceReason a where

-- | A type containing the time and when something occurred
newtype Occurrence what when = MkOccurrence ( what , EventTime when )
  deriving (Eq, Show, Generic)

-- | Create an 'Occurrence'
makeOccurrence
  :: (OccurrenceReason what) => what -> EventTime b -> Occurrence what b
makeOccurrence r t = MkOccurrence (r, t)

-- | Get the reason for an 'Occurrence'.
getOccurrenceReason :: Occurrence what b -> what
getOccurrenceReason (MkOccurrence (r, t)) = r

-- | Get the time of an 'Occurrence'.
getOccurrenceTime :: Occurrence what b -> EventTime b
getOccurrenceTime (MkOccurrence (r, t)) = t

-- Define a custom ordering based on times and then reasons.
instance (OccurrenceReason r, Ord b) => Ord (Occurrence r b) where
  compare (MkOccurrence (r1, t1)) (MkOccurrence (r2, t2))
    | t1 < t2              = LT
    | t1 == t2 && r1 < r2  = LT
    | t1 == t2 && r1 == r2 = EQ
    | otherwise            = GT

-- | Sum type for possible censoring and outcome reasons, including administrative
--   censoring.
data CensoringReason cr or = AdminCensor | C cr | O or
  deriving (Eq, Show, Generic)

-- | A type to represent censored 'Occurrence'.
data CensoredOccurrence censors outcomes b = MkCensoredOccurrence
  { reason :: CensoringReason censors outcomes
  , time   :: MaybeCensored (EventTime b)
  }
  deriving (Eq, Generic)

instance (OccurrenceReason c, OccurrenceReason o, Show b) =>
  Show ( CensoredOccurrence c o b ) where
  show (MkCensoredOccurrence r t) = "(" <> show t <> ", " <> show r <> ")"

-- | Creates an administratively censored occurrence.
adminCensor :: EventTime b -> CensoredOccurrence c o b
adminCensor t = MkCensoredOccurrence AdminCensor (RightCensored t)


{--}

-- | Type representing locations that data can be read from
data Location where
  StdIn ::Location
  Local ::FilePath -> Location
  S3    ::Region -> BucketName -> ObjectKey -> Location

-- | Read data from a @Location@. 
readData :: Location -> IO B.ByteString
readData StdIn      = B.getContents
readData (Local x ) = B.readFile x
readData (S3 r b k) = getS3Object r b k

-- | Get an object from S3. 
getS3Object :: Region -> BucketName -> ObjectKey -> IO B.ByteString
getS3Object r b k = do
  lgr <- newLogger Debug stderr
  env <- newEnv Discover <&> set envLogger lgr . set envRegion r
  runResourceT . runAWS env $ do
    result <- send $ getObject b k
    (result ^. gorsBody) `sinkBody` sinkLbs
