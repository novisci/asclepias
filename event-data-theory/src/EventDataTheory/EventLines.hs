{-|
Module      : Functions for Parsing Events from JSON lines
Description : Defines FromJSON instances for Events.
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventDataTheory.EventLines
  ( EventLine
  , parseEventLinesL
  , parseEventLinesL'
  , eitherDecodeEvent
  , eitherDecodeEvent'
  , decodeEvent
  , decodeEvent'
  ) where

import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Monad                  ( Functor(fmap)
                                                , MonadFail(fail)
                                                )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Either                    ( Either(..)
                                                , partitionEithers
                                                )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Maybe                     ( Maybe
                                                , maybe
                                                )
import           Data.Ord                       ( Ord )
import           Data.Scientific                ( floatingOrInteger )
import           Data.String                    ( String )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Vector                    ( (!) )
import           EventDataTheory.Core
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Integer )
import           GHC.Show                       ( Show(..) )
import           IntervalAlgebra                ( Interval
                                                , IntervalSizeable
                                                  ( add
                                                  , diff
                                                  , moment
                                                  )
                                                , beginerval
                                                , parseInterval
                                                )
import           Type.Reflection                ( Typeable )
import           Witch

{-|
At this time, 
'EventLine', 'FactsLine', 'SubjectIDLine', and 'IntervalLine' are
simply wrapper types
in order to create 'FromJSON' instances which can be used to marshal data from 
[ndjson](http://ndjson.org/).
See [event data model docs](https://docs.novisci.com/edm-sandbox/latest/index.html#_event_representation)
'ToJSON' instances are not provided, but may be in the future. 
-}

data EventLine d c a = MkEventLine Value a (Maybe a) Text [c] (FactsLine d a)
  deriving (Eq, Show, Generic)

getSubjectID :: EventLine d c a -> SubjectID
getSubjectID (MkEventLine _ _ _ _ _ fcts) =
  (getSubjectIDLine . patient_id) fcts

instance (FromJSON a, Show a, IntervalSizeable a b
         , Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c)
          => FromJSON (EventLine d c a) where

instance (FromJSON a, Show a, IntervalSizeable a b
         , Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c) => From (EventLine d c a) (Event d c a) where
  from (MkEventLine _ _ _ _ cpts fi) = event
    (getIntervalLine $ time fi)
    (MkContext { getConcepts = from @[c] cpts
             , getFacts    = facts fi
             , getSource   = source fi
             }
    )

-- | See 'EventLine'.
data FactsLine d a = MkFactsLine
  { domain     :: Text
  , time       :: IntervalLine a
  , facts      :: d
  , patient_id :: SubjectIDLine
  , source     :: Maybe Source
  , valid      :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a, Show a, IntervalSizeable a b
         , Show d, Eq d, FromJSON d)
          => FromJSON (FactsLine d a) where
  parseJSON = withObject "Facts Blob" $ \o -> do
    dmn <- o .: "domain"
    fct <- parseJSON (Object o)
    itv <- o .: "time"
    pid <- parseJSON (Object o)
    vld <- o .:? "valid"
    src <- o .:? "source"
    pure $ MkFactsLine dmn itv fct pid src vld


-- | See 'EventLine'.
newtype SubjectIDLine = MkSubjectIDLine {getSubjectIDLine  :: SubjectID }
  deriving (Eq, Show)

instance FromJSON SubjectIDLine where
  parseJSON = withObject "patient ID" $ \o -> do
    z <- o .: "patient_id"
    case z of
      String x -> pure $ MkSubjectIDLine $ from x
      Number x -> case floatingOrInteger x of
        Left  _ -> fail "SubjectID number is not an integer"
        Right i -> pure $ MkSubjectIDLine $ from @Integer i
      _ -> fail (show z)

-- | See 'EventLine'
newtype IntervalLine a = MkIntervalLine { getIntervalLine :: Interval a }
  deriving (Eq, Show)

{-|
Parses the @time@ JSON object.
NOTE: a @'moment is always added to the 'IntervalAlgebra.Core.end'.
Moreover, in the case that the end is missing, a moment is created.
-}
instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (IntervalLine a) where
  parseJSON = withObject "Time" $ \o -> do
    b <- o .: "begin"
    e <- o .:? "end"
    let e2 = maybe (add (moment @a) b) (add (moment @a)) e
    let ei = parseInterval b e2
    case ei of
      Left  e -> fail (show e)
      Right i -> pure (MkIntervalLine i)

{-|
Decode a bytestring corresponding to an 'EventLine' into
@Either String (SubjectID, Event d c a)@,
where the @String@ is an error message on failure
and @(SubjectID, Event d c a)@ is the success case.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
eitherDecodeEvent, eitherDecodeEvent'
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => B.ByteString
  -> Either String (SubjectID, Event d c a)
eitherDecodeEvent = makeEventDecoder eitherDecode
eitherDecodeEvent' = makeEventDecoder eitherDecode'

{-|
Decode a bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event d c a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event d c a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
decodeEvent, decodeEvent'
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => B.ByteString
  -> Maybe (SubjectID, Event d c a)
decodeEvent' = makeEventDecoder decode'
decodeEvent = makeEventDecoder decode

makeEventDecoder
  :: ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     , Functor f
     )
  => (B.ByteString -> f (EventLine d c a))
  -> (B.ByteString -> f (SubjectID, Event d c a))
makeEventDecoder f = fmap (\x -> (getSubjectID x, into x)) . f

{-| 
Parse @Event d c a@ from new-line delimited JSON.

Per the [aeson docs](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22),
when using this version: 
This is a strict version of json which avoids building up thunks during parsing; 
it performs all conversions immediately.
Prefer this version if most of the JSON data needs to be accessed.

Returns a pair where
the first element is a list of parse errors
and the second element is a list of successfully parsed (subjectID, event) pairs.
-}
parseEventLinesL
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => B.ByteString
  -> ([String], [(SubjectID, Event d c a)])
parseEventLinesL l = partitionEithers $ fmap eitherDecodeEvent (B.lines l)

{-| 
Parse @Event d c a@ from new-line delimited JSON.

Per the [aeson docs](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22),
when using this version: 
The conversion of a parsed value to a Haskell value is deferred until the Haskell value is needed.
This may improve performance if only a subset of the results of conversions are needed,
but at a cost in thunk allocation. 

Returns a pair where
the first element is a list of parse errors
and the second element is a list of successfully parsed (subjectID, event) pairs.
-}
parseEventLinesL'
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => B.ByteString
  -> ([String], [(SubjectID, Event d c a)])
parseEventLinesL' l = partitionEithers $ fmap eitherDecodeEvent' (B.lines l)
