{-|
Module      : Functions for Parsing Events from JSON lines
Description : Defines FromJSON instances for Events.
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module EventDataTheory.EventLines
  ( EventLine
  , EventLineAble
  , parseEventLinesL
  , parseEventLinesL'
  , eitherDecodeEvent
  , eitherDecodeEvent'
  , decodeEvent
  , decodeEvent'
  , decodeEventStrict
  , decodeEventStrict'
  , LineParseError
  , ParseEventLineOption(..)
  , defaultParseEventLineOption
  , modifyEventLineWithContext

  -- for internal use;
  , SubjectIDLine
  , FactsLine
  , TimeLine
  ) where

import           Control.Exception
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Data
import           Data.Either                (Either (..), partitionEithers)
import           Data.Scientific            (floatingOrInteger)
import           Data.Text                  (Text, pack)
import           EventDataTheory.Core
import           GHC.Generics               (Generic)
import           GHC.Num                    (Integer, Natural)
import           IntervalAlgebra            (Interval,
                                             IntervalSizeable (add, diff, moment),
                                             ParseErrorInterval, begin,
                                             beginerval, end, getInterval,
                                             parseInterval)
import           Type.Reflection            (Typeable)
import           Witch
{-|
At this time,
'EventLine', 'FactsLine', 'SubjectIDLine', and 'TimeLine' are
simply wrapper types
in order to create 'FromJSON' instances which can be used to marshal data from
[ndjson](http://ndjson.org/).
See [event data model docs](https://docs.novisci.com/event-data/3.0/index.html)
'ToJSON' instances are not provided, but may be in the future.
-}

data EventLine t m a = MkEventLine Value Value Value Value [t] (FactsLine m a)
  deriving (Eq, Show, Generic)

instance (Eventable t m a, FromJSONEvent t m a, IntervalSizeable a b)
          => FromJSON (EventLine t m a)

instance (Ord a, ToJSON a, ToJSON t, ToJSON m) => ToJSON (EventLine t m a)

-- | A synonym for the basic class constraints needed to create an @EventLine@.
type EventLineAble t m a b
  = (Generic m, Typeable m, Typeable t, Typeable a, IntervalSizeable a b)

-- INTERNAL utility for getting subjectID from EventLine
getSubjectID :: EventLine t m a -> SubjectID
getSubjectID (MkEventLine _ _ _ _ _ fcts) =
  (getSubjectIDLine . patient_id) fcts

-- INTERNAL utility for getting the FactsLine from EventLine
fctln :: EventLine t m a -> FactsLine m a
fctln (MkEventLine _ _ _ _ _ x) = x

-- INTERNAL utility for getting a tag set from EventLine
tagSetIn :: EventLine t m a -> [t]
tagSetIn (MkEventLine _ _ _ _ x _) = x

{-|
Options for how an 'EventLine' will be parsed into an 'Event'.
-}
data ParseEventLineOption =
  -- | Before trying to 'IntervalAlgebra.parseInterval':
  --   * If the end of the @TimeLine@ in the @EventLine@ is missing,
  --   then create the event's interval
  --   as a moment from the begin of the @TimeLine@.
  --   * Otherwise, add a moment to the end of the @TimeLine@.
    AddMomentToTimeEnd
  -- | Do not modify the @TimeLine@ before
  --   trying to 'IntervalAlgebra.parseInterval'.
  | DoNotModifyTime
  deriving (Show)

-- | The default 'ParseEventLineOption'.
defaultParseEventLineOption :: ParseEventLineOption
defaultParseEventLineOption = AddMomentToTimeEnd

instance Exception ParseErrorInterval

{-|
Try to parse an @'EventLine'@ into an @'Event'@,
given an 'ParseEventLineOption'.
-}
instance ( Eventable t m a, EventLineAble t m a b ) =>
  TryFrom (EventLine t m a, ParseEventLineOption) (Event t m a) where
  tryFrom x = do
    let fcts = (fctln . fst) x
    let i    = time fcts
    let tag  = (tagSetIn . fst) x
    let toEvent = \case
          Left  err  -> Left $ TryFromException x (Just $ toException err)
          Right itrv -> Right
            $ event itrv (context (from @[t] tag) (facts fcts) (source fcts))
    case snd x of
      AddMomentToTimeEnd -> do
        let ei = parseInterval
              (timeBegin i)
              (maybe (add (moment @a) (timeBegin i))
                     (add (moment @a))
                     (timeEnd i)
              )
        toEvent ei
      DoNotModifyTime -> do
        case timeEnd i of
          Nothing -> Left $ TryFromException x Nothing
          Just z  -> toEvent (parseInterval (timeBegin i) z)

-- | See 'EventLine'.
data FactsLine m a = MkFactsLine
  { domain     :: Text
  , time       :: TimeLine a
  , facts      :: m
  , patient_id :: SubjectIDLine
  , source     :: Maybe Source
  , valid      :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a, Show a, IntervalSizeable a b, Show m, Eq m, FromJSON m)
          => FromJSON (FactsLine m a) where
  parseJSON = withObject "Facts Blob" $ \o -> do
    dmn <- o .: "domain"
    fct <- parseJSON (Object o)
    itv <- o .: "time"
    pid <- parseJSON (Object o)
    vld <- o .:? "valid"
    src <- o .:? "source"
    pure $ MkFactsLine dmn itv fct pid src vld

instance (Ord a, ToJSON a, ToJSON m) => ToJSON (FactsLine m a) where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

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

instance ToJSON SubjectIDLine where
  toJSON (MkSubjectIDLine x) = into x

-- | See 'EventLine'
data TimeLine a = MkTimeLine
  { timeBegin :: a
  , timeEnd   :: Maybe a
  }
  deriving (Eq, Show)

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (TimeLine a) where
  parseJSON = withObject "Time" $ \o -> do
    b <- o .: "begin"
    e <- o .:? "end"
    pure (MkTimeLine b e)

instance (Ord a, ToJSON a) => ToJSON (TimeLine a) where
  toJSON (MkTimeLine b e) = case e of
    Nothing -> object ["begin" .= b]
    Just x  -> object ["begin" .= b, "end" .= x]

{-|
Decode a bytestring corresponding to an 'EventLine' into
@Either String (SubjectID, Event t m a)@,
where the @String@ is an error message on failure
and @(SubjectID, Event t m a)@ is the success case.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22
for discusson of json vs json'.
-}
eitherDecodeEvent, eitherDecodeEvent'
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => ParseEventLineOption
  -> B.ByteString
  -> Either String (SubjectID, Event t m a)
eitherDecodeEvent opt = makeEventDecoder show opt eitherDecode
eitherDecodeEvent' opt = makeEventDecoder show opt eitherDecode'

{-|
Decode a bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event t m a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event t m a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22
for discusson of json vs json'.
-}
decodeEvent, decodeEvent'
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => ParseEventLineOption
  -> B.ByteString
  -> Maybe (SubjectID, Event t m a)
decodeEvent opt x = rightToMaybe $ eitherDecodeEvent opt x
decodeEvent' opt x = rightToMaybe $ eitherDecodeEvent' opt x

{-|
Decode a strict bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event t m a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event t m a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22
for discusson of json vs json'.
-}
decodeEventStrict, decodeEventStrict'
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => ParseEventLineOption
  -> C.ByteString
  -> Maybe (SubjectID, Event t m a)
decodeEventStrict opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict x
decodeEventStrict' opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict' x

makeEventDecoder
  :: forall m t a b e
   . (Eventable t m a, EventLineAble t m a b)
  => (  TryFromException (EventLine t m a, ParseEventLineOption) (Event t m a)
     -> e
     )
  -> ParseEventLineOption
  -> (B.ByteString -> Either e (EventLine t m a))
  -> (B.ByteString -> Either e (SubjectID, Event t m a))
makeEventDecoder g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event t m a) (eline, opt)
  pure (getSubjectID eline, tryev)

makeEventDecoderStrict
  :: forall m t a b e
   . (Eventable t m a, EventLineAble t m a b)
  => (  TryFromException (EventLine t m a, ParseEventLineOption) (Event t m a)
     -> e
     )
  -> ParseEventLineOption
  -> (C.ByteString -> Either e (EventLine t m a))
  -> (C.ByteString -> Either e (SubjectID, Event t m a))
makeEventDecoderStrict g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event t m a) (eline, opt)
  pure (getSubjectID eline, tryev)

-- INTERNAL utlity for transforming an @Either@ into a @Maybe@
rightToMaybe :: Either b a -> Maybe a
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x

{-|
Contains the line number and error message of any parsing errors.
-}
newtype LineParseError = MkLineParseError (Natural, String)
  deriving (Eq, Show, Generic)

-- providing a From instance making LineParseError values in the tests
instance From (Natural, String) LineParseError where

-- internal for create line parsers
makeLineParser
  :: forall m t a b
   . (Eventable t m a, FromJSONEvent t m a, Typeable m, IntervalSizeable a b)
  => (B.ByteString -> Either String (SubjectID, Event t m a))
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event t m a)])
makeLineParser f l = partitionEithers $ zipWith
  (\x i -> first (\t -> MkLineParseError (i, t)) (f x))
  (B.lines l)
  [1 ..]

{-|
Parse @Event t m a@ from new-line delimited JSON.

Per the [aeson docs](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22),
when using this version:
This is a strict version of json which avoids building up thunks during parsing;
it performs all conversions immediately.
Prefer this version if most of the JSON data needs to be accessed.

Returns a pair where
the first element is a list of parse errors
and the second element is a list of successfully parsed (subjectID, event) pairs.
-}
parseEventLinesL, parseEventLinesL'
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => ParseEventLineOption
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event t m a)])
parseEventLinesL opt = makeLineParser (eitherDecodeEvent opt)
parseEventLinesL' opt = makeLineParser (eitherDecodeEvent' opt)

{-------------------------------------------------------------------------------
Transforming event lines
-------------------------------------------------------------------------------}
{-
INTERNAL

Modify a @FactLines@ value with values from a @Context@.
The @TimeLine@ value IS NOT changed.
Only those fields in the context that align with the factsline
are modified.
-}
updateFactsLine :: (Data m') => FactsLine m a -> Context t m' -> FactsLine m' a
updateFactsLine (MkFactsLine dmn tm _ sid _ vld) x = MkFactsLine
  { domain     = pack $ show $ toConstr (getFacts x)
  , time       = tm
  , facts      = getFacts x
  , patient_id = sid
  , source     = getSource x
  , valid      = vld
  }

{-
INTERNAL

Modify a @FactLines@ value with values from a @Context@.
The @TimeLine@ value IS NOT changed
based on the provided interval.
Only those fields in the context that align with the factsline
are modified.
-}
updateFactsLineWithInterval
  :: (Data m', Ord a')
  => FactsLine m a
  -> Context t m'
  -> Interval a'
  -> FactsLine m' a'
updateFactsLineWithInterval (MkFactsLine _ _ _ sid _ vld) x i = MkFactsLine
  { domain     = pack $ show $ toConstr (getFacts x)
  , time       = MkTimeLine (begin i) (Just $ end i)
  , facts      = getFacts x
  , patient_id = sid
  , source     = getSource x
  , valid      = vld
  }

{-
INTERNAL

Modifies data in an @EventLine@
from data in an @Event@.
-}
updateEventLineFromEvent
  :: (Data m', Ord a', ToJSON a', Ord t')
  => EventLine t m a
  -> Event t' m' a'
  -> EventLine t' m' a'
updateEventLineFromEvent (MkEventLine _ _ _ _ _ f) x =
  let ctxt = getContext x
  in  let i = getInterval x
      in  MkEventLine (toJSON (patient_id f))
                      (toJSON (begin i))
                      (toJSON (Just . end $ i))
                      (toJSON (show . toConstr . getFacts $ ctxt))
                      (into (getTagSet ctxt))
                      (updateFactsLineWithInterval f ctxt i)

{-
INTERNAL

Transforms an Eventline via a function
that operates on the Context
within the Event corresponding to the EventLine.
-}
eitherModifyEventLineFromContext
  :: forall m m' t t' a b e
   . ( Eventable t m a
     , EventLineAble t m a b
     , FromJSONEvent t m a
     , Ord t'
     , Data m'
     )
  => ParseEventLineOption
  -> (Context t m -> Context t' m')
  -> EventLine t m a
  -> Either String (EventLine t' m' a)
eitherModifyEventLineFromContext opt g (MkEventLine a b t m e f) = do
  ev <- first show $ tryInto @(Event t m a) (MkEventLine a b t m e f, opt)
  let ctxt  = g (getContext ev)
  let newFl = updateFactsLine f ctxt
  pure $ MkEventLine a b t m (into . getTagSet $ ctxt) newFl

{-
TODO
-}
eitherModifyEventLineFromEvent
  :: forall m m' t t' a a' b e
   . ( Eventable t m a
     , Eventable t' m' a'
     , EventLineAble t m a b
     , FromJSONEvent t m a
     , ToJSON a'
     , Data m'
     )
  => ParseEventLineOption
  -> (Event t m a -> Event t' m' a')
  -> EventLine t m a
  -> Either String (EventLine t' m' a')
eitherModifyEventLineFromEvent opt g x = do
  ev1 <- first show $ tryInto @(Event t m a) (x, opt)
  let ev2 = g ev1
  pure $ updateEventLineFromEvent x ev2


{-|
This function:

* parses a JSON bytestring into an 'EventLine'
* modifies the data in the 'EventLine'
that corresponds to an 'Event' 'Context'
using the supplied function

The function may fail and return an error message
if either the JSON parsing fails
or the 'EventLine' -> 'Event' tranformation fails.

This function does not modify the time information in the 'EventLine',
nor any of the first four elements of the 'EventLine'.

See 'modifyEventLineWithEvent' for a function that can also modify the interval.
-}
modifyEventLineWithContext
  :: forall m m' t t' a b
   . ( Eventable t m a
     , EventLineAble t m a b
     , FromJSONEvent t m a
     , Eventable t' m' a
     , Data m'
     )
  => ParseEventLineOption
  -> (Context t m -> Context t' m')
  -> B.ByteString
  -> Either String (EventLine t' m' a)
modifyEventLineWithContext opt f x =
  let el = eitherDecode @(EventLine t m a) x
  in  eitherModifyEventLineFromContext opt f =<< el

{-
NOT EXPORTED AT THIS TIME
HERE FOR FURTHER CONSIDERATION

This function:

* parses a JSON bytestring into an 'EventLine'
* modifies the data in the 'EventLine'
that corresponds to an 'Event'
using the supplied function

The function may fail and return an error message
if either the JSON parsing fails
or the 'EventLine' -> 'Event' tranformation fails.

This function may modify time information in the 'EventLine',
thus cannot be used to roundtrip to/from JSON isomorphically.
For example, if the end of an interval is missing in the input JSON,
the output will contain an interval end if the 'AddMomentToTimeEnd'
parse option was used.

Therefore, USER BEWARE.

-}
modifyEventLineWithEvent
  :: forall m m' t t' a a' b
   . ( FromJSONEvent t m a
     , Eventable t m a
     , Eventable t' m' a'
     , EventLineAble t m a b
     , ToJSON a'
     , Data m'
     )
  => ParseEventLineOption
  -> (Event t m a -> Event t' m' a')
  -> B.ByteString
  -> Either String (EventLine t' m' a')
modifyEventLineWithEvent opt f x =
  let el = eitherDecode @(EventLine t m a) x
  in  eitherModifyEventLineFromEvent opt f =<< el
