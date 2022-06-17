{-|
Module      : Functions for Parsing Events from JSON lines
Description : Defines FromJSON instances for Events.
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

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
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Data
import           Data.Either                    ( Either(..)
                                                , partitionEithers
                                                )
import           Data.Scientific                ( floatingOrInteger )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           EventDataTheory.Core
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Integer
                                                , Natural
                                                )
import           IntervalAlgebra                ( Interval
                                                , IntervalSizeable
                                                  ( add
                                                  , diff
                                                  , moment
                                                  )
                                                , ParseErrorInterval
                                                , begin
                                                , beginerval
                                                , end
                                                , getInterval
                                                , parseInterval
                                                )
import           Type.Reflection                ( Typeable )
import           Witch
{-|
At this time, 
'EventLine', 'FactsLine', 'SubjectIDLine', and 'TimeLine' are
simply wrapper types
in order to create 'FromJSON' instances which can be used to marshal data from 
[ndjson](http://ndjson.org/).
See [event data model docs](https://docs.novisci.com/edm-sandbox/latest/index.html#_event_representation)
'ToJSON' instances are not provided, but may be in the future. 
-}

data EventLine c m a = MkEventLine Value Value Value Value [c] (FactsLine m a)
  deriving (Eq, Show, Generic)

instance (Eventable c m a, FromJSONEvent c m a, IntervalSizeable a b)
          => FromJSON (EventLine c m a)

instance (Ord a, ToJSON a, ToJSON c, ToJSON m) => ToJSON (EventLine c m a)

-- | A synonym for the basic class constraints needed to create an @EventLine@.
type EventLineAble c m a b
  = (Generic m, Typeable m, Typeable c, Typeable a, IntervalSizeable a b)

-- INTERNAL utility for getting subjectID from EventLine
getSubjectID :: EventLine c m a -> SubjectID
getSubjectID (MkEventLine _ _ _ _ _ fcts) =
  (getSubjectIDLine . patient_id) fcts

-- INTERNAL utility for getting the FactsLine from EventLine
fctln :: EventLine c m a -> FactsLine m a
fctln (MkEventLine _ _ _ _ _ x) = x

-- INTERNAL utility for getting concepts from EventLine
cptsln :: EventLine c m a -> [c]
cptsln (MkEventLine _ _ _ _ x _) = x

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
instance ( Eventable c m a, EventLineAble c m a b ) =>
  TryFrom (EventLine c m a, ParseEventLineOption) (Event c m a) where
  tryFrom x = do
    let fcts = (fctln . fst) x
    let i    = time fcts
    let cpts = (cptsln . fst) x
    let toEvent = \case
          Left  err  -> Left $ TryFromException x (Just $ toException err)
          Right itrv -> Right
            $ event itrv (context (from @[c] cpts) (facts fcts) (source fcts))
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
@Either String (SubjectID, Event c m a)@,
where the @String@ is an error message on failure
and @(SubjectID, Event c m a)@ is the success case.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
eitherDecodeEvent, eitherDecodeEvent'
  :: forall m c a b
   . (Eventable c m a, EventLineAble c m a b, FromJSONEvent c m a)
  => ParseEventLineOption
  -> B.ByteString
  -> Either String (SubjectID, Event c m a)
eitherDecodeEvent opt = makeEventDecoder show opt eitherDecode
eitherDecodeEvent' opt = makeEventDecoder show opt eitherDecode'

{-|
Decode a bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event c m a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event c m a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
decodeEvent, decodeEvent'
  :: forall m c a b
   . (Eventable c m a, EventLineAble c m a b, FromJSONEvent c m a)
  => ParseEventLineOption
  -> B.ByteString
  -> Maybe (SubjectID, Event c m a)
decodeEvent opt x = rightToMaybe $ eitherDecodeEvent opt x
decodeEvent' opt x = rightToMaybe $ eitherDecodeEvent' opt x

{-|
Decode a strict bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event c m a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event c m a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
decodeEventStrict, decodeEventStrict'
  :: forall m c a b
   . (Eventable c m a, EventLineAble c m a b, FromJSONEvent c m a)
  => ParseEventLineOption
  -> C.ByteString
  -> Maybe (SubjectID, Event c m a)
decodeEventStrict opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict x
decodeEventStrict' opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict' x

makeEventDecoder
  :: forall m c a b e
   . (Eventable c m a, EventLineAble c m a b)
  => (  TryFromException (EventLine c m a, ParseEventLineOption) (Event c m a)
     -> e
     )
  -> ParseEventLineOption
  -> (B.ByteString -> Either e (EventLine c m a))
  -> (B.ByteString -> Either e (SubjectID, Event c m a))
makeEventDecoder g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event c m a) (eline, opt)
  pure (getSubjectID eline, tryev)

makeEventDecoderStrict
  :: forall m c a b e
   . (Eventable c m a, EventLineAble c m a b)
  => (  TryFromException (EventLine c m a, ParseEventLineOption) (Event c m a)
     -> e
     )
  -> ParseEventLineOption
  -> (C.ByteString -> Either e (EventLine c m a))
  -> (C.ByteString -> Either e (SubjectID, Event c m a))
makeEventDecoderStrict g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event c m a) (eline, opt)
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
  :: forall m c a b
   . (Eventable c m a, FromJSONEvent c m a, Typeable m, IntervalSizeable a b)
  => (B.ByteString -> Either String (SubjectID, Event c m a))
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event c m a)])
makeLineParser f l = partitionEithers $ zipWith
  (\x i -> first (\t -> MkLineParseError (i, t)) (f x))
  (B.lines l)
  [1 ..]

{-| 
Parse @Event c m a@ from new-line delimited JSON.

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
  :: forall m c a b
   . (Eventable c m a, EventLineAble c m a b, FromJSONEvent c m a)
  => ParseEventLineOption
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event c m a)])
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
updateFactsLine :: (Data m') => FactsLine m a -> Context c m' -> FactsLine m' a
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
  -> Context c m'
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
  :: (Data m', Ord a', ToJSON a', Ord c')
  => EventLine c m a
  -> Event c' m' a'
  -> EventLine c' m' a'
updateEventLineFromEvent (MkEventLine _ _ _ _ _ f) x =
  let ctxt = getContext x
  in  let i = getInterval x
      in  MkEventLine (toJSON (patient_id f))
                      (toJSON (begin i))
                      (toJSON (Just . end $ i))
                      (toJSON (show . toConstr . getFacts $ ctxt))
                      (into (getConcepts ctxt))
                      (updateFactsLineWithInterval f ctxt i)

{-
INTERNAL

Transforms an Eventline via a function 
that operates on the Context 
within the Event corresponding to the EventLine.
-}
eitherModifyEventLineFromContext
  :: forall m m' c c' a b e
   . ( Eventable c m a
     , EventLineAble c m a b
     , FromJSONEvent c m a
     , Ord c'
     , Data m'
     )
  => ParseEventLineOption
  -> (Context c m -> Context c' m')
  -> EventLine c m a
  -> Either String (EventLine c' m' a)
eitherModifyEventLineFromContext opt g (MkEventLine a b c m e f) = do
  ev <- first show $ tryInto @(Event c m a) (MkEventLine a b c m e f, opt)
  let ctxt  = g (getContext ev)
  let newFl = updateFactsLine f ctxt
  pure $ MkEventLine a b c m (into . getConcepts $ ctxt) newFl

{-
TODO
-}
eitherModifyEventLineFromEvent
  :: forall m m' c c' a a' b e
   . ( Eventable c m a
     , Eventable c' m' a'
     , EventLineAble c m a b
     , FromJSONEvent c m a
     , ToJSON a'
     , Data m'
     )
  => ParseEventLineOption
  -> (Event c m a -> Event c' m' a')
  -> EventLine c m a
  -> Either String (EventLine c' m' a')
eitherModifyEventLineFromEvent opt g x = do
  ev1 <- first show $ tryInto @(Event c m a) (x, opt)
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
  :: forall m m' c c' a b
   . ( Eventable c m a
     , EventLineAble c m a b
     , FromJSONEvent c m a
     , Eventable c' m' a
     , Data m'
     )
  => ParseEventLineOption
  -> (Context c m -> Context c' m')
  -> B.ByteString
  -> Either String (EventLine c' m' a)
modifyEventLineWithContext opt f x =
  let el = eitherDecode @(EventLine c m a) x
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
  :: forall m m' c c' a a' b
   . ( FromJSONEvent c m a
     , Eventable c m a
     , Eventable c' m' a'
     , EventLineAble c m a b
     , ToJSON a'
     , Data m'
     )
  => ParseEventLineOption
  -> (Event c m a -> Event c' m' a')
  -> B.ByteString
  -> Either String (EventLine c' m' a')
modifyEventLineWithEvent opt f x =
  let el = eitherDecode @(EventLine c m a) x
  in  eitherModifyEventLineFromEvent opt f =<< el
