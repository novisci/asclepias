{-|
Module      : Functions for Parsing Events from JSON lines
Description : Defines FromJSON instances for Events.
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
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

module EventDataTheory.EventLines
  ( EventLine
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
  , modifyEventLine

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

data EventLine d c a = MkEventLine Value Value Value Value [c] (FactsLine d a)
  deriving (Eq, Show, Generic)

instance (FromJSON a, Show a, IntervalSizeable a b
         , Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c)
          => FromJSON (EventLine d c a)

instance (Ord a, ToJSON a, ToJSON c, ToJSON d) => ToJSON (EventLine d c a)

-- INTERNAL utility for getting subjectID from EventLine
getSubjectID :: EventLine d c a -> SubjectID
getSubjectID (MkEventLine _ _ _ _ _ fcts) =
  (getSubjectIDLine . patient_id) fcts

-- INTERNAL utility for getting the FactsLine from EventLine
fctln :: EventLine d c a -> FactsLine d a
fctln (MkEventLine _ _ _ _ _ x) = x

-- INTERNAL utility for getting concepts from EventLine
cptsln :: EventLine d c a -> [c]
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
instance (Show a, IntervalSizeable a b
         , Show d, Eq d, Generic d
         , Show c, Eq c, Ord c, Typeable c) =>
         TryFrom (EventLine d c a, ParseEventLineOption) (Event d c a) where
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
data FactsLine d a = MkFactsLine
  { domain     :: Text
  , time       :: TimeLine a
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

instance (Ord a, ToJSON a, ToJSON d) => ToJSON (FactsLine d a) where
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
     , Typeable d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Typeable a
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => ParseEventLineOption
  -> B.ByteString
  -> Either String (SubjectID, Event d c a)
eitherDecodeEvent opt = makeEventDecoder show opt eitherDecode
eitherDecodeEvent' opt = makeEventDecoder show opt eitherDecode'

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
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Typeable a
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => ParseEventLineOption
  -> B.ByteString
  -> Maybe (SubjectID, Event d c a)
decodeEvent opt x = rightToMaybe $ eitherDecodeEvent opt x
decodeEvent' opt x = rightToMaybe $ eitherDecodeEvent' opt x

{-|
Decode a strict bytestring corresponding to an 'EventLine' into
@Maybe (SubjectID, Event d c a)@,
where the value is @Nothing@ on failure
and @Just (SubjectID, Event d c a)@ on success.

NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}
decodeEventStrict, decodeEventStrict'
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Typeable a
     , Show a
     , IntervalSizeable a b
     )
  => ParseEventLineOption
  -> C.ByteString
  -> Maybe (SubjectID, Event d c a)
decodeEventStrict opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict x
decodeEventStrict' opt x =
  rightToMaybe $ makeEventDecoderStrict show opt eitherDecodeStrict' x

makeEventDecoder
  :: forall d c a b e
   . ( Show d
     , Eq d
     , Generic d
     , Typeable d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Typeable a
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => (  TryFromException (EventLine d c a, ParseEventLineOption) (Event d c a)
     -> e
     )
  -> ParseEventLineOption
  -> (B.ByteString -> Either e (EventLine d c a))
  -> (B.ByteString -> Either e (SubjectID, Event d c a))
makeEventDecoder g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event d c a) (eline, opt)
  pure (getSubjectID eline, tryev)

makeEventDecoderStrict
  :: forall d c a b e
   . ( Show d
     , Eq d
     , Generic d
     , Typeable d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Typeable a
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => (  TryFromException (EventLine d c a, ParseEventLineOption) (Event d c a)
     -> e
     )
  -> ParseEventLineOption
  -> (C.ByteString -> Either e (EventLine d c a))
  -> (C.ByteString -> Either e (SubjectID, Event d c a))
makeEventDecoderStrict g opt f x = do
  eline <- f x
  tryev <- first g $ tryInto @(Event d c a) (eline, opt)
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
  => (B.ByteString -> Either String (SubjectID, Event d c a))
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event d c a)])
makeLineParser f l = partitionEithers $ zipWith
  (\x i -> first (\t -> MkLineParseError (i, t)) (f x))
  (B.lines l)
  [1 ..]

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
parseEventLinesL, parseEventLinesL'
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , Typeable d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Typeable a
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => ParseEventLineOption
  -> B.ByteString
  -> ([LineParseError], [(SubjectID, Event d c a)])
parseEventLinesL opt = makeLineParser (eitherDecodeEvent opt)
parseEventLinesL' opt = makeLineParser (eitherDecodeEvent' opt)

{-------------------------------------------------------------------------------
Transforming event lines
-------------------------------------------------------------------------------}
{-
INTERNAL

Used in modifyEventLine to
modfiy a @FactLines@ value with values from a @Context@.
Only those fields in the context that align with the factsline
are modified.
-}
updateFactsLine :: (Data d') => FactsLine d a -> Context d' c -> FactsLine d' a
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

Transforms an Eventline via a function 
that operates on the Context 
within the Event corresponding to the EventLine.
-}
modifyEventLineContext
  :: forall d d' c c' a b e
   . ( FromJSON a
     , Show a
     , IntervalSizeable a b
     , Typeable a
     , Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Ord c'
     , Data d'
     )
  => ParseEventLineOption
  -> (Context d c -> Context d' c')
  -> EventLine d c a
  -> Either String (EventLine d' c' a)
modifyEventLineContext opt g (MkEventLine a b c d e f) = do
  ev <- first show $ tryInto @(Event d c a) (MkEventLine a b c d e f, opt)
  let ctxt  = g (getContext ev)
  let newFl = updateFactsLine f ctxt
  pure $ MkEventLine a b c d (into . getConcepts $ ctxt) newFl

{-|
-}
modifyEventLine
  :: forall d d' c c' a b m
   . ( FromJSON a
     , Show a
     , IntervalSizeable a b
     , FromJSON c
     , FromJSON d
     , Typeable a
     , Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , Ord c'
     , Data d'
     )
  => ParseEventLineOption
  -> (Context d c -> Context d' c')
  -> B.ByteString
  -> Either String (EventLine d' c' a)
modifyEventLine opt f x =
  let el = eitherDecode @(EventLine d c a) x
  in  modifyEventLineContext opt f =<< el
