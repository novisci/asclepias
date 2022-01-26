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

module EventDataTheory.EventLines
  ( parseEventLinesL
  , parseEventLinesL'
  , EventLine
  , eitherDecodeEvent
  , eitherDecodeEvent'
  , decodeEvent
  , decodeEvent'
  ) where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( Functor(fmap)
                                                , MonadFail(fail)
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , Value(Array)
                                                , decode
                                                , decode'
                                                , eitherDecode
                                                , eitherDecode'
                                                , withArray
                                                , withObject
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
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


{-|
At this time, 'EventLine', 'ContextLine', and 'IntervalLine' are simply wrapper types
in order to create 'FromJSON' instances which can be used to marshal data from 
[ndjson](http://ndjson.org/).
See [event data model docs](https://docs.novisci.com/edm-sandbox/latest/index.html#_event_representation)
'ToJSON' instances are not provided, but may be in the future. 
-}
data EventLine d c a = MkEventLine
  { getSubjectID :: SubjectID
  , getEventLine :: Event d c a
  }
  deriving (Eq, Show)

instance (FromJSON a, Show a, IntervalSizeable a b
         , Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c)
          => FromJSON (EventLine d c a) where
  parseJSON = withArray "Event" $ \a -> do
    sid    <- parseJSON (a ! 0)
    intrvl <- parseJSON (a ! 5)
    let i = getIntervalLine intrvl
    c <- parseJSON (Array a)

    pure $ MkEventLine sid (event i (getContextLine c))

-- | See 'EventLine'.
newtype ContextLine d c  = MkContextLine { getContextLine :: Context d c }
  deriving (Eq, Show)

instance ( Ord c, FromJSON c, FromJSON d ) => FromJSON (ContextLine d c) where
  parseJSON = withArray "Context" $ \a -> do
    cpts <- parseJSON (a ! 4)
    fcts <- parseJSON (a ! 5)
    srce <- withObject "source" (.:? "source") (a ! 5)
    pure $ MkContextLine $ Context cpts fcts srce

-- | See 'IntervalLine'
newtype IntervalLine a = MkIntervalLine { getIntervalLine :: Interval a }
  deriving (Eq, Show)

{-|
Parses the @time@ JSON object.
NOTE: a @'moment
In the case that the end is missing, a moment is created.
-}
instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (IntervalLine a) where
  parseJSON = withObject "Time" $ \o -> do
    t <- o .: "time"
    b <- t .: "begin"
    e <- t .:? "end"
    let e2 = maybe (add (moment @a) b) (add (moment @a)) e
    let ei = parseInterval b e2
    case ei of
      Left  e -> fail (show e)
      Right i -> pure (MkIntervalLine i)

{-
NOTE: See https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#g:22 
for discusson of json vs json'.
-}

eitherDecodeEvent, eitherDecodeEvent'
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
     )
  => B.ByteString
  -> Either String (SubjectID, Event d c a)
eitherDecodeEvent = makeEventDecoder eitherDecode
eitherDecodeEvent' = makeEventDecoder eitherDecode'

decodeEvent, decodeEvent'
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
     )
  => B.ByteString
  -> Maybe (SubjectID, Event d c a)
decodeEvent' = makeEventDecoder decode'
decodeEvent = makeEventDecoder decode

makeEventDecoder
  :: Functor f
  => (B.ByteString -> f (EventLine d c a))
  -> (B.ByteString -> f (SubjectID, Event d c a))
makeEventDecoder f = fmap (\x -> (getSubjectID x, getEventLine x)) . f

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
     )
  => B.ByteString
  -> ([String], [(SubjectID, Event d c a)])
parseEventLinesL' l = partitionEithers $ fmap eitherDecodeEvent' (B.lines l)


