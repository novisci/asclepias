{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasklepias.MakeFilterApp
  ( makeFilterApp
  , runFilterApp
  , runFilterAppWithLocation
  , FilterApp
  ) where

import           Cohort.Core                    ( SubjectID )
import           Control.Applicative            ( (<**>)
                                                , Alternative((<|>))
                                                , Applicative(liftA2)
                                                )
import           Control.Monad                  ( join )
import           EventData                      ( Event )
import           IntervalAlgebra                ( IntervalSizeable )


import           Conduit                        ( (.|)
                                                , ConduitM
                                                , mapC
                                                , runConduit
                                                , yield
                                                )
import           Data.Aeson                    as A
                                                ( FromJSON(parseJSON)
                                                , decodeStrict'
                                                , withArray
                                                )
import qualified Data.ByteString.Char8         as C
import qualified Data.Conduit.Combinators      as CC
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Any(Any, getAny) )
import           Data.Vector                    ( (!) )

import           Colog                          ( (<&)
                                                , logStringStderr
                                                )

import           Hasklepias.AppUtilities
import           Options.Applicative



-- Container for app options
newtype FilterAppOpts = FilterAppOpts
  { input  :: Input
  }

-- Create the ParserInfo for a MakePrefilterApp 
makeAppArgs :: String -> ParserInfo FilterAppOpts
makeAppArgs name = Options.Applicative.info
  (FilterAppOpts <$> (fileInput <|> s3Input <|> stdInput) <**> helper)
  (fullDesc <> header ("Filter events for " <> name))

-- A type to hold a subject ID
newtype HoldID = MkID SubjectID deriving (Eq, Show)
instance FromJSON HoldID where
  parseJSON = withArray "Event" $ \a -> do
    id <- parseJSON (a ! 0)
    return $ MkID id

-- Collector for filter
--  * current ID
--  * boolean indicating whether any events have met predicate
--  * accumulated ByteString
newtype FilterState = FilterState (HoldID, Bool, C.ByteString)
  deriving (Show)

getId :: FilterState -> HoldID
getId (FilterState (x, _, _)) = x

getBool :: FilterState -> Bool
getBool (FilterState (_, x, _)) = x

getAcc :: FilterState -> C.ByteString
getAcc (FilterState (_, _, x)) = x

-- Combine two FilterStates. This combinator and the rest of the application logic
-- makes the assumption that all of a subject's data are grouped together.
instance Semigroup FilterState where
  (<>) (FilterState (id0, b0, x0)) (FilterState (id1, b1, x1)) = if id0 == id1
    then FilterState (id1, getAny $ Any b0 <> Any b1, x0 <> "\n" <> x1)
    else FilterState (id1, getAny $ Any b1, x1)

-- Initialize a FilterState using a given parser and predicate function.
initFilterState
  :: (Show a, FromJSON a, IntervalSizeable a b)
  => (C.ByteString -> Maybe (Event a)) -- ^ Event parser
  -> (Event a -> Bool) -- ^ Predicate on events
  -> C.ByteString -- ^ the data to (attempt to) parse into an event
  -> FilterState
initFilterState f p x = FilterState (id, b, x)
 where
  id = fromMaybe (MkID "") (decodeStrict' x :: Maybe HoldID)
  b  = maybe False p (f x)

-- Combine two FilterStates. In the case, the subject id changes AND the status
-- is True (i.e. at least one event satisfied a predicate) then output the 
-- accummulated subject's data.
fsc :: FilterState -> FilterState -> IO FilterState
fsc x y = do
        -- let errLog = logStringStderr
        -- In the case 
  case (getId x /= getId y, getBool x) of
    (True, True) -> C.putStrLn (getAcc x)
    -- (True, False) -> do errLog <& (unpack . (\(MkID z) -> z)) (getId x)
    _            -> C.putStr ""
  return (x <> y)

-- Lifted version of fsc
fscIO :: IO FilterState -> IO FilterState -> IO FilterState
fscIO x y = do
  join (liftA2 fsc x y)

-- The main pipeline of the filter app. Note that the last subject is not handled
-- here. Their data is accumulated but not output (if it needs to be) by this 
-- Conduit.
prefilterC
  :: (IntervalSizeable a b, FromJSON a, Show a, Monad m)
  => (Event a -> Bool)
  -> C.ByteString
  -> ConduitM i c m (Maybe (IO FilterState))
prefilterC p x =
  yield x
    .| CC.linesUnboundedAscii
    .| mapC (return . initFilterState decodeStrict' p)
    .| CC.foldl1 fscIO

-- | Type containing the filter app
newtype FilterApp m = MkFilterApp { runPreApp :: Maybe Location -> m C.ByteString }

{- | 
-}
makeFilterApp
  :: (Show a, FromJSON a, IntervalSizeable a b)
  => String -- ^ name of the app (e.g. a project's id)
  -> (Event a -> Bool) -- ^ predicate to evaluate for each event
  -> FilterApp IO
makeFilterApp name predicate = MkFilterApp $ \l -> do
  options <- execParser (makeAppArgs name)
  let loc = case l of
        Nothing -> inputToLocation $ input options
        Just x  -> x

  dat <- readDataStrict loc
  res <- runConduit $ prefilterC predicate dat

  case res of
    Nothing -> pure ""
    Just final ->
      final >>= (\x -> if getBool x then pure (getAcc x <> "\n") else pure "")

-- | Just run the thing.
runFilterApp :: FilterApp IO -> IO ()
runFilterApp x = C.putStr =<< runPreApp x Nothing

-- | Just run the thing with a set location (e.g for testing).
runFilterAppWithLocation :: Location -> FilterApp IO -> IO C.ByteString
runFilterAppWithLocation l x = runPreApp x (Just l)


