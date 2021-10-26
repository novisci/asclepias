{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasklepias.MakePrefilterApp
  (
  ) where

import EventData.Core
import EventData.Predicates
import IntervalAlgebra ( IntervalSizeable )
import Cohort
import Control.Applicative
import Data.Maybe
import Data.Functor.Contravariant
import Data.Aeson as A
import Data.Monoid
import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad
import qualified Data.ByteString.Char8         as C
import           Colog                          ( (<&)
                                                , logStringStderr
                                                )

import           Hasklepias.AppUtilities     
import           Options.Applicative
import Data.Time
import           Data.Vector                    ( (!) )


--- App Setup
newtype MakePrefilterApp = MakePrefilterApp
  { input  :: Input
  -- , output :: FilePath
  }

mainOptions :: Parser MakePrefilterApp
mainOptions = MakePrefilterApp <$> (fileInput <|> s3Input <|> stdInput)
--  <*> strOption
--   (long "output" <> short 'o' <> metavar "FILE" <> value "output.jsonl" <> help
--     "Output location"
--   )

makeAppArgs :: String -> ParserInfo MakePrefilterApp
makeAppArgs name  = Options.Applicative.info
  (mainOptions <**> helper)
  (fullDesc <> header ("Prefilter app for " <> name))

newtype HoldID = MkID SubjectID deriving (Eq, Show)
instance FromJSON HoldID where 
    parseJSON = withArray "Event" $ \a -> do
      id <- parseJSON (a ! 0)
      return $ MkID id 

newtype MyState2 = MyState2 (HoldID, Bool, C.ByteString)
  deriving (Show)

instance Semigroup MyState2 where
  (<>) (MyState2 (id0, b0, x0)) (MyState2 (id1, b1, x1)) =
    if id0 == id1 then
        MyState2 (id1, getAny $ Any b0 <> Any b1, x0 <> "\n" <> x1)
    else
        MyState2 (id1, getAny $ Any b1, x1)



makeMyState2 :: (Show a, FromJSON a, IntervalSizeable a b) =>
     (C.ByteString -> Maybe (Event a))
  -> (Event a -> Bool) -- ^ 
  -> C.ByteString -> MyState2
makeMyState2 f p x = MyState2 ( id , b , x )
        where
            id = fromMaybe (MkID "") (decodeStrict' x :: Maybe HoldID)
            b  = maybe False p (f x)

getId :: MyState2 -> HoldID
getId (MyState2 (x, _, _)) = x

getBool :: MyState2 -> Bool
getBool (MyState2 (_, x, _)) = x

getAcc :: MyState2 -> C.ByteString
getAcc (MyState2 (_, _, x)) = x

ff :: MyState2 -> MyState2 -> IO MyState2
ff x y = do
        -- let errLog = logStringStderr
        case (getId x /= getId y, getBool x) of
              (True, True)  -> C.putStrLn (getAcc x)
            --  (True, False) -> do errLog <& show x
              -- (True, False) -> do errLog <& (unpack . (\(MkID z) -> z)) (getId x)
            --  _             -> do errLog <& show x
              _             -> C.putStr ""
        return (x <> y)

fff :: IO MyState2 -> IO MyState2 -> IO MyState2
fff x y = do join (liftA2 ff x y)

-- myConduitE x =
--        yield x
--     .| CC.linesUnboundedAscii
--     .| mapC (return . initMyState2)
--     .| CC.foldl1 fff


-- main2 :: IO ()
-- main2 = do
--     res <- runConduit (myConduitE someSilliness2)
--     forM_ res ((C.putStrLn . getAcc) =<<)

-- myConduitG p x =
--        yield x
--     .| CC.linesUnboundedAscii
--     .| mapC (return . makeMyState2 decodeStrict' p)
--     .| CC.foldl1 fff


-- main3 :: IO ()
-- main3 = do
--     dat <- readDataStrict (Local "testMe.jsonl")
--     res <- runConduit (myConduitG (getPredicate isEnrollmentEvent :: Event Day -> Bool) dat)
--     case res of 
--       Nothing    -> pure ()
--       Just final -> final >>= (\x -> if getBool x then (C.putStrLn . getAcc) x
--                                      else pure ())

-- | Type containing the cohort app
newtype PrefilterApp m = MkPrefilterApp { runPrefilterApp :: Maybe Location -> m C.ByteString }

prefilterC :: (IntervalSizeable a b, FromJSON a, Show a, Monad m) =>
    (Event a -> Bool)
  -> C.ByteString
  -> ConduitM i c m (Maybe (IO MyState2))
prefilterC p x =
       yield x
    .| CC.linesUnboundedAscii
    .| mapC (return . makeMyState2 decodeStrict' p)
    .| CC.foldl1 fff

makePrefilterApp :: (Show a, FromJSON a, IntervalSizeable a b) =>
       String
    -> (Event a -> Bool)
    -> PrefilterApp IO
makePrefilterApp name predicate  = MkPrefilterApp $
    \l -> do
      options <- execParser (makeAppArgs name)
      let loc = case l of
            Nothing -> inputToLocation $ input options
            Just x  -> x
        
      dat <- readDataStrict loc
      res <- runConduit $ prefilterC predicate dat

      case res of 
        Nothing    -> pure ""
        Just final -> final >>= (\x -> if getBool x then pure (getAcc x <> "\n")
                                      else pure "")

-- | Just run the thing.
runPreApp :: PrefilterApp IO -> IO ()
runPreApp x = C.putStr =<< runPrefilterApp x Nothing

testEnrollment :: PrefilterApp IO
testEnrollment = makePrefilterApp "test" (getPredicate isEnrollmentEvent :: Event Day -> Bool)

-- | Just run the thing with a set location (e.g for testing).
runPreAppWithLocation :: Location -> PrefilterApp IO -> IO C.ByteString
runPreAppWithLocation l x = runPrefilterApp x (Just l)
