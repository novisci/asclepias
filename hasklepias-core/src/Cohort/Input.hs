{-|
Module      : Functions for Parsing Hasklepias populations 
Description : Defines FromJSON instances for Hasklepias populations .
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Cohort.Input
  ( parsePopulationLines
  , parsePopulationIntLines
  , parsePopulationDayLines
  , ParseError(..)
  ) where

import           Cohort.Core                    ( ID
                                                , Population(..)
                                                , Subject(MkSubject)
                                                )
import           Control.Applicative            ( (<$>)
                                                , Applicative((<*>))
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(Array)
                                                , eitherDecode
                                                )
import           Data.Bifunctor                 ( Bifunctor(first) )
import qualified Data.ByteString.Char8         as C
                                                ( lines )
import qualified Data.ByteString.Lazy          as B
                                                ( ByteString
                                                , fromStrict
                                                , toStrict
                                                )
import           Data.Either                    ( Either(..)
                                                , partitionEithers
                                                )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($)
                                                , id
                                                )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List                      ( (++)
                                                , sort
                                                , zipWith
                                                )
import qualified Data.Map.Strict               as M
                                                ( fromListWith
                                                , toList
                                                )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time.Calendar             ( Day )
import           Data.Vector                    ( (!) )
import           EventData                      ( Event
                                                , Events
                                                , event
                                                )
import           EventData.Aeson                ( )
import           GHC.Int                        ( Int )
import           GHC.Num                        ( Natural )
import           GHC.Show                       ( Show )
import           IntervalAlgebra                ( IntervalSizeable )
import           Prelude                        ( String )


newtype SubjectEvent a = MkSubjectEvent (ID, Event a)

subjectEvent :: ID -> Event a -> SubjectEvent a
subjectEvent x y = MkSubjectEvent (x, y)

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (SubjectEvent a) where
  parseJSON (Array v) =
    subjectEvent
      <$> parseJSON (v ! 0)
      <*> (event <$> parseJSON (v ! 5) <*> parseJSON (Array v))

mapIntoPop :: (Ord a) => [SubjectEvent a] -> Population (Events a)
mapIntoPop l = MkPopulation $ fmap
  (\(id, es) -> MkSubject (id, sort es)) -- TODO: is there a way to avoid the sort?
  ( M.toList
  $ M.fromListWith (++) (fmap (\(MkSubjectEvent (id, e)) -> (id, [e])) l)
  )

decodeIntoSubj
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> Either Text (SubjectEvent a)
decodeIntoSubj x = first pack $ eitherDecode x

-- | Contains the line number and error message.
newtype ParseError = MkParseError (Natural, Text) deriving (Eq, Show)

-- |  Parse @Event Int@ from json lines.
parseSubjectLines
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> ([ParseError], [SubjectEvent a])
parseSubjectLines l = partitionEithers $ zipWith
  (\x i -> first (\t -> MkParseError (i, t)) (decodeIntoSubj $ B.fromStrict x))
  (C.lines $ B.toStrict l)
  [1 ..]

-- |  Parse @Event Int@ from json lines.
parsePopulationLines
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> ([ParseError], Population (Events a))
parsePopulationLines x = fmap mapIntoPop (parseSubjectLines x)

-- |  Parse @Event Int@ from json lines.
parsePopulationIntLines
  :: B.ByteString -> ([ParseError], Population (Events Int))
parsePopulationIntLines x = fmap mapIntoPop (parseSubjectLines x)

-- |  Parse @Event Day@ from json lines.
parsePopulationDayLines
  :: B.ByteString -> ([ParseError], Population (Events Day))
parsePopulationDayLines x = fmap mapIntoPop (parseSubjectLines x)
