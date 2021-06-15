{-|
Module      : Functions for Parsing Hasklepias populations 
Description : Defines FromJSON instances for Hasklepias populations .
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
module Hasklepias.Aeson(
      parsePopulationIntLines
    , parsePopulationDayLines
    , module Data.Aeson
) where

import IntervalAlgebra
import EventData
import EventData.Aeson
import Hasklepias.Cohort
import Data.Aeson                           ( FromJSON(..)
                                            , ToJSON(..)
                                            , eitherDecode
                                            , Value(Array))
import qualified Data.ByteString.Lazy as B  ( fromStrict
                                            , toStrict
                                            , ByteString)
import qualified Data.ByteString.Char8 as C ( lines )
import Prelude                              ( (<$>), (<*>), ($), fmap, id
                                            , Int, String, Ord, Show)
import Data.Bifunctor ( Bifunctor(first) )
import Data.Either                          ( Either(..), either, rights, partitionEithers )
import Data.List                            ( sort, (++), zipWith )
import qualified Data.Map.Strict as M       ( toList, fromListWith)
import Data.Vector                          ( (!) )
import Data.Time.Calendar                   ( Day )
import GHC.Num                              ( Natural )

newtype SubjectEvent a = MkSubjectEvent (ID, Event a)

subjectEvent :: ID -> Event a -> SubjectEvent a
subjectEvent x y = MkSubjectEvent (x, y)

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (SubjectEvent a) where
    parseJSON (Array v) = subjectEvent <$>
        parseJSON (v ! 0) <*> (event <$> parseJSON (v ! 5) <*> parseJSON (Array v))

mapIntoPop :: (Ord a) => [SubjectEvent a] -> Population (Events a)
mapIntoPop l = MkPopulation $
    fmap (\(id, es) -> MkSubject (id, sort es)) -- TODO: is there a way to avoid the sort
        (M.toList $ M.fromListWith (++) (fmap (\(MkSubjectEvent (id, e)) -> (id, [e])) l ))

-- |  Parse @Event Int@ from json lines.
parseSubjectLines ::
    (FromJSON a, Show a, IntervalSizeable a b) =>
        B.ByteString -> ([(Natural, String)], [SubjectEvent a] )
parseSubjectLines l =
    -- partitionEithers $ fmap 
    -- (\x -> eitherDecode $ B.fromStrict x :: (FromJSON a, Show a, IntervalSizeable a b) => Either String (SubjectEvent a))
    --     (C.lines $ B.toStrict l)
    partitionEithers $ zipWith
    (\x i ->
        first (i,) (eitherDecode $ B.fromStrict x :: (FromJSON a, Show a, IntervalSizeable a b) => Either String (SubjectEvent a))
    )
    (C.lines $ B.toStrict l)
    [1..]

-- |  Parse @Event Int@ from json lines.
parsePopulationIntLines :: B.ByteString -> ([(Natural, String)], Population (Events Int))
parsePopulationIntLines x = fmap mapIntoPop (parseSubjectLines x)

-- |  Parse @Event Day@ from json lines.
parsePopulationDayLines :: B.ByteString -> ([(Natural, String)], Population (Events Day))
parsePopulationDayLines x = fmap mapIntoPop (parseSubjectLines x)
