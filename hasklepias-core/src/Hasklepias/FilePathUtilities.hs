{-|
Module      : FilePath functions 
Description : Few helpers for manipulating file paths
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

NOTE: this module is currently unused in Hasklepias

-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.FilePathUtilities
  ( updatePartitionPath ) 
  where

import           Data.Bool                      ( (&&)
                                                , (||)
                                                )
import           Data.Function                  ( ($)
                                                , (.)
                                                , flip
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord(..) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.String                    ( String )
import           GHC.IO                         ( FilePath
                                                , IO
                                                )
import           GHC.Show                       ( Show(..) )
import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( Monad((>>=)) )
import           Formatting                     ( Buildable
                                                , Format
                                                , formatToString
                                                , left
                                                )
import           GHC.Int                        ( Int )
import           GHC.Num                        ( Num((-)) )
import           GHC.Real                       ( (^) )
import           Safe                           ( atMay
                                                , headMay
                                                )
import           Text.Read                      ( readMaybe )

data MaybePartitionFile =
    NOutOfBounds
  | ParseTemplateError String
  | PartitionFile String
  deriving Show

data TemplateParse =
    FailedIntParse
  | FailedPathSplit
  | SuccessfulParse (String, Int, String)
  deriving Show

leftPadZeros :: Buildable a => Int -> Format r (a -> r)
leftPadZeros n = left n '0'

templateDelimiter :: String
templateDelimiter = "%%"

call3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
call3 f g h = (\((x, y), z) -> (x, y, z)) . ((f &&& g) &&& h)

checkParse :: (Maybe String, Maybe Int, Maybe String) -> TemplateParse
checkParse (Just x, Just y, Just z) = SuccessfulParse (x, y, z)
checkParse (_, Just x, _) = if x < 0 then FailedIntParse else FailedPathSplit
checkParse (_, Nothing, _) = FailedIntParse

parsePartitionFile :: FilePath -> TemplateParse
parsePartitionFile =
  checkParse
    . call3 headMay (\x -> (x `atMay` 1) >>= readMaybe) (`atMay` 2)
    . splitOn templateDelimiter

fillTemplate :: Int -> (String, Int, String) -> FilePath
fillTemplate i (x, y, z) = x <> formatToString (leftPadZeros y) i <> z

-- |
--
-- >>> let g = "xxx/yyy/myPart-%%5%%.json"
-- >>> updatePath g 3
--
updatePartitionPath :: FilePath -> Int -> MaybePartitionFile
updatePartitionPath x n = case parsePartitionFile x of
  FailedIntParse -> ParseTemplateError "failed to parse int within delimiter"
  FailedPathSplit ->
    ParseTemplateError ("failed to split template by " <> templateDelimiter)
  SuccessfulParse x1 -> if 0 <= n && n <= (\(_, y, _) -> 10 ^ y - 1) x1
    then PartitionFile $ fillTemplate n x1
    else NOutOfBounds

