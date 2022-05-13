{-| 
Module      : Test.Monarch.Parse
Description : Helpers for marshalling csv text input to `TestMap` via `dhall-csv`.
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
module Test.Monarch.Parse
  ( tryParseRecordsCsv
  , decodeMapSchema
  , decodeMapSchemaAuto
  , parseDhallFile
  , parseDhallFileWith
  ) where

import           Test.Monarch.Internal.Csv      ( tryParseRecordsCsv )
import           Test.Monarch.Internal.Dhall    ( decodeMapSchema
                                                , decodeMapSchemaAuto
                                                , parseDhallFile
                                                , parseDhallFileWith
                                                )
