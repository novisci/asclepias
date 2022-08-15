{-|

-}

{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Aeson
import           Data.Vector                         ((!))
import           Hasklepias.AppBuilder.LineFilterApp

newtype LineAppTesterID = MkLineAppTesterID Int deriving (Show, Eq)

instance FromJSON LineAppTesterID where
  parseJSON = withArray "FooID" $ \a -> do
    id <- parseJSON (a ! 0)
    pure $ MkLineAppTesterID id

newtype LineAppTester = MkLineAppTester Bool deriving (Show, Eq, Ord)

instance FromJSON LineAppTester where
  parseJSON = withArray "Foo" $ \a -> do
    id <- parseJSON (a ! 1)
    pure $ MkLineAppTester id

dclS' = decodeStrict' @LineAppTester
dciS' = decodeStrict' @LineAppTesterID
tpr = (== MkLineAppTester True)

main :: IO ()
main = makeLineFilterApp "demo" dciS' dclS' tpr
