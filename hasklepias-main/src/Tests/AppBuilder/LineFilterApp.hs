{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.AppBuilder.LineFilterApp where

import Hasklepias.AppBuilder.LineFilterApp
import Hasklepias.AppBuilder.LineFilterApp.Foldl
import Hasklepias.AppBuilder.LineFilterApp.Conduit
import qualified Data.ByteString.Char8         as C
import Data.Aeson
import Data.Text
import           Data.Vector                    ( (!) )
import Test.Tasty.Bench
import Test.Tasty
import           Data.String.Interpolate        ( i ) 


{-
      Types for testing
-}

newtype LineAppTesterID = MkLineAppTesterID Int deriving (Show, Eq)

instance FromJSON LineAppTesterID where
    parseJSON = withArray "FooID" $ \a -> do
        id <- parseJSON (a ! 0)
        pure $ MkLineAppTesterID id

newtype LineAppTester = MkLineAppTester Int deriving (Show, Eq, Ord)

instance FromJSON LineAppTester where
    parseJSON = withArray "Foo" $ \a -> do
        id <- parseJSON (a ! 1)
        pure $ MkLineAppTester id

{-
      Test applications
-}

testFold1 = makeLineFilterAppF 
            "Fold-based1"
            (decodeStrict' @LineAppTesterID)
            (decodeStrict' @LineAppTester)
            (> MkLineAppTester 0)

testFold2 = makeLineFilterAppF'
            "Fold-based2"
            (decodeStrict' @LineAppTesterID)
            (decodeStrict' @LineAppTester)
            (> MkLineAppTester 0)


testC1 = makeLineFilterAppC
            "Conduit-based1"
            (decodeStrict' @LineAppTesterID)
            (decodeStrict' @LineAppTester)
            (> MkLineAppTester 0)

testC2 = makeLineFilterAppC'
            "Conduit-based2"
            (decodeStrict' @LineAppTesterID)
            (decodeStrict' @LineAppTester)
            (> MkLineAppTester 0)


{-
      Test values constructors
-}

mkTestInput :: Int -> Text -> C.ByteString
mkTestInput y x = [i|[#{ show y }, #{ x }]|]

mkTestLines :: [(Int, Text)] -> C.ByteString
mkTestLines x = C.intercalate "\n" (fmap (uncurry mkTestInput) x)

{-
      Test Cases
-}

input1 = mkTestInput 1 "\"-\""

input2 = mkTestLines [(1, "1"), (1, "\"-\"")]


input :: C.ByteString
input = C.pack $ unpack $ intercalate "\n" (Prelude.replicate 10000 "[1, 2]")



appF = filterAppF'
      (decodeStrict' @LineAppTesterID)
      (decodeStrict' @LineAppTester)
      (> MkLineAppTester 0)

appL = filterAppF
      (decodeStrict' @LineAppTesterID)
      (decodeStrict' @LineAppTester)
      (> MkLineAppTester 0)

-- appC'  = appC
--       (decodeStrict' @LineAppTesterID)
--       (decodeStrict' @LineAppTester)
--       (> MkLineAppTester 1)

-- appC'' = appC2
--       (decodeStrict' @LineAppTesterID)
--       (decodeStrict' @LineAppTester)
--       (> MkLineAppTester 1)

benches = [bgroup "filter app"
     [ bench "list-based" $ nfAppIO appF input
     , bench "list2" $ nfAppIO   appL input
--      , bench "conduit2" $ nf appC'' input
     ]]





tests = testGroup "a test" 
    []