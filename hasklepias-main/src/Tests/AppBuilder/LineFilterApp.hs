{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.AppBuilder.LineFilterApp where

import qualified Control.Foldl                 as L
import           Data.Aeson
import qualified Data.ByteString.Char8         as C
import           Data.String.Interpolate        ( i )
import           Data.Text
import           Data.Vector                    ( (!) )
import           Hasklepias.AppBuilder.LineFilterApp
import           Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
import           Test.Tasty
import           Test.Tasty.Bench


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
      Test application

      Used in lineFilter-test/Main.hs
-}
testFilterApp = makeLineFilterApp "Testing 1,2"
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
      Benchmarks
-}

listInput :: Int -> [C.ByteString]
listInput n =
  uncurry mkTestInput <$> Prelude.replicate n (1 :: Int, "0" :: Text)

folder =
  L.fold (filterGroupFold (decodeStrict' @LineAppTester) (> MkLineAppTester 0))



benches =
  [ bgroup
      "Line filter app benchmarks"
      [ bench "Group-level fold - 100 elements" $ nf folder (listInput 100)
      , bench "Group-level fold - 1000 elements" $ nf folder (listInput 1000)
      , bench "Group-level fold - 10000 elements" $ nf folder (listInput 10000)
      ]
  ]

{-
      Tests
-}

-- tests = testGroup "" 
--     []
