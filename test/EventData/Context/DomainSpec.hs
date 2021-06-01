{-# LANGUAGE OverloadedStrings #-}
module EventData.Context.DomainSpec (spec) where

import EventData.Context.Domain
import EventData.Context.Domain.Demographics
import Test.Hspec                       ( it, shouldBe, Spec )
import Control.Lens                     ( preview, (^.), (^?), (.~), over )
import Control.Monad                    ( join )
import Data.Aeson                       ( decode )
import Data.Text                        ( Text, unpack )
import qualified Data.ByteString.Lazy as B

dmo :: Domain
dmo = Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

jsonDemoTest :: B.ByteString
jsonDemoTest = "{\"domain\":\"Demographics\",\"facts\":{\"demo\":{\"field\":\"BirthYear\",\"info\":\"1987\"}}}"

jsonOtherTest :: B.ByteString
jsonOtherTest = "{\"domain\":\"Labs\",\"facts\":{\"code\":{\"code\":\"XYZ\"}}}"

myIntMap :: Text -> Maybe Integer -- TODO: this is ridiculous
myIntMap "1987" = Just 1987
myIntMap _ = Nothing

spec :: Spec
spec = do
    it "preview demographics facts" $
      dmo ^? _Demographics `shouldBe` Just (DemographicsFacts( DemographicsInfo BirthYear (Just "1987")))
    it "preview demographic field" $
      fmap (^.demo.field) (dmo ^? _Demographics)  `shouldBe` Just BirthYear
    it "preview birthyear" $
      ((^.demo.info) =<< preview _Demographics dmo)  `shouldBe` Just "1987"
    it "preview birthyear as Integer" $
      (myIntMap =<< ((^.demo.info) =<< preview _Demographics dmo))  `shouldBe` Just 1987
    it "jsonDemoTest is parsed correctly" $
       decode jsonDemoTest  `shouldBe` Just dmo

    it "jsonOtherTest is parsed correctly" $
       decode jsonOtherTest  `shouldBe` Just (UnimplementedDomain ())