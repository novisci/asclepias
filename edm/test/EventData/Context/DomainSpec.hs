{-# LANGUAGE OverloadedStrings #-}
module EventData.Context.DomainSpec (spec) where

import EventData.Context.Domain
import Test.Hspec                       ( it, shouldBe, Spec )
import Control.Lens                     ( preview, (^.), (^?), (.~), over )
import Control.Monad                    ( join )
import Data.Aeson                       ( decode )
import Data.Text                        ( Text, unpack )
import Data.Text.Read                   ( rational )
import qualified Data.ByteString.Lazy as B

dmo :: Domain
dmo = Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

myIntMap :: Text -> Maybe Integer -- TODO: this is ridiculous
myIntMap x =  fmap floor (either (const Nothing) (Just . fst) (Data.Text.Read.rational x))

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
