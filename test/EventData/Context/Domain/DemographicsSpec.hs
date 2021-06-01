{-# LANGUAGE OverloadedStrings #-}
module EventData.Context.Domain.DemographicsSpec (spec) where

import EventData.Context.Domain.Demographics
import Test.Hspec                       ( it, shouldBe, Spec )
import Control.Lens                     ( (^.), view )

dmo :: DemographicsFacts
dmo = DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

spec :: Spec
spec = do
    it "" $
      view (demo.field) dmo `shouldBe` BirthYear
    it "" $ 
       dmo^.demo.field `shouldBe` BirthYear
    it "" $
       dmo^.demo.info `shouldBe` Just "1987"