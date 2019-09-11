module Hasklepias.Events.EventsSpec where

import Hasklepias.Events
import Hasklepias.IntervalAlgebra
import Test.Hspec

c1 = eventContext Nothing Nothing Nothing
e1 = event (period 0 1) c1 
e2 = event (period 2 3) c1


main :: IO ()
main = hspec $ do
  describe "Tests of Events" $ 
    do 
      it "A simple unit test to get the ball rolling" $ e1 < e2 `shouldBe` True




