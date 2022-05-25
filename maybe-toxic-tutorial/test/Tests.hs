--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore #-}

import           Test.Hspec
import           TypeExercises

main :: IO ()
main = hspec specs

-- TODO run over more tests
specs :: Spec
specs = describe "First type exercises" $ do
    it "dummy" $ do
      True `shouldBe` True

-- TODO this has multiple problems
--  it "noPower works" $ do
--    noPower 4 :: Double `shouldBe` 16 :: Int
--
--  it "doublePower Double Int" $ do
--    doublePower (4.5 :: Double) (1 :: Int) `shouldBe` 4.5 :: Double
--
--  it "doublePower Int Int" $ do
--    doublePower (4 :: Int) ((-1) :: Int) `shouldBe` 0.25 :: Int

-- TODO implement something like this, cribbed from exercism
--specs :: Spec
--specs = describe "TypeExercises" $ for_ cases test
--  where
--    test Case{..} = it explanation assertion
--      where
--        explanation = unwords [show input, "-", description]
--        assertion   = fun (fromIntegral input) `shouldBe` expected
--
--data Case = Case { description :: String
--                 , input       :: Integer
--                 , expected    :: Bool
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "year not divisible by 4 in common year"
--               , input       = 2015
--               , expected    = False
--               }
--        ]
