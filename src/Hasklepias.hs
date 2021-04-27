{-# LANGUAGE OverloadedStrings #-}
module Hasklepias (
) where
import IntervalAlgebra
import Hasklepias.Types


-- import Data.Aeson
-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Char8 as C


-- testInputs :: B.ByteString
-- testInputs = 
--       "[\"abc\", 0, 1, \"Diagnosis\",\
--       \[\"someThing\"],\
--       \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":0,\"end\":1}}]\n\
--       \[\"abc\", 5, 6, \"Diagnosis\",\
--       \[\"someThing\"],\
--       \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":5,\"end\":6}}]"



-- main :: IO ()
-- main = do
--      d <- (decode <$> testIn) :: IO (Maybe (InInterval Int))
--      case d of
--         Nothing -> putStrLn "test"
--         Just x  -> print x
--      -- case d of
--      --  Left err -> putStrLn err
--      --  Right ps -> print ps