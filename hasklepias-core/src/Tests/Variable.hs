{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for type information related to `Variable`. For output shape tests,
-- see 'Tests.Cohort.Output'.
module Tests.Variable
  ( tests,
  )
where

import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Variable.Attributes
import Variable.R.Factor
import Variable.R.SEXP
import Variable.R.Stype
import Variable.Variable

{- Values -}

-- Type reporting

rVarScalar :: [SomeRTypeRep]
rVarScalar =
  [ SomeRTypeRep $ as_logical True,
    SomeRTypeRep $ as_integer (1 :: Integer),
    SomeRTypeRep $ as_numeric (1 :: Double),
    -- See Note in Variable.R.SEXP on SEXPElem 'CPLSXP
    SomeRTypeRep $ as_character ("yes" :: String)
  ]

rVarScalarTypes :: [SEXPTYPE]
rVarScalarTypes = map sexpTypeOfErased rVarScalar

rList :: RTypeRep 'VECSXP
rList =
  V.fromList $
    zipWith
      (curry Just)
      ["my_logical", "my_integer", "my_numeric", "my_character"]
      rVarScalar

rVar :: [SomeRTypeRep]
rVar =
  [ SomeRTypeRep $ as_logical [True],
    SomeRTypeRep $ as_integer [1 :: Int32],
    SomeRTypeRep $ as_numeric [1 :: Double],
    SomeRTypeRep $ as_character ["yes" :: Text],
    -- Singleton list, coerced from atomic vector via as_list.
    SomeRTypeRep $ as_list (as_logical True),
    -- Direct construction of a list.
    SomeRTypeRep rList
  ]

rVarTypes :: [SEXPTYPE]
rVarTypes = map sexpTypeOfErased rVar

-- Wrapped in Variable to allow inhomogeneous types, for convenience.
rVarFactor :: [Variable]
rVarFactor =
  [ rFactor "my_logical" (as_logical $ replicate 3 True) $ V.singleton (pack "True"),
    rFactor "my_integer" (as_integer [1 :: Int32, 2]) $ V.map (pack . show) $ V.fromList [1 :: Int32, 2],
    rFactor "my_character" (as_character $ map show [1 :: Int32, 2]) $ V.map (pack . show) $ V.fromList [1 :: Int32, 2],
    rFactor "my_character_2" (as_character cs) cs
  ]
  where
    cs = V.map (pack . show) $ V.fromList ['a' .. 'd']

rVarFactorTypes :: [Text]
rVarFactorTypes = map (varType . varAttrs) rVarFactor

dummyStypeAttrs :: StypeAttrs
dummyStypeAttrs = MkStypeAttrs "" "" Nothing

rVarStype :: [Variable]
rVarStype =
  [ stypeVector "my_binary" $ as_v_binary True dummyStypeAttrs,
    stypeVector "my_continuous_nonneg" $ as_v_continuous_nonneg (1.0 :: Double) dummyStypeAttrs,
    stypeVector "my_count" $ as_v_count (1 :: Int32) dummyStypeAttrs,
    stypeVector "my_proportion" $ as_v_proportion (0.5 :: Double) dummyStypeAttrs
  ]

rVarStypeTypes :: [Text]
rVarStypeTypes = map (varType . varAttrs) rVarStype

-- Constructors and conversions
-- Need only test R-like coercion behavior

rVarIntOutOfBounds :: RTypeRep 'INTSXP
rVarIntOutOfBounds = as_integer (1 + toInteger (maxBound :: Int32))

-- At present, the intended behavior is to fail invalid conversions at by
-- throwing 'error'.
rVarStypeInvalidNonneg :: Bool
rVarStypeInvalidNonneg = V.all isNothing $ stypeRTypeRep $ as_v_continuous_nonneg [-1.0 :: Double] dummyStypeAttrs

rVarStypeInvalidCount :: Bool
rVarStypeInvalidCount = V.all isNothing $ stypeRTypeRep $ as_v_count [-1 :: Integer] dummyStypeAttrs

rVarStypeInvalidProportion :: Bool
rVarStypeInvalidProportion = V.all isNothing $ stypeRTypeRep $ as_v_proportion [-1.0 :: Double] dummyStypeAttrs

-- R Factor construction.
-- Constructor 'factor' does transformation to emulate the
-- R function's behavior, which is what is tested here.

rFactorCorrectLevels :: [Factor]
rFactorCorrectLevels =
  [ factor [1 .. 3 :: Int] (V.fromList ["1", "2", "3"]),
    factor [1.0 :: Double, 2.0, 2.0, 1.0, 0.0] (V.fromList ["0.0", "1.0", "2.0"]),
    factor ["Yes" :: Text, "No", "No", "Yes"] (V.fromList ["Yes", "No"]),
    factor (map Just ["Yes" :: Text, "No", "No", "Yes"]) (V.fromList ["Yes", "No"]),
    factor (Nothing : map Just ["Yes" :: Text, "No", "No", "Yes"]) (V.fromList ["Yes", "No"])
  ]

rFactorIncorrectLevels :: [Factor]
rFactorIncorrectLevels =
  [ -- 3 is missing from levels and will be converted to Nothing
    factor [1 .. 3 :: Int] (V.fromList ["1", "2"]),
    -- levels incorrectly formatted.
    factor [1.0 :: Double, 2.0, 2.0, 1.0, 0.0] (V.fromList ["0", "1", "2"]),
    -- pack . show is not the id from Text -> Text,
    -- as show for Text does some escaping and other transforms.
    factor ["Yes" :: Text, "No", "No", "Yes"] (V.fromList $ map (pack . show) ["Yes" :: Text, "No"])
  ]

{- Expected -}

-- Type reporting

rVarScalarTypesExp :: [SEXPTYPE]
rVarScalarTypesExp = [LGLSXP, INTSXP, REALSXP, STRSXP]

rVarTypesExp :: [SEXPTYPE]
rVarTypesExp = [LGLSXP, INTSXP, REALSXP, STRSXP, VECSXP, VECSXP]

rVarFactorTypesExp :: [Text]
rVarFactorTypesExp = replicate 4 (pack $ show STRSXP)

rVarStypeTypesExp :: [Text]
rVarStypeTypesExp = map (pack . show) [LGLSXP, REALSXP, INTSXP, REALSXP]

-- Constructors and conversions

rVarIntOutOfBoundsExp :: RTypeRep 'INTSXP
rVarIntOutOfBoundsExp = V.singleton Nothing

-- Factor
rFactorCorrectLevelsExp :: [Factor]
rFactorCorrectLevelsExp =
  [ MkFactor (V.fromList $ map Just ["1", "2", "3"]) (V.fromList ["1", "2", "3"]),
    MkFactor (V.fromList $ map Just ["1.0", "2.0", "2.0", "1.0", "0.0"]) (V.fromList ["0.0", "1.0", "2.0"]),
    -- NOTE: levels are lexicographically sorted.
    MkFactor (V.fromList $ map Just ["Yes", "No", "No", "Yes"]) (V.fromList ["No", "Yes"]),
    MkFactor (V.fromList $ map Just ["Yes", "No", "No", "Yes"]) (V.fromList ["No", "Yes"]),
    MkFactor (V.fromList $ Nothing : map Just ["Yes", "No", "No", "Yes"]) (V.fromList ["No", "Yes"])
  ]

rFactorIncorrectLevelsExp :: [Factor]
rFactorIncorrectLevelsExp =
  [ -- 3 is missing from levels and will be converted to Nothing
    MkFactor (V.fromList [Just "1", Just "2", Nothing]) (V.fromList ["1", "2"]),
    -- levels incorrectly formatted.
    MkFactor (V.replicate 5 Nothing) (V.fromList ["0", "1", "2"]),
    -- pack . show is not the id from Text -> Text,
    -- as show for Text does some escaping and other transforms.
    MkFactor (V.replicate 4 Nothing) (V.fromList $ map (pack . show) ["No" :: Text, "Yes"])
  ]

{- Tests -}
tests :: TestTree
tests =
  testGroup
    "Variable module tests"
    [ testCase "Scalars reported correctly" $
        rVarScalarTypes @?= rVarScalarTypesExp,
      testCase "Vector types reported correctly" $
        rVarTypes @?= rVarTypesExp,
      testCase "Factor types reported correctly" $
        rVarFactorTypes @?= rVarFactorTypesExp,
      testCase "Stype types reported correctly" $
        rVarStypeTypes @?= rVarStypeTypesExp,
      -- Constructors and coercions
      testCase "Integer conversion out of Int32 range gives Nothing" $
        rVarIntOutOfBounds @?= rVarIntOutOfBoundsExp,
      testCase "invalid v_continuous_nonneg" $
        rVarStypeInvalidNonneg @? "as_v_continuous_nonneg should be Nothing",
      testCase "invalid v_count" $
        rVarStypeInvalidCount @? "as_v_count should have errored",
      testCase "invalid v_proportion" $
        rVarStypeInvalidProportion @? "as_v_proportion should be Nothing",
      testCase "factor construction, correct levels" $
        rFactorCorrectLevels @?= rFactorCorrectLevelsExp,
      testCase "factor construction, incorrect levels" $
        rFactorIncorrectLevels @?= rFactorIncorrectLevelsExp
    ]
