{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module should test that the output shape of 'VariableWrapped' behaves
-- as expected. This is not an integration test, in which data would be read
-- in, processed via the 'CohortApp' and returned.
-- The 'NonEmpty VariableWrapped' within 'Cohort.Output.CohortJSON' is the only
-- element with non-trivial formatting of the output produced by a 'CohortApp'.
module Tests.Cohort.Output
  ( tests,
  )
where

import Data.Aeson
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Variable.R.SEXP
import Variable.R.Stype
import Variable.Variable

{- Values -}

-- RVector

-- Atomic: Only one atomic vector needs be tested. They're all similar.
rAtomic :: RTypeRep 'REALSXP
rAtomic = as_numeric (1.0 :: Double)

rAtomic' :: RTypeRep 'LGLSXP
rAtomic' = as_logical [True, False]

-- This one has an NA value
rAtomic'' :: RTypeRep 'LGLSXP
rAtomic'' = as_logical [Just True, Nothing]

rVarAtomic :: Variable
rVarAtomic = rVector "my_numeric" rAtomic

rVarAtomic' :: Variable
rVarAtomic' = rVector "my_logical" rAtomic'

rVarAtomic'' :: Variable
rVarAtomic'' = rVector "my_logical_withna" rAtomic''

rVarList :: Variable
rVarList =
  rVector "my_list" $
    as_list
      -- NOTE: These names override the vector names provided in rVector. Kept the
      -- same here for convenience in testing.
      [ ("my_numeric" :: Text, SomeRTypeRep rAtomic),
        ("my_logical", SomeRTypeRep rAtomic')
      ]

rVars :: VariableRow
rVars = [rVarAtomic, rVarAtomic', rVarAtomic'', rVarList]

rVarsOut :: Value
rVarsOut = toJSON $ map asVariableWrapped rVars

-- Stype vectors

stypeBinary :: Stype 'LGLSXP
stypeBinary = as_v_binary [True, False] a
  where
    a = MkStypeAttrs "my_binary" "my_binary_long" (Just StypeOutcome)

stypeNominal :: Stype 'STRSXP
stypeNominal = v_nominal (as_integer [1 :: Integer, 0, 3]) (V.map pack $ V.fromList ["0", "1", "3"]) a
  where
    a = MkStypeAttrs "my_nominal" "my_nominal_long" (Just StypeOutcome)

stypeVarNominal :: Variable
stypeVarNominal = stypeVector "my_stype_nominal" stypeNominal

stypeVarBinary :: Variable
stypeVarBinary = stypeVector "my_stype_binary" stypeBinary

stypeVars :: VariableRow
stypeVars = [stypeVarBinary, stypeVarNominal]

stypeVarsOut :: Value
stypeVarsOut = toJSON $ map asVariableWrapped stypeVars

{- Expected -}

-- RVector
rVarAtomicOutExp :: Value
rVarAtomicOutExp =
  object
    [ ("varTarget", String "RVector"),
      ("vals", Array $ V.fromList [Number 1.0]),
      ("attrs", object [("varType", "REALSXP"), ("varName", "my_numeric")]),
      ("subAttrs", Array V.empty)
    ]

rVarAtomicOutExp' :: Value
rVarAtomicOutExp' =
  object
    [ ("varTarget", String "RVector"),
      ("vals", Array $ V.fromList [Bool True, Bool False]),
      ("attrs", object [("varType", "LGLSXP"), ("varName", "my_logical")]),
      ("subAttrs", Array V.empty)
    ]

rVarAtomicOutExp'' :: Value
rVarAtomicOutExp'' =
  object
    [ ("varTarget", String "RVector"),
      ("vals", Array $ V.fromList [Bool True, Null]),
      ("attrs", object [("varType", "LGLSXP"), ("varName", "my_logical_withna")]),
      ("subAttrs", Array V.empty)
    ]

rVarListOutExp :: Value
rVarListOutExp =
  object
    [ ("varTarget", String "RVector"),
      ("vals", Array $ V.fromList [rVarAtomicOutExp, rVarAtomicOutExp']),
      ("attrs", object [("varType", "VECSXP"), ("varName", "my_list")]),
      ("subAttrs", Array V.empty)
    ]

rVarsOutExp :: Value
rVarsOutExp = Array $ V.fromList [rVarAtomicOutExp, rVarAtomicOutExp', rVarAtomicOutExp'', rVarListOutExp]

-- StypeVector
-- Note: Remember that no matter the SEXPTYPE input, the 'factor' backing a
-- 'v_nominal' is 'STRSXP', in keeping with R's implementation.
stypeVarNominalOutExp :: Value
stypeVarNominalOutExp =
  object
    [ ("varTarget", String "StypeVector"),
      ("vals", Array $ V.fromList [String "1", String "0", String "3"]),
      ("attrs", object [("varType", String "STRSXP"), ("varName", String "my_stype_nominal")]),
      ("subAttrs", suba)
    ]
  where
    suba =
      object
        [ ("short_label", String "my_nominal"),
          ("long_label", String "my_nominal_long"),
          ("study_role", String "StypeOutcome"),
          ("stypeType", String "v_nominal"),
          ("special_attrs", Array $ V.fromList [String "0", String "1", String "3"])
        ]

stypeVarBinaryOutExp :: Value
stypeVarBinaryOutExp =
  object
    [ ("varTarget", String "StypeVector"),
      ("vals", Array $ V.fromList [Bool True, Bool False]),
      ("attrs", object [("varType", String "LGLSXP"), ("varName", String "my_stype_binary")]),
      ("subAttrs", suba)
    ]
  where
    suba =
      object
        [ ("short_label", String "my_binary"),
          ("long_label", String "my_binary_long"),
          ("study_role", String "StypeOutcome"),
          ("stypeType", String "v_binary"),
          ("special_attrs", Array V.empty)
        ]

stypeVarsOutExp :: Value
stypeVarsOutExp = Array $ V.fromList [stypeVarBinaryOutExp, stypeVarNominalOutExp]

{- TESTS -}

tests :: TestTree
tests =
  testGroup
    "Unit tests on Cohort.Output"
    [ testCase "RVector output shape" $
        rVarsOut @?= rVarsOutExp,
      testCase "StypeVector output shape" $
        stypeVarsOut @?= stypeVarsOutExp
    ]
