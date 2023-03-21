-- | Module defining the output types for 'Cohort.runVariables'.

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Variable.Variable where

import           Data.Aeson          (ToJSON (..), Value)
import           Data.Singletons
import           Data.Text
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import           Variable.Attributes (VarAttrs (..))
import           Variable.R.Factor
import           Variable.R.SEXP
import           Variable.R.Stype

-- | 'Variable', the element type of 'VariableRow', exists to ensure as much as
-- possible that the values computed within an `asclepias` application can be
-- seamlessly converted to one of the supported target type systems downstream.
-- At present, the conversions all pass through JSON as an intermediary.
--
-- Each supported target has an associated variant of 'Variable'. See the
-- variant-specific documentation for details.
--
-- Programmers building an `asclepias` application are intended to wrap their
-- computations in 'Variable' only at the final step of the computations in
-- 'Cohort.runVariables'. 'Variable' intentionally erases the underlying types
-- it wraps, so as to allow an inhomogeneous list in 'VariableRow'. That makes
-- it inconvenient to work with 'Variable' directly in most cases.
--
-- However, users might find it helpful to work with the underlying target
-- types that 'Variable' wraps, such as the base R vectors represented by
-- @RTypeRep r@. See the documentation for the respective types for details.
--
-- To construct a 'Variable', users will use the smart constructors for each
-- target type, such as 'rVector'. Each supported target is represented by one
-- of the variants of 'Variable'.
--
--
-- ==== __Examples__
--
-- Example of converting a list of '[Bool]' to the target R logical vector and
-- wrapping in 'Variable' all in one step.
--
-- The printed output shape shown here is determined by the internal type
-- 'VariableWrapped', which provides the JSON output shape, and is used only
-- for debugging or logging. It contains target-dependent attributes and type
-- information needed to interpret values of this 'Variable' in downstream
-- applications, as read from JSON.
--
-- >>> :set -XOverloadedStrings
-- >>> import Variable
-- >>> :{
-- >>> myVar :: Variable
-- >>> myVar = rVector "myVar" $ as_logical [True, False]
-- >>> :}
-- >>> myVar
-- MkVariableWrapped {varTarget = "RVector", vals = Array [Bool True,Bool
-- False], attrs = MkVarAttrs {varType = "LGLSXP", varName = "myVar"}, subAttrs
-- = Array []}
--
-- Example of using R vector representations for interim computations,
-- before wrapping the final value in 'Variable'. R representations are
-- 'Data.Vector.Vector' s, so you can use all of the utilities of that module.
--
-- Since R has no singleton types, e.g. 'Integer', only vectors, summarizing
-- functions such as 'Data.Vector.maximum' must wrap the results of their
-- computation in a vector again. The utility 'Variable.R.SEXP.summarizeWith'
-- is provided to do so as a convenience.
--
-- See 'RTypeRep' for more on how to use R-related types.
--
-- >>> :set -XOverloadedStrings -XDataKinds
-- >>> import Variable
-- >>> import qualified Data.Vector as V
-- >>> :{
-- >>> ageAtEvent :: RTypeRep 'REALSXP
-- >>> ageAtEvent = as_numeric [51 :: Double, 30, 60]
-- >>> maxAgeVar :: Variable
-- >>> maxAgeVar = rVector "maxAgeVar" $ summarizeWith V.maximum ageAtEvent
-- >>> :}
-- >>> maxAgeVar
-- MkVariableWrapped {varTarget = "RVector", vals = Array [Number 60.0], attrs
-- = MkVarAttrs {varType = "REALSXP", varName = "maxAgeVar"}, subAttrs = Array
-- []}
--
data Variable where
  -- | A subset of base R vector types, those listed among 'SEXPTYPE', backed
  -- by the Haskell types given in 'RTypeRep'. A 'Variable' intended for this
  -- target should be constructed with 'rVector'.
  RVector :: (RTypeRepConstraints r) => RTypeRep r -> VarAttrs -> Variable
  -- | The unordered @factor@ type in R, backed by the 'Variable.R.Factor.Factor'
  -- type in Haskell and constructed with 'rFactor'.
  RFactor :: Factor -> VarAttrs -> Variable
  -- | The ordered @factor@ type in R, backed by the 'Variable.R.Factor.Factor'
  -- type in Haskell and constructed with 'rOrdered'.
  ROrdered :: Factor -> VarAttrs -> Variable
  -- | Vectors defined in the R @stype@ package, backed by some
  -- 'Variable.R.Stype.Stype' and constructed with 'stypeVector'. All but
  -- @v_rcensored@ is supported.
  StypeVector :: (RTypeRepConstraints r) => Stype r -> VarAttrs -> Variable
  -- | Element of an 'RTypeRep r'. R has no singleton type for its atomic
  -- vectors. However, it is sometimes convenient when processing data row-wise
  -- to indicate to the downstream consumer that the 'Variable' should be a
  -- singleton, to be wrapped in the associated 'SEXPTYPE' vector indicated by
  -- parameter 'r' of the input. It is up to the user to ensure 'r' is not
  -- @'VECSXP@.
  RAtomicVectorElem :: (RTypeRepConstraints r) => SEXPElem r -> VarAttrs -> Variable

-- | A 'VariableRow' is the output type of 'runVariables' and is the collection
-- of computed values associated with each 'ObsUnit' of a 'Cohort'. It can be
-- thought of as a row of data for a given observational unit in a given
-- cohort, with each component 'Variable' giving a particular column's value
-- for that observational unit.
--
-- See 'Variable' for details.
--
-- ==== __JSON shape__
--
-- This section describes the JSON shape produced by a single 'VariableRow'.
-- The JSON will include one element of this shape for each 'Cohort.ObsUnit'.
-- See the top-level 'Hasklepias' module documentation for an overview of the
-- full JSON output shape produced by an @asclepias@ cohort-building
-- application.
--
-- JSON output from a 'VariableRow' is an @array@, each element of which is an
-- @object@ with shape demonstrated by the following example:
--
-- @
-- {
--   "varTarget": "StypeVector",
--   "attrs": {
--     "varName": "ageAtIndex",
--     "varType": "INTSXP"
--   },
--   "subAttrs": {
--     "long_label": "Age at day of index, computed from January 7 of smallest provided birth year.",
--     "short_label": "Age at day of index",
--     "special_attrs": [
--       "91"
--     ],
--     "study_role": null,
--     "stypeType": "v_nominal"
--   },
--   "vals": [
--     91
--   ]
-- }
-- @
--
--   - "varTarget" identifies the supported target type this data was
--   constructed from, corresponding to one of the 'Variable' variant names.
--   - "attrs" is an @object@ with @string@ fields defining the variable name
--   and target variable type. In this example, the target is an @integer@
--   vector in R with name "ageAtIndex".
--   - "subAttrs" is an @object@ that can vary based on the "varTarget". A
--   "StypeVector" takes additional context. If the "varTarget" were "RVector",
--   indicating one of the base R 'SEXPTYPE' s, this field would be @null@.
--   - "vals" contains the values of the 'Variable'. At present, only R-related
--   vector types are supported and hence "vals" will always be an @array@. The
--   JSON type of elements in this array will differ based on the "varTarget"
--   and "varType".  At present, elements will either be of one of the atomic
--   JSON types (@null@, @bool@, @number@, @string@) or will be of the same
--   shape as the JSON 'Variable' displayed above, in the case of @"varType":
--   "VECSXP"@, representing an R list.
type VariableRow = [Variable]

  {- CONSTRUCTORS -}
-- | Constructor for 'RVector' with the given name as first argument,
-- automatically producing type attribute information. To produce an
-- 'RTypeRep', use of the of @as_*@ constructors or produce one directly by
-- constructing the 'Data.Vector.Vector a' with appropriate 'a', as determined
-- by 'RTypeRep'.
rVector :: (RTypeRepConstraints r) => Text -> RTypeRep r -> Variable
rVector nm rv = RVector rv a
  where a = MkVarAttrs (textSEXPTYPEOf rv) nm

-- | Constructor for 'RFactor' with the given name as first argument, 'values'
-- as second argument and 'levels' as third.  Calls 'factor', which is
-- associated to an unordered @factor@ in R. Note the 'varType' always is
-- @STRSXP@ and not the 'SEXPTYPE' of the input, in keeping with the R
-- implementation in which 'factor' variables are backed by character vectors.
rFactor :: (RTypeRepConstraints r) => Text -> RTypeRep r -> V.Vector Text -> Variable
rFactor nm rv lvls = RFactor (factor rv lvls) a
  where a = MkVarAttrs "STRSXP" nm

-- | Constructor for 'ROrdered with the given name as first argument, 'values'
-- as second argument and 'levels' as third. Calls 'ordered', which is
-- associated to an ordered @factor@ in R. Note the 'varType' always is
-- @STRSXP@ and not the 'SEXPTYPE' of the input, in keeping with the R
-- implementation in which 'factor' variables are backed by character vectors.
rOrdered :: (Ord (SEXPElem r), RTypeRepConstraints r) => Text -> RTypeRep r -> V.Vector Text -> Variable
rOrdered nm rv lvls = RFactor (ordered rv lvls) a
  where a = MkVarAttrs "STRSXP" nm

-- | Constructor for 'StypeVector' with the given name as first argument. To
-- produce a 'Stype', use one of the @v_*@ constructors such as
-- 'Variable.R.Stype.v_binary' or 'Variable.R.Stype.as_v_binary'.
stypeVector :: (RTypeRepConstraints r) => Text -> Stype r -> Variable
stypeVector nm sv = StypeVector sv a
  where a = MkVarAttrs ty nm
        (ty, _) = stypeInfoOf sv

rAtomicVectorElem :: (RTypeRepConstraints r) => Text -> SEXPElem r -> Variable
rAtomicVectorElem nm e = RAtomicVectorElem e a
  where a = MkVarAttrs ty nm
        ty = textSEXPTYPEOfElem e

  {- Show, JSON etc -}

-- | Internal. Container to control Show and ToJSON shapes.
data VariableWrapped
  = MkVariableWrapped
      { varTarget :: Text
        -- ^ "Target" of the variable, meaning which variant of 'Variable' the
        -- data are wrapped in.
      , vals      :: Value
        -- ^ Values of the variable.
      , attrs     :: VarAttrs
        -- ^ Common attritubutes required for all 'Variable' s.
      , subAttrs  :: Value
        -- ^ Any additional attributes lumped into a JSON 'Value'.
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON VariableWrapped

-- NOTE Show and ToJSON instances for Variable are defined only in terms of
-- VariableWrapped, which is the latter's entire purpose.
instance Show Variable where
  show = show . asVariableWrapped

instance ToJSON Variable where
  toJSON = toJSON . asVariableWrapped

  {- UTILITIES -}

-- Helper utilities for nested RVector lists.
atomicRVectorAsWrapped :: (RTypeRepConstraints s) => RTypeRep s -> VarAttrs -> VariableWrapped
atomicRVectorAsWrapped rv a = MkVariableWrapped "RVector" (toJSON rv) a (toJSON ())


rVectorAsWrappedA :: (RTypeRepConstraints s) => Sing s -> RTypeRep s -> VarAttrs -> VariableWrapped
rVectorAsWrappedA sxp rv a = case sxp of
                     SLGLSXP    -> atomicRVectorAsWrapped rv a
                     SINTSXP    -> atomicRVectorAsWrapped rv a
                     SREALSXP   -> atomicRVectorAsWrapped rv a
                     SCPLSXP    -> atomicRVectorAsWrapped rv a
                     SSTRSXP    -> atomicRVectorAsWrapped rv a
                     -- Must recurse through the list to produce a
                     -- 'VariableWrapped' (converting it then to JSON) for each
                     -- element. Types erased within 'SomeRTypeRep' are
                     -- recovered.
                     SVECSXP -> MkVariableWrapped "RVector" (toJSON $ V.map (\case
                                                                           Nothing -> Nothing
                                                                           Just (nm, v) -> Just $ withSomeRTypeRep v (\rv' -> rVectorAsWrappedA (singOfRTypeRep rv') rv' (MkVarAttrs (textSEXPTYPEOfErased v) nm))) rv) a (toJSON ())

rVectorAsWrapped :: forall s. (RTypeRepConstraints s) => RTypeRep s -> VarAttrs -> VariableWrapped
rVectorAsWrapped = rVectorAsWrappedA (sing @s)

-- | Internal. Convert a 'Variable' to its structured output shape for JSON and String
-- targets, 'VariableWrapped'.
asVariableWrapped :: Variable -> VariableWrapped
asVariableWrapped (RVector rv a) = rVectorAsWrapped rv a
asVariableWrapped (RFactor rv a) = MkVariableWrapped "RFactor" (toJSON $ values rv) a (toJSON $ levels rv)
asVariableWrapped (ROrdered rv a) = MkVariableWrapped "ROrdered" (toJSON $ values rv) a (toJSON $ levels rv)
asVariableWrapped (StypeVector sv a) = MkVariableWrapped "StypeVector" (toJSON $ stypeRTypeRep sv) a (toJSON $ wrappedStypeAttrs sv)
asVariableWrapped (RAtomicVectorElem e a) = MkVariableWrapped "RAtomicVectorElem" (toJSON e) a (toJSON ())

-- | Internal. Used for testing. Extract the attributes of a 'Variable'.
varAttrs :: Variable -> VarAttrs
varAttrs (RVector _ a)           = a
varAttrs (RFactor _ a)           = a
varAttrs (ROrdered _ a)          = a
varAttrs (StypeVector _ a)       = a
varAttrs (RAtomicVectorElem _ a) = a
