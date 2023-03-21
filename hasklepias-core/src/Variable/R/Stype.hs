-- | Model of R `stype` types within Haskell.

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Variable.R.Stype where

import           Data.Aeson        (ToJSON (..), Value)
import           Data.Kind         (Type)
import           Data.Text         (Text)
import qualified Data.Vector       as V
import           GHC.Generics      (Generic)
import           Variable.R.Factor
import           Variable.R.SEXP

-- TODO these should be consolidated with Features.Role

-- | Indication of variable role.
data StypeRole = StypeCovariate | StypeOutcome deriving (Eq, Generic, Show)

-- | User-provided stype common attributes. 'SEXPTYPE' and name information is
-- purposefully ommitted, since that will appear in
-- 'Variable.Attributes.VariableAttrs'.
data StypeAttrs
  = MkStypeAttrs
      { -- | A short, text label.
        short_label :: Text
        -- | A longer label.
      , long_label  :: Text
        -- | Indication of variable's statistical purpose, either 'Covariate'
        -- or 'Outcome'.
      , study_role  :: Maybe StypeRole
      }
  deriving (Eq, Generic, Show)

-- | Internal. Wrapper for exporting attributes to JSON or text for debugging. Pulls
-- together 'special_attrs' as 'Value' and attaches the 'stype_type'. We don't
-- expose this to the user because we want them to provide only the minimal set
-- of attributes given in 'StypeAttrs'.
data WrappedStypeAttrs
  = MkWrappedStypeAttrs
      { -- | A short, text label.
        short_label   :: Text
        -- | A longer label.
      , long_label    :: Text
        -- | Indication of variable's statistical purpose, either 'Covariate'
        -- or 'Outcome'.
      , study_role    :: Maybe StypeRole
        -- | Stype type name. Underlying SEXPTYPE 's' in 'Stype s' will be
        -- provided at the top level 'VariableAttrs'.
      , stypeType     :: Text
        -- | Catch-all for specialized attributes, e.g. 'levels' of the
        -- underlying 'Factor' for 'Nominal'.
      , special_attrs :: Value
      }
  deriving (Eq, Generic, Show)

-- | Internal. Utility to construct 'WrappedStypeAttributes' provided a Stype
-- variant name.
wrappedStypeAttrs :: Stype r -> WrappedStypeAttrs
wrappedStypeAttrs (Binary _ a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_binary" (toJSON ())
wrappedStypeAttrs (Continuous _ a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_continuous" (toJSON ())
wrappedStypeAttrs (ContinuousNonneg _ a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_continuous_nonneg" (toJSON ())
wrappedStypeAttrs (Count _ a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_count" (toJSON ())
wrappedStypeAttrs (Nominal fv a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_nominal" (toJSON sa)
  where sa = V.toList $ levels fv
wrappedStypeAttrs (Ordered fv a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_ordered" (toJSON sa)
  where sa = V.toList $ levels fv
wrappedStypeAttrs (Proportion _ a) = MkWrappedStypeAttrs a.short_label a.long_label a.study_role "v_proportion" (toJSON ())

-- | Haskell representatives of vector types from the [`stype` R
-- package](https://docs.novisci.com/stype/reference/index.html). Parameterized
-- by the "prototype" 'SEXP' backing the type. For types such as
-- 'ContinuousNonneg', with constrained domains not represented in the standard
-- Haskell types, the constraints are enforced in the `v_*` constructors, as in
-- `stype`.
--
-- 'Stype' vectors must always be created with their corresponding
-- 'StypeAttrs'. However, to reduce the overhead in some cases, they can be
-- constructed with `as_v_*` functions calling the appropriate `as_*` coercion
-- functions from 'Variable.R.SEXP' to create the underlying 'RTypeRep'
-- directly from some other Haskell type.
--
-- To work with 'Stype' vectors directly, you will want `DataKinds`.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings -XDataKinds
-- >>> import Variable
-- >>> :{
-- >>> src_dbl :: [Maybe Double]
-- >>> src_dbl = Nothing : map Just [1, 0.5, 0]
-- >>> ctx = MkStypeAttrs "important_var" "A very important variable" Nothing
-- >>> v = as_v_proportion src_dbl ctx
-- :}
-- >>> v
-- Proportion [Nothing,Just 1.0,Just 0.5,Just 0.0] MkStypeAttrs {short_label =
-- "important_var", long_label = "A very important variable", study_role =
-- Nothing}
data Stype :: SEXPTYPE -> Type where
  Binary :: RTypeRep 'LGLSXP -> StypeAttrs -> Stype 'LGLSXP
  Continuous :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP
  ContinuousNonneg :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP
  Count :: RTypeRep 'INTSXP -> StypeAttrs -> Stype 'INTSXP
  Nominal :: Factor -> StypeAttrs -> Stype 'STRSXP
  Ordered :: Factor -> StypeAttrs -> Stype 'STRSXP
  Proportion :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP

{- JSON -}

-- NOTE: You do not want a ToJSON for Stype, since that will be handled by the
-- top-level Variable.

instance ToJSON StypeRole
instance ToJSON StypeAttrs
instance ToJSON WrappedStypeAttrs

-- Note: This requires UndecideableInstances, I think because of the type
-- family in the constraint which (as stated in the docs) could in principle
-- expand with arbitrary logic. it doesn't here, so this should not cause ghc
-- to hang.
instance (Show (SEXPElem s)) => Show (Stype s) where
  show (Binary v a)           = "Binary " ++ show v ++ " " ++ show a
  show (Continuous v a)       = "Continuous " ++ show v ++ " " ++ show a
  show (ContinuousNonneg v a) = "ContinuousNonneg " ++ show v ++ " " ++ show a
  show (Count v a)            = "Count " ++ show v ++ " " ++ show a
  show (Nominal v a)          = "Nominal " ++ show v ++ " " ++ show a
  show (Ordered v a)          = "Ordered " ++ show v ++ " " ++ show a
  show (Proportion v a)       = "Proportion " ++ show v ++ " " ++ show a

  {- CONSTRUCTORS -}

-- | Build a 'Binary' from @RTypeRep 'LGLSXP@ and StypeAttrs. Aliases
-- constructor in this case.
v_binary :: RTypeRep 'LGLSXP -> StypeAttrs -> Stype 'LGLSXP
v_binary = Binary

-- | Construct a 'Binary' from any 'a' that can be converted to @RTypeRep
-- 'LGLSXP@.
as_v_binary :: (AsLogical a) => a -> StypeAttrs -> Stype 'LGLSXP
as_v_binary v = v_binary (as_logical v)

-- | Build a 'Continuous' from @RTypeRep 'REALSXP@ and 'StypeAttrs'. Aliases
-- constructor.
v_continuous :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP
v_continuous = Continuous

-- | Construct a 'Continuous' from any 'a' that can be converted to @RTypeRep
-- 'REALSXP@.
as_v_continuous :: (AsNumeric a) => a -> StypeAttrs -> Stype 'REALSXP
as_v_continuous v = v_continuous (as_numeric v)

-- | Build a 'ContinuousNonneg' from @RTypeRep 'REALSXP@ and 'StypeAttrs'.
-- This converts negative elements to NA, unlike the R function with the same
-- name, which throws an error.
v_continuous_nonneg :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP
v_continuous_nonneg = ContinuousNonneg . V.map op
  where op Nothing = Nothing
        op (Just x) = if x < 0 then Nothing else Just x

-- | Attempt to construct a 'ContinuousNonneg' from any 'a' that can be
-- converted to @RTypeRep 'REALSXP@.
as_v_continuous_nonneg :: (AsNumeric a) => a -> StypeAttrs -> Stype 'REALSXP
as_v_continuous_nonneg v = v_continuous_nonneg (as_numeric v)

-- | Build a 'Count' from the @RTypeRep 'INTSXP@ and 'StypeAttrs'.  This
-- converts negative elements to NA, unlike the R function with the same name,
-- which throws an error.
v_count :: RTypeRep 'INTSXP -> StypeAttrs -> Stype 'INTSXP
v_count = Count . V.map op
  where op Nothing = Nothing
        op (Just x) = if x < 0 then Nothing else Just x

-- | Attempt to build a 'Count' from any 'a' that can be converted to @RTypeRep
-- 'INTSXP@.
as_v_count :: (AsInteger a) => a -> StypeAttrs -> Stype 'INTSXP
as_v_count v = v_count (as_integer v)

-- | Build a 'Nominal' with the provided 'levels' as second argument. The
-- @RTypeRep r@ will be converted to a 'Factor' via 'factor'.
v_nominal :: (RTypeRepConstraints r) => RTypeRep r -> V.Vector Text -> StypeAttrs -> Stype 'STRSXP
v_nominal rv lvls = Nominal (factor rv lvls)

-- | Build a 'Ordered' with provided 'levels' as second argument. The 'RTypeRep
-- r' will be converted to a 'Factor' via 'ordered'. 
v_ordered :: (Ord (SEXPElem r), RTypeRepConstraints r) => RTypeRep r -> V.Vector Text -> StypeAttrs -> Stype 'STRSXP
v_ordered rv lvls = Ordered (ordered rv lvls)

-- | Build a 'Proportion' from @RTypeRep 'REALSXP@ and 'StypeAttrs'. This
-- converts elements less than 0 or greater than 1 to NA, unlike the R function
-- with the same name, which throws an error.
v_proportion :: RTypeRep 'REALSXP -> StypeAttrs -> Stype 'REALSXP
v_proportion = Proportion . V.map op
  where op Nothing = Nothing
        op (Just x) = if x < 0 || x > 1 then Nothing else Just x

-- | Attempt to build a 'Proportion' from an 'a' that can be converted to
-- @RTypeRep 'REALSXP@.
as_v_proportion :: (AsNumeric a) => a -> StypeAttrs -> Stype 'REALSXP
as_v_proportion v = v_proportion (as_numeric v)

  {- UTILITIES -}

-- | Cast a 'Stype r' to its underlying 'RTypeRep r'. Aliases 'stypeRTypeRep'
-- for the sake of matching the `stype` package API.
as_canonical :: Stype r -> RTypeRep r
as_canonical = stypeRTypeRep

-- | Cast a 'Stype r' to its underlying 'RTypeRep r'.
stypeRTypeRep :: Stype r -> RTypeRep r
stypeRTypeRep sv = case sv of
                     Binary rv _           -> rv
                     Continuous rv _       -> rv
                     ContinuousNonneg rv _ -> rv
                     Count rv _            -> rv
                     Nominal rv _          -> values rv
                     Ordered rv _          -> values rv
                     Proportion rv _       -> rv

-- | Extract the SEXPTYPE and attributes of a Stype.
stypeInfoOf :: Stype r -> (Text, StypeAttrs)
stypeInfoOf v = case v of
                  Binary rv sattrs -> (textSEXPTYPEOf rv, sattrs)
                  Continuous rv sattrs -> (textSEXPTYPEOf rv, sattrs)
                  ContinuousNonneg rv sattrs -> (textSEXPTYPEOf rv, sattrs)
                  Count rv sattrs -> (textSEXPTYPEOf rv, sattrs)
                  Nominal f sattrs -> (textSEXPTYPEOf (values f), sattrs)
                  Ordered f sattrs -> (textSEXPTYPEOf (values f), sattrs)
                  Proportion rv sattrs -> (textSEXPTYPEOf rv, sattrs)
