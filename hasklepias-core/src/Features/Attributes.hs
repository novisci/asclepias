{-|
Module      : Functions for defining attributes Feature data
Description : Defines attributes instances for Features.

The @'Attributes'@ type contains metadata that can be attached
to a @'Features.Core.Feature'@.
Importantly, for a @'Features.Core.Feature'@ to be cast
to a @'Features.Featureable.Featurable'@,
it *must* have a @'HasAttributes'@ instance defined.
When a @'Features.Featurable.Featurable'@ is encoded as JSON,
the @'Attributes'@ are included as metadata in the output.

Several template Haskell functions are provided to make defining instances easier:

* 'setAttributes'
* 'setAttributesEmpty'
* 'setManyAttributes'
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Features.Attributes
  ( HasAttributes(..)
  , Attributes(..)
  , Role(..)
  , Purpose(..)
  , emptyAttributes
  , basicAttributes
  , emptyPurpose

  -- ** Template Haskell Utilities
  , setAttributes
  , setAttributesEmpty
  , setManyAttributes
  ) where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( KnownSymbol )
import           Language.Haskell.TH     hiding ( Role )
import           Language.Haskell.TH.Syntax
                                         hiding ( Role )

{-|
A type to identify a feature's (i.e. variable's) role in a research study.
In a @'Purpose'@, multiple roles can be specified.
-}
data Role =
    Outcome
  | Censoring
  | Covariate
  | Exposure
  | Competing
  | Weight
  | Intermediate
  | Unspecified
  deriving (Eq, Ord, Show, Generic, Lift)

-- NOTE:
-- The types for roles and tags should really be Set 
-- to ensure unique elements.
-- However, Set is not currently an instanece of Lift
-- (https://hackage.haskell.org/package/template-haskell-2.18.0.0/docs/Language-Haskell-TH-Syntax.html).
-- Rather than defining an instance of Lift for Set here,
-- which is an indrect place to do so,
-- the types in Purpose have been changed to List.
{-|
A type to identify a feature's purpose.
The @'Role'@ type enumerates many common purposes,
and the @getTags@ field can be used for additional information. 
-}
data Purpose = MkPurpose
  { getRole :: [Role]
  , getTags :: [Text]
  }
  deriving (Eq, Show, Generic, Lift)

{-|
A data type for holding attritbutes of features.

Attributes are not generally used with asclepias itself,
and instead used to pass contextual information to downstream applications.

For example, the @Attributes@ type directly maps to a 
[stype context](https://docs.novisci.com/stype/reference/context.html).

See 'emptyAttributes' and 'basicAttributes' for convenience constructor functions.

-}
data Attributes = MkAttributes
  {
    -- | A short, text label
    getShortLabel :: Text
    -- | A longer label
  , getLongLabel  :: Text
    -- | Used as a textual description for how the feature was derived
  , getDerivation :: Text
    -- | A @'Purpose'@
  , getPurpose    :: Purpose
  }
  deriving (Eq, Show, Generic, Lift)

-- | An empty purpose value. 
emptyPurpose :: Purpose
emptyPurpose = MkPurpose mempty mempty

-- | An empty attributes value.
emptyAttributes :: Attributes
emptyAttributes = MkAttributes "" "" "" emptyPurpose

-- | Create attributes with just short label, long label, roles, and tags.
basicAttributes
  :: Text -- ^ short label
  -> Text -- ^ long label
  -> [Role] -- ^ @'Purpose'@ roles
  -> [Text] -- ^ @'Purpose'@ tags
  -> Attributes
basicAttributes sl ll roles tags = MkAttributes sl ll "" (MkPurpose roles tags)

{-|
A typeclass providing a single method for attaching @Attributes@
to a @name@ and @d@ata.
The type of @d@ata is determined by the @name@,
by way of using a 
 [functional dependency](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/functional_dependencies.html).

The default method is @'emptyAttributes'@.

The 'setAttributes' function creates a template Haskell splice,
which generates a @HasAttributes@ instance declaration.
For example, instead of writing

@ 
  instance HasAttributes "foo" Bool where
    getAttributes = basicAttributes "lab" "long label" [Covariate] []  
@

one can instead write

@
  setAttributes
    (basicAttributes "lab" "long label" [Covariate] []) 
    "foo" ''Bool 
@

The latter approach is useful for writing helper functions
to generate @'Attributes'@.

@
   covariateAttrs label tag  = 
       setAttributes (labeller label tag) 
       where labeller = basicAttributes label label [Covariate] [tag] 
   
   covariateAttrs "foo var" "a" "foo" ''Bool
   covariateAttrs "bar bar" "b" "bar" ''Int
@

-}
class (KnownSymbol name) => HasAttributes name d | name -> d where
  getAttributes :: forall name . Attributes
  getAttributes = emptyAttributes

{-|
Creates a template haskell splice for @'HasAttributes'@ instance.
See @'HasAttributes'@ for an example.

Usage requires the 
 [template haskell language extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html).

-}
setAttributes
  :: Attributes
  -- ^ an @Attributes@ value
  -> String
  -- ^ The name the feature as a @String@.
  -- This refers to the @KnownSymbol name@ 
  -- in the @'HasAttributes'@ class declaration.
  -> Name
  -- ^ The type of data.
  -- Use double ticks as in @''Bool@.
  -- This is template haskell's quotation syntax.
  -- See 'Language.Haskell.TH.Syntax.Name' in template haskell documentation 
  -- for other ways to create a @Name@.
  -> Q [Dec]
setAttributes attrs name ty = [d|
  instance HasAttributes $a $b where
     getAttributes = attrs
  |]
  where
    a = litT ( strTyLit name )
    b = conT ty

{-|
A convenience function for declaring a @HasAttributes@ instance
as 'emptyAttributes'.
-}
setAttributesEmpty :: String -> Name -> Q [Dec]
setAttributesEmpty = setAttributes emptyAttributes

{-|
A convenience function for declaring many @HasAttributes@ instances
given a list of inputs to 'setAttributes'.
-}
setManyAttributes :: [(String, Name, Attributes)] -> Q [Dec]
setManyAttributes x = fmap concat (traverse f x)
  where f (name, typ, attrs) = setAttributes attrs name typ
