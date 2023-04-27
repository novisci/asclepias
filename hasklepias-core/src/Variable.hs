-- | This module provides an interface for producing a 'VariableRow', the
-- return type of 'Cohort.runVariables'. Its goals are to:
--
--   - provide Haskell types that mirror types of downstream applications
--   ingesting the JSON produced from the data in 'Cohort.runVariables', such
--   that 'Variable' values within a 'VariableRow' can be seamlessly converted
--   to the target type values via JSON.
--   - define the shape of JSON produced from a 'VariableRow' for downstream
--   applications to rely on.
--
--   Users should only wrap Haskell values in the 'Variable' type at the end of
--   the 'Cohort.runVariables' computations, using one of the constructors
--   provided, such as 'rVector'. 'Variable' wraps each of the supported target
--   type systems for inclusion in the inhomogeneous list 'VariableRow'.
--
--   However, in some cases it can be convenient to work directly with the
--   target type system representations of this module. See 'RTypeRep' for
--   details.
module Variable
  ( -- * Variable type
    Variable (..),
    VariableRow,
    VariableConstraints,

    -- ** Variable constructors
    rVector,
    rFactor,
    stypeVector,
    rAtomicVectorElem,

    -- * Supported downstream type representations

    -- ** R vectors
    SEXPTYPE (..),
    SEXPElem,
    SomeRTypeRep (..),
    RTypeRep,
    RTypeRepConstraints,

    -- ** R factors, ordered and unordered
    Factor (..),
    factor,

    -- ** Stype vectors
    Stype,
    StypeAttrs (..),
    StypeRole (..),

    -- *** Constructors
    v_binary,
    v_continuous,
    v_continuous_nonneg,
    v_count,
    v_nominal,
    v_ordered,
    v_proportion,

    -- * Utilities

    -- ** For casting to R vectors
    AsRTypeRep (..),
    AsLogical,
    AsInteger,
    AsNumeric,
    AsComplex,
    AsCharacter,
    as_logical,
    as_integer,
    as_numeric,
    as_character,
    as_list,

    -- ** For manipulating R vectors
    sort,
    sortUniq,
    summarizeWith,

    -- ** For casting to Stype vectors
    as_v_binary,
    as_v_continuous,
    as_v_continuous_nonneg,
    as_v_count,
    as_v_proportion,

    -- ** For type-level programming
    withSomeRTypeRep,
    sexpTypeOf,
    sexpTypeOfErased,
  )
where

import Variable.Constraints
import Variable.R.Factor
import Variable.R.SEXP
import Variable.R.Stype
import Variable.Variable
