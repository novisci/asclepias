{-|
   At a minimum, project programmers will need to specify @ToOutput@ instances
   for their Monarch testing routines. This class provides a single method, for
   transforming project-relevant @input@ to @output@.

   The class also enforces the notion that test routines should be defined by
   their types: There can be only one @ToOutput@ instance for a given pair of
   @input@ and @output@. Doing so was a choice to have the type system
   determine the test as much as possible.

   The rationale is as follows: Monarch aims to allow non-programmers to
   provide a direct line to test specification via csv file inputs and ouputs.
   Project programmers need only specify the schema for the internal inputs and
   outputs, and a @toOutput@ transformation for getting from one to the other.
   If Monarch were to allow more than one transformation per pair of types,
   there would need to be some additional configuration to allow
   non-programmers to specify the transformation. Such a configuration could
   have details that are difficult to resolve and difficult for those who did
   not write the code to reason about.

   Of course, if needed programmers could circumvent this intentional
   limitation by defining newtype wrappers around existing types.

   The @Testable@ constraint alias is one every @input@ and @output@ type to be
   tested should adhere to. It requires types to be convertible from the
   internal flat csv-like representation, @[TestMap]@, and to be instances of
   @ToOutput@.
     -}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.Monarch.ToOutput
  ( Testable
  , ToOutput(..)
  ) where

import           Test.Monarch.TestMap
import           Witch.TryFrom

-- | Class defining the conversion from inputs to outputs to be tested. See
-- Testable.
class ToOutput input output where
  toOutput :: input -> output

-- | Context synonym for an @input@ and @output@ pair that can be constructed
-- from the general flat TestMap and run through @toOutput@. @input@ is a Haskell
-- representation of a type serving as input to a function to be tested, given
-- in @toOutput@, and @ouput@ is the resulting type of the conversion.
--
-- The @TryFrom@ instances allow @input@ and @output@ to be constructed from
-- textual input via @[TestMap]@, rather than having the Haskell programmer
-- specify them.
--
-- Note one need only implement @TryFrom TestMap a@ for some @a@, and @TryFrom
-- [TestMap] [a]@ is provided automatically.
type Testable input output
  = (TryFrom [TestMap] input, TryFrom [TestMap] output, ToOutput input output)
