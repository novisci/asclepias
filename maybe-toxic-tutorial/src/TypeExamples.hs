module TypeExamples ( Hour(..) ) where

-- TODO what is a function, what is a type signature
-- wiki entry on type signatures
-- https://wiki.haskell.org/Type_signature
squareIt :: Double -> Double
squareIt x = x ^ 2

-- TODO basic functional programming (more in next tutorial!)
squareItToo :: Double -> Double
squareItToo = (^ 2)

-- TODO multiple types
checkPower :: Double -> Int -> Double
checkPower x y = x ^ y

-- TODO functional programming!
baseThree :: Int -> Double
baseThree = checkPower 3

-- Lists
-- plenty of things to do with lists in Data.List module
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:13
-- TODO

ones :: [Int]
ones = [1, 1]

-- build a list from scratch by pushing to the front of the list with :
-- your IDE should warn you this is a verbose way to do it
-- [1, 2, 3]
oneTwoThree :: [Int]
oneTwoThree = 1 : 2 : 3 : []

-- head, tail
-- 1
one :: Int
one = head oneTwoThree

-- [2, 3]
twoThree :: [Int]
twoThree = tail oneTwoThree

-- using these on an empty list will cause a run-time error
bad :: Int
bad = head []

-- concatenate lists with the *infix* operator ++
smash :: [a] -> [a] -> [a]
smash x y = x ++ y

-- use an infix operator like a function with two arguments by wrapping it in parentheses
smash' :: [a] -> [a] -> [a]
smash' = (++)

-- Maybe for error handling
-- TODO 'enum' or 'sum' types
-- Maybe a = Just a | Nothing

justFour :: Maybe Int
justFour = Just 4

-- consider 0 to be Nothing
-- incidentally, this allows you to define a function by matching on inputs
-- if you give it zero, it returns nothing. otherwise it moves to the next lineand returns Just x, where x is the input
justNotZero :: Int -> Maybe Int
justNotZero 0 = Nothing
justNotZero x = Just x


-- Destructuring
-- TODO pattern matching in function args

-- our version of head that handles empty lists more safely
-- haskell's base package has a function for this, called listToMaybe
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Maybe.html#v:listToMaybe
safetyFirst :: [a] -> Maybe a
safetyFirst []     = Nothing
safetyFirst (x:xs) = Just x

-- can use the anonymous parameter _ to match anything you don't later want to use on the right-hand side
safetyFirst' :: [a] -> Maybe a
safetyFirst' []    = Nothing
safetyFirst' (x:_) = Just x

-- destructuring also works with Maybe
-- here we undo justNotZero: if Nothing, return zero. if Just x we get back x.
-- the more general version of this is called fromMaybe
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Maybe.html#v:fromMaybe
intOrZero :: Maybe Int -> Int
intOrZero Nothing  = 0
intOrZero (Just x) = x


-- TODO typeclasses
-- (^) has type signature (Num a, Integral b) => a -> b -> a
-- which applies  to x ^ y where x is of class a and y of class b
--https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Real.html#v:-94-
-- value of classes: define for a range of input type, still controling for the
-- behvaior you care about
-- both x and result have
morePower :: Num a => a -> Int -> a
morePower x k = x ^ k

redundantPower :: (Num a, Integral b) => a -> b -> a
redundantPower x k = x ^ k


-- TODO newtypes
-- types are data, typeclasses are collections of types for which we can
-- define a common set of operations
-- e.g. Num, which requires as a 'minimual complete definition' +, -, * and a few other numerical operations to be defined
-- 'instances' of a typeclass such as Num are types that implement those operations
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Num
--
-- other common typeclasses are Eq, things that such that two objects of a given type can be checked for equality, and Show, which means they can be converted to String type and therefore printed in a console.

-- Defining a circle group, i.e. integers mod a certain number
-- https://en.wikipedia.org/wiki/Modular_arithmetic


-- if we are working with clocks a lot, to make our lives easier, we could
-- define a new type Hour as follows
-- 'deriving' allows us to automatically declare our new type as an instance of the typeclass Show here. Show, roughly, is for types that can be turned into strings. It is simple to derive here because Integer already is an instance of Show and this derivation just prepends the name of the type to whatever Integer prints
newtype Hour
  = Hour Integer
  deriving (Show)

-- integer circles have an associative operator which I'll write here as <> that takes one point on the circle and rotates it clockwise
-- so for example, say x is a point on the circle mod 12, thought of as a clock's hour hand. you can rotate x clockwise by 13 units to arrive at 1. rotating it by zero hours means x is unchanged.
-- such data structures are called Monoids (in math as in Haskell). Monoid is a typeclass that requires an associative group operation <> and an identity element, which in the Circle case is rotation by zero integers.
-- docs tell us what's required of a type that is an instance of Monoid
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Monoid.html

-- let's do it!

-- a Semigroup is a type with an associative operator <>
-- a Monoid is a Semigroup with an identity element mempty, such that x <>
-- mempty = x
instance Semigroup Hour where
  -- NOTE: destructuring here too
  (<>) (Hour x) (Hour y) = Hour (mod (x + y) 12)

instance Monoid Hour where
  mempty = Hour 0

-- often you need only define some of the operations required for a typeclass, as the core operations define yet more. for example, defining <> and mempty gives for free mconcat which takes a list of your type, applies <> successively and collects the results. try it in ghci on something like [Hour 2, Hour 3, Hour 24]
