-- IGNORE: i have explicitly turned off certain compiler warnings.
{-# HLINT ignore #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

  {--
     WHAT YOU WILL LEARN

      - types and type signatures: Double, Integer, Int, Char, String etc.
      - list basics
      - 'sum' types and a soft intro to the Maybe type
      - pattern matching in function definitions using types
      - basic self-defined types: using `newtype`, `data` and `type`
      - intro to type classes and their instances
      - type class constraints in function definitions

     PRE-REQUISITES

     This is the first and simplest of the tutorial example files, so you might
     be able to start here. It would not hurt to have looked at the first
     chapters of some intro text, like "Learn You a Haskell for Great Good" or
     "Haskell from First Principles."

     --}

-- IGNORE for now and skip to SIGNATURES section
module TypeExamples
  ( Hour(..)
  , Tree
  ) where


   {- SIGNATURES and basic types-}

-- Types define function arguments and outputs
-- every function *should* have a type signature
-- though often the type signature can be inferred

-- wiki entry on type signatures
-- https://wiki.haskell.org/Type_signature

-- documentation on the types in the Prelude, the module that typically is in
-- scope by default.
-- This documentation can be difficult to follow as a new haskell user, I
-- found. If you also find it difficult, stay here and turn back to it later.
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#g:1

-- A function taking a Double type value as argument and returning a Double
-- Signature syntax:
-- function name :: argument type -> output type
squareIt :: Double -> Double
squareIt x = x ^ 2

-- Multiple arguments are given types in turn. The last type in the signature
-- is always the output type
-- function name :: type of x -> type of y -> type of output
morePower :: Double -> Int -> Double
morePower x y = x ^ y

-- function name :: type of x -> type of y -> type of z -> type of output
morePowerPlus :: Double -> Int -> Double -> Double
morePowerPlus x y z = x ^ y + z

-- There are two different kinds of integers
-- Int, for limited-precision integers, with a valid range of [-2^29 .. 2^29-1]
-- Integer, for arbitrary-precision integers
whichIsInt :: Int -> Integer
whichIsInt x = toInteger x

-- Mismatched types in your function calls will generate errors when you try to
-- *compile* the program, rather than when you try to run it.
-- If your development environment is set up properly, with haskell-language-server,
-- when you uncomment this definition you should see a little error message
-- without needing to compile this module, like
-- "Couldn't match expected type ‘Double’ with actual type ‘Integer’"

-- whichIsInt passes an Integer type to squareIt, which takes a Double type
--intIsNotDouble :: Int -> Double
--intIsNotDouble x = squareIt (whichIsInt x)

-- Incidentally, we've used parentheses in the function definition to specify
-- the order of operations: whichIsInt x is evaluated first, and the result is
-- passed to squareIt. See more in the FunctionalExamples.hs module.


   {- LISTS -}

-- plenty of things to do with lists in Data.List module
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:13

-- A List of some type b is denoted [b]
ones :: [Int]
ones = [1, 1]

-- build a list from scratch by pushing to the front of the list with :
-- your IDE should warn you this is a verbose way to do it. This is in fact how
-- all lists in Haskell are implemented, as a so-called linked list.
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

-- WARNING: using these on an empty list will cause a run-time error
-- there is a better way to handle this that we'll see below.
bad :: Int
bad = head []

-- Get the value at some index with the bang-bang operator !!
-- WARNING: a runtime error if the index is out of range.
nthElem :: [Int] -> Int -> Int
nthElem xs n = xs !! n

-- indices start at zero
-- this is dumb. don't do this.
otherHead :: [Int] -> Int
otherHead xs = xs !! 0

-- concatenate lists with the *infix* operator ++
smash :: [Int] -> [Int] -> [Int]
smash x y = x ++ y

-- use an infix operator like a function with two arguments by wrapping it in parentheses
smash' :: [Int] -> [Int] -> [Int]
smash' x y = (++) x y


   {- STRINGS AND TYPE ALIASES -}

-- A Char in haskell is a character, the basic unit of text.
-- NOTE: literal characters are denoted with single quotation marks, not double
bestChar :: Char
bestChar = 'b'

-- The String type in haskell is constructed with double quotation marks
bestString :: String
bestString = "bb"

-- In fact, String is a *type alias* for a list of characters, [Char]

-- A type alias is simply a renaming of an existing type, for the convenience
-- of the programmer. Haskell treats it as identical to the type it renames.
bestString' :: [Char]
bestString' = "bb"

-- NOTE: all list operations therefore work on Strings
makeItBoo :: String -> String
makeItBoo s = s ++ "-boo"

-- "honey-boo"
honeyBoo :: String
honeyBoo = makeItBoo "honey"

-- Create your own type aliases with the type keyword

-- This is a *mental cue* that the integer value should be non-negative
-- It is no different from Integer, though, and so has no actual guarantees.
-- For a type-checked Natural number type, see Natural from GHC.Natural module
type Nat = Integer

-- This compiles, because Nat is the same as Integer to the compiler.
-- Note we've done nothing to actually ensure the outcome is non-negative: Nat
-- is just an alias for Integer.
whichIsNat :: Int -> Nat
whichIsNat = whichIsInt


   {- Generic types, first look at type classes -}

-- Often we want a placeholder for *any* type in our function signature
-- so that the function is "generic" over that argument.

-- 'b' is a placeholder for any type
myIdentity :: b -> b
myIdentity x = x

-- Generic functions are good because they save us from defining the same
-- function for each type.

-- Rewriting some of our list functions to be more generic
nthElem' :: [a] -> Int -> a
nthElem' xs n = xs !! n

smash'' :: [a] -> [a] -> [a]
smash'' x y = x ++ y

-- The problem is not all operations work on all types. The functions nthElem'
-- and smash'' are valid because the operations ++ and !! are valid for lists
-- of *any* type. However, that is not true for arithemtic addition.

-- This will not compile because the compiler has no way to check whether a
-- value of type b can be added to another.

--doPlus :: b -> b -> b
--doPlus x y = x + y

-- This is where *TYPECLASSES* come into play.
-- Think of a typeclass as a collection of methods.
-- A type is an instance of a typeclass if it has implementations of all of the
-- required methods. This is a similar concept to interfaces in C++, Go and
-- other languages, or Traits in Rust.

-- We can constrain the type to be general *so long as it is an instance of a
-- particular typeclass*. That way, we can use methods that are valid for any
-- instance of the type class, without specifying which concrete type we are
-- applying it to.
--
-- For example, the Num class is one that requires a (+) operator to be
-- defined, among other things. See documentation here, in particular the
-- 'minimal complete definition' giving the set of methods that must be defined
-- for an instance of the class. Often, classes will provide methods that can
-- be derived from the required ones and hence do not need to be defined
-- separately.
--
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Num

-- Typeclass constraints are given in function signatures to the left of the
-- double arrow =>, as follows.
-- The typeclass constraint in your function signature must be compatible with
-- that of the methods and functions you are using.
doPlus :: (Num a) => a -> a -> a
doPlus x y = x + y

-- Show is the class for types that can be converted to strings by defining a
-- 'show' function. This demonstrates how to combine multiple constraints.
showDoPlus :: (Num a, Show a) => a -> a -> String
showDoPlus x y = show (doPlus x y)

-- make morePower generic over the type x, instead of requiring Double
-- look at the type signature for ^ to see that this is OK.
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:-94-
morePower' :: Num a => a -> Int -> a
morePower' x k = x ^ k

-- Compare this type signature to that in the documentation for ^. This
-- function is just an alias for ^ now and is as generic as it can be.
morePower'' :: (Num a, Integral b) => a -> b -> a
morePower'' x k = x ^ k

   {- CUSTOM TYPES with NEWTYPE and DATA -}

-- You can write your own types that are in principle just aliases but that the
-- compiler will treat as different when checking types.
-- You might do this in the case of Nat above, when you want to build out more
-- Nat-specific methods that can more safely assume a Nat value is
-- non-negative.

-- to do so, use the newtype keyword instead of type
newtype Natl
  = MkNatl Integer

-- This no longer compiles.
-- Compare to whichIsNat. Uncomment to get the error.
-- Though Nat' is just an Integer, Haskell treats it as a different type when type-checking.

--whichIsNatl :: Int -> Natl
--whichIsNatl = whichIsInt

-- MkNatl acts as a 'constructor', meaning a function to convert Integer to
-- Natl. Often the constructor will be given the same name as the type (so
-- MkNat' might be Natl), but here I name them differently to avoid confusion.
-- NOTE: all types and type constructors must begin with capital letters.

-- One benefit of writing such a newtype is that we can write additional
-- constructors to make a Natl from an Integer *that guarantee* in some way the
-- Natl created is non-negative.

-- e.g. by truncation at zero
toNatlTrunc :: Integer -> Natl
toNatlTrunc x = MkNatl (max 0 x)


   {- BASIC 'SUM' and 'PRODUCT' TYPES -}

-- A sum type is a type that can take exactly one of multiple possible subtypes.
-- Each of the subtypes is called a 'variant'. Sum types are called enums in
-- some languages.

-- A core example is the Maybe type, explored below. It has two variants: Just
-- a for any type a, or Nothing, which holds no value.

-- sum types are declared with the data keyword
-- variants are separated by `|`, read 'or'.

data MyMaybe a
  = MyJust a
  | MyNothing

-- This type has two constructors, instead of just the one MkNatl for Natl.
-- Note: use _ as placeholder for an argument if it is not used in the function
-- definition.
alwaysNothing :: a -> MyMaybe a
alwaysNothing _ = MyNothing

-- this dumb example is *just* a wordy alias for MyJust, for exposition
alwaysJust :: a -> MyMaybe a
alwaysJust x = MyJust x

-- Here's a Sum type representing colors of the rainbow
-- NOTE: These are all type constructors, with no values.
data Rainbow = R | O | Y | G | B | I | V

-- Product types:
-- These can also be defined with the data keyword. They represent types with
-- multiple values, like this common example of a 2D Point.

data Point = MkPoint Double Double

-- Combining sum and product
-- You can combine these types, and make them recursive even. The classic
-- example is that of a binary tree, generic over any type 'a'.

-- each Node has two branches. Leaf is the tip of the tree.
-- NOTE: this can accomodate multiple kinds of binary trees.
-- eg a tree with structure Node (Leaf a) (Node (Leaf
-- a) (Leaf a)), or one in which the number of vertices is guaranteed to be a
-- power of 2.
-- https://en.wikipedia.org/wiki/Binary_tree#Types_of_binary_trees

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

-- NOTE: often the typeclass implementations are easy for
-- the compiler to derive automatically, such as Show
-- (converting the type to string). See below for how to
-- declare a type as an instance of a given typeclass
-- with custom definitions.

   {- PATTERN MATCHING ON SUM and PRODUCT TYPES -}

-- See the FunctionalExamples.hs file for other control-flow examples, e.g.
-- guards and if-else.

-- Sum types make for easy case-handling. We will match the pattern of the type
-- by variant, which is possible since a sum type can only be one of its
-- variants. This is cleaner to read than if-then-else statements, and requires
-- little to no computation.

-- read it like this:
-- if Rainbow is R, say "RED", if O say "ORANGE" ...
-- NOTE: remember R, O, Y ... are the constructors of the variant in this type.
-- We are matching using the constructors.
sayColor :: Rainbow -> String
sayColor R = "RED"
sayColor O = "ORANGE"
sayColor Y = "YELLOW"
sayColor G = "GREEN"
sayColor B = "BLUE"
sayColor I = "INDIGO"
sayColor V = "VIOLET"


-- Matches are handled in order, from top to bottom.
-- If R return True, otherwise False
isItRed :: Rainbow -> Bool
isItRed R = True
-- could use _ instead of x here
isItRed x = False

-- Since matches are evaluated top to bottom, this is always false
-- You should get a warning here from your editor telling you the second line
-- is redundant, since the first line matches every possible case.
neverRed :: Rainbow -> Bool
neverRed x = False
neverRed R = True

-- You can also pattern match on particular values, not just on types.
zeroToOne :: Int -> Int
zeroToOne 0 = 1
zeroToOne x = x


-- 'Destructuring' in pattern matches and Product matches
-- In the case above, the variants of Rainbow had no values.
-- However, Point *does* contain values. You can use pattern matching to get the values out.

-- This converts a Point to the tuple type, another common one.
-- NOTE: the pattern match happens using the constructor. This also was the
-- case for sum types. Rainbow's constructors are R, O, Y ..., just as Point's
-- single constructor is MkPoint.
pointToTuple :: Point -> (Double, Double)
pointToTuple (MkPoint x y) = (x, y)

-- However, variants for Tree *do* contain values. You can use pattern matching
-- to get the values out of the variants.

-- This function will recurse through the Tree, adding the values together as
-- it goes. Generic over all types for which + is valid.
treeSum :: Num a => Tree a -> a
treeSum (Leaf x    ) = x
treeSum (Node t1 t2) = treeSum t1 + treeSum t2


   {- PATTERN MATCHING ON LISTS -}

-- Pattern matching works also with the list constructors `:` ('cons') and [],
-- the empty list.

-- Get the first of a list of integers, or return the default 0
-- Just for exposition: such a function is not likely to be a good idea.
-- x : xs matches a list that is non-empty and has first element x.
-- NOTE: xs here might be [] but x must be a value, hence this pattern *only* matches non-empty lists.
headOrZero :: [Int] -> Int
headOrZero []       = 0
headOrZero (x : xs) = x

-- match patterns can continue in this fashion
-- the _ in x:y:_ is again the arbitrary placeholder for a variable we do not
-- use on the right-hand-side
firstTwoOrNothing :: [a] -> [a]
firstTwoOrNothing []          = []
firstTwoOrNothing (x : y : _) = [x, y]

-- Convert a list to a binary Tree
-- NOTE: you can use 'literal' matches for lists, such as [x], instead of x:[],
-- to denote a list with exactly one element.
-- Since [x] appears before x:xs, the latter will only match lists with at
-- least two elements, since the case in which xs = [] was handled first.

-- NOTE: this creates a particular 'proper' binary tree. See wikipedia article
-- linked near the Tree definition.

-- NOTE: this throws an error at run time if called on an empty list. Not what
-- you want probably. You can fix this in the exercises.
listToTree :: [a] -> Tree a
listToTree []       = undefined
listToTree [x     ] = Leaf x
listToTree (x : xs) = Node (Leaf x) (listToTree xs)


   {- MAYBE wrapper type example -}

-- See how Maybe is constructed in the Sum/Product types example above.
-- data Maybe a = Just a | Nothing

-- Maybe is very common as a way to handle safely any operation that might fail
-- or have negligible output.

-- We'll do much more with this type and similar ones in the
-- MaybeEitherExamples module

-- Here, we rewrite `head` to be 'safe', in the sense that our program will not
-- fail if run on an empty list, and any function that receives the output of
-- `head` will need to explicitly deal with the case in which head was called
-- on an empty list.

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x


   {- TYPECLASSES: DEFINING CLASSES AND INSTANCES -}

-- How do you specify that some custom type is an instance of a particular
-- typeclass, e.g. Num, so that you can use it with the appropriate typeclass
-- methods, e.g. (+)?

-- NOTE: type aliases (declared with the type keyword) inherit the type class
-- instances of the types they are renaming. 

-- some can be derived automatically, like the ... deriving (Show) example above.

-- Let's derive Show ourselves for the Natl type.
-- Start by checking the minimally required methods for this typeclass in the docs.
-- For Show, we need to define showsPrec OR show
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Show
instance Show Natl where
   -- on the left-hand side we desctructure to get the underlying Integer value
   -- out of our Natl type. on the right-hand side we use the fact that Integer
   -- x is itself an instance of show. This is *not* recursion. It is using
   -- different definitions of show on both sides of =.
  show (MkNatl x) = show x

-- Eq also is easy and could be derived
instance Eq Natl where
  (MkNatl x) == (MkNatl y) = x == y

-- NOTE: there are no type signatures on the methods. Those are supplied by the
-- class definition. See below.


-- Num is more complex, but the same ideas apply: Use the underlying type's
-- instances. Unlike Show, Num is not derivable.
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Num

-- PITFALL: Notice the Num instance declares several rules its methods should
-- abide by. It is up to *you* to guarantee those rules hold. In the example
-- below, we inadvertently wrote a method that *doesn't abide by the class
-- rules listed in the docs* because the underlying type Integer can be
-- positive or negative. Can you spot which one(s)?
--
-- This should be a clue that you need to rethink whether Natl can be an
-- instance of Num, without some kind of redesign. The compiler doesn't save
-- you here.
instance Num Natl where
  (MkNatl x) + (MkNatl y) = toNatlTrunc (x + y)
  (MkNatl x) * (MkNatl y) = toNatlTrunc (x * y)
  abs (MkNatl x) = MkNatl (abs x)
  -- Natl can only be zero or positive ... in principle
  signum (MkNatl 0) = 0
  signum (MkNatl x) = 1
  fromInteger = toNatlTrunc
  negate _ = MkNatl 0


-- Define your own typeclass with the class keyword.

-- Let's say you want to define the class of types that can be associated to
-- some color in Rainbow.

-- after the 'where' statement, you define *only the type signatures* of the
-- methods that are in your minimal complete definition. Any user declaring
-- some instance must define these methods for themselves.
-- Default methods, once that can apply to any member for which the required
-- methods are defined, can be defined just as any other function.

class Colorable a where
   toColor :: a -> Rainbow
   isRed :: a -> Bool
   isRed x = isItRed (toColor x)


-- paint all leaves red, and everything else is yellow
instance Colorable (Tree a) where
  toColor (Leaf _  ) = R
  toColor (Node _ _) = B

-- we get isRed for free
yesItIs :: Bool
yesItIs = isRed (Leaf 1)


   {- EXTENDED EXAMPLE: DEFINING A CIRCLE -}

-- Defining a circle group, i.e. integers mod a certain number
-- https://en.wikipedia.org/wiki/Modular_arithmetic


-- if we are working with clocks a lot, to make our lives easier, we could
-- define a new type Hour as follows
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
