
   {--
 RASKELL
 The Haskell version of a split tutorial translating silly things from Haskell
 to R and back, in hopes of creating a mental bridge between the two

 INTRO
 The first difference you're likely to see between Haskell and R is the former
 is a compiled language, whereas the latter is a scripting language. This
 affects the tooling and actual execution of code, but that is not what this
 tutorial will cover.

 Instead, the focus here is on similarities and differences between Haskell and
 R in code syntax and logic. Each function written here will have a
 corresponding function in the Raskell.hs file. Names will be the same where
 possible, but sometimes we'll want multiple functions in R to do what a single
 function does in Haskell.

 The solutions presented here will try to be code you might actually want to
 write, but sometimes I will write inadvisable code to make a point of
 comparison.

 These examples will touch on the basics, e.g. defining functions and types,
 but only very briefly so as to compare the two languages. Since this is
 intended for R programmers learning Haskell, the assumption is that you are
 comfortable with R and have at hand some basic Haskell resource, like the
 books "Learn you a Haskell" or "Haskell from First Principles".

 First section overview:

 The first part will emphasize writing R and Haskell code that looks very
 similar, without worrying too much about whether the code is actually what
 you want to write in a real application. We're just trying to establish
 answers to the question "How do I do this thing in {other language}?" This
 means that in many cases the logic of the R and Haskell code will differ
 substantially, since the languages have different strengths.

 Lists will play a big role here, as they are common to both languages.

 Second section overview:

 Since the languages are substantially different, solutions to a common
 problem will have very different approaches. For example, lists in R are
 often not what you want --- rectangular structures and vectors in particular
 are better for many tasks.

 This section will try to solve at least one problem, attempting to take a
 'good' approach for each language.
   --}


   {--
      PART ONE
      --}
   
{-- A. IMPORTING LIBRARIES

 * import specific modules not entire packages, e.g. Data.List not base package
 * can import any module from a package listed in a project's .cabal file
 * importing makes available all functions of the module
 * nothing is available (except Prelude module) unless imported, not even after qualification
 * import qualified ModuleName as M will make all functions in ModuleName available if first qualified with "M.", e.g. the function f can be used as M.f
 * conflicts are not allowed and must be resolved by importing a module as qualified
--}

import Data.List
import qualified Control.Applicative as A
import qualified GHC.Arr as Arr
-- from collections package
import qualified Data.Map as M


   {-- B. FUNCTIONS AND ARGUMENTS
      
      * Function arguments must be named or pattern-matched
      * No equivalent to R's ...
      * Lexical scoping is similar in broad strokes to R: Definiing
      within-scope variables is handled with `where` and `let` statements
      within the function definition.
      * Functions in general are 'pure': they cannot modify objects outside the
      function scope (no side-effects), and a given set of inputs will always
      produce the same output
      * Functional programming: Each function is made up of building blocks of
      functions that binds a single input name in the function body. Functions
      with multiple arguments are built by composition of these simple
      functions. See the Lambda Calculus overview here:
      http://dev.stephendiehl.com/fun/lambda_calculus.html
      * As a result of the functional programming structure, a function given
      only some of its arguments returns another function, with the values of
      those arguments fixed. This is called 'currying'
      
      --}

-- B.1
-- prepend an element to a list
-- NOTE: this just aliases the Prelude function `:`, called 'cons'. You do not
-- need to define such a function. This is just for comparison with the R
-- version.
-- This type signature says for any type a, given an element of type a and a
-- list of a return a list of a. Supplying arguments that do not conform to
-- this type signature will lead to a failure to even *compile* the program.

prepend :: [a] -> a -> [a]
prepend xs x = x : xs

-- B.2
-- NOTE: Currying means prepend [1, 2, 3] gives a function calling prepend with
-- the first argument fixed. See the type signature.
-- constants are defined in the same way as functions
-- .. is range syntax. see http://learnyouahaskell.com/starting-out, Texas Ranges
-- This only works now for prepending Char type inputs now

-- Aside: In R the "character" class is a vector of strings. In haskell
-- 'character' or Char type is a single element of a String, which is synonymous
-- with [Char] a list of Char. This is confusing. In haskell, "ab" is a String
-- and is the same as ['a', 'b']. Note the use of single vs. double quotations,
-- which in R doesn't matter. In R, list("a", "b") is analogous to ["a", "b"].

letters :: [Char]
letters = ['a'..'z']

-- No need to specify the remaining input names
prependToLetters :: Char -> [Char]
prependToLetters = prepend letters

-- B.2 Examples

zfirst = prependToLetters 'z'

-- uncommenting this will prevent the program from compiling, rather than when
-- the program actually is run as in R
-- check for yourself by running in the project directory 'cabal build'

-- bad = prependToLetters 1


-- B.3 
-- prepend elements of ys to a list xs with foldr
-- NOTE: 'folds' are a flexible and very useful class of 'reduce'-like operations
-- to skip on first reading: https://wiki.haskell.org/Foldr_Foldl_Foldl' 
-- You would not write this. Haskell has a built in concat operator `++` and function `concat`
-- flip reverses the order of the arguments of prepend
-- used because foldr assumes the function it is given has the accumulator in
-- the second argument not the first

-- same as ys ++ xs
-- foldr peels off elements from the back of ys
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = foldr (flip prepend) xs ys


-- B.3 Examples
oneToFive = myConcat [4..5] [1..3]


-- B.4
-- Haskell also has a map that works much the same way
-- see the type signature for the map functional by typing :t map in ghci
-- map :: (a -> b) -> [a] -> [b]
-- this says take a function mapping *any* type a to some
-- type b, then take a list of type a elements (written
-- [a]) and return a list with type b elements (written
-- [b])

-- compare that to the type signature of mapCompose below

-- this simple type-checking will buy us a lot of safety
-- relative to the R method, but also constrains us. in
-- particular, mapCompose will not work for data
-- structures other than lists: the R version worked for
-- lists or vectors.
--
-- notice that unlike the R implementation i have to do *zero* extra coding for
-- the type checks. i get all of that for free.

mapCompose :: (b -> c) -> (a -> b) -> [a] -> [c]
mapCompose f g xs = map (f . g) xs

-- tip: see the functional programming tutorial for why
-- you can leave out the last argument here and get the same function
mapCompose' :: (b -> c) -> (a -> b) -> [a] -> [c]
mapCompose' f g = map (f . g)

-- this version allows us to make it more general
-- 'Traversable' is a type-class, which means an object
-- that implements a particular set methods.  in
-- particular, the `fmap` method for Traversable allows us
-- to do something like map for any data structure that
-- can be 'traversed'. It will work for lists but also
-- many other data structures, including custom-made ones.

-- there is an extra cost in abstraction, but in the end
-- this version maintains the type safety of map while
-- vastly expanding its use cases.

traverseCompose :: Traversable t => (b -> c) -> (a -> b) -> t a -> t c
traverseCompose f g = fmap (f . g)


-- B.4 Examples

-- Unlike the R implementation, you care about the output
-- / input types. here we do an explicit conversion
shiftOne :: Int -> Double
shiftOne x = fromIntegral (x + 1)

reciprocal :: Double -> Double
reciprocal x = (1 / x)

xs :: [Int]
-- [20, 30, 40, 50]
xs = map (*2) [2..5]

good = mapCompose reciprocal shiftOne xs

-- these give a *compile-time* error
-- uncomment to see
--bad = mapCompose reciprocal shiftOne letters
--badOrder = mapCompose letters reciprocal shiftOne


-- using traverseCompose on an array, an indexed data
-- structure in haskell. first argument gives index bounds.

xs' :: Arr.Array Int Int
xs' = Arr.listArray (1, 4) xs

good' = traverseCompose reciprocal shiftOne xs'


-- B.5 
-- case handling and pattern matching

-- trying to match the R function closely
-- to demonstrate if/else etc.
-- instead of a list we use a Map from the containers
-- library

-- using case of and if/else, the most directly comparable
-- this has all of the flaws of the R version, except argument type checking
-- you don't want to do this
whatisit :: String -> Bool -> String
whatisit x cat = 
   let 
     catList = ["bobcat", "highland tiger"]
     marsList = ["tazmanian devil", "possum", "monito del monte"]
     checkCat x' = if x' `elem` catList 
                   then "This is a cat"
                   else "This is not a cat"
     -- 'guard' pattern can make for cleaner syntax. can do multiple cases
     checkMars x'
       | x' `elem` marsList = "This is a marsupial"
       | otherwise = "This is not a marsupial"
   in 
     if cat then checkCat x else checkMars x


-- whatisitGeneral
-- In this case Haskell lets us do something a little better than the R implementation.
-- So long as the programmer knows ahead of time what the anmls (named list of
-- animal types) they need
--
-- we'll define a data type enumerating all of the animal types and holding a
-- list of animals of that type
-- | means a value of this type can either be a Mongoose with a list of mongoose animals
-- or Bear with a list of bear animals

-- now the function is very clean and simple. there is no need to handle the
-- missing key type of error because the compiler will prevent you from passing
-- an Anmls that is incorrect

-- you'll see however that this isn't a good design for a program, really

data Anmls = Mongoose [String] | Bear [String] deriving (Show, Eq)

whatisitImproved :: String -> Anmls -> String
whatisitImproved z' (Mongoose zs') = yesNo z' "mongoose" zs'
whatisitImproved z' (Bear zs') = yesNo z' "bear" zs'

yesNo :: String -> String -> [String] -> String
yesNo z msg zs
  | z `elem` zs = "This is a " ++ msg
  | otherwise = "This is not a " ++ msg

-- whatisitTellMe
-- instead of a named list we'll use a list of key-value tuples of type
-- (String, [String]), an animal class name and a list of animals in the class
--
-- you could instead use Map from the collections package (imported above for
-- your convenience)
--
-- output will wrap errors in the Either structure: Left is for a captured
-- error and Right for an Ok result

-- lookups with return another haskell wrapper type, Maybe
-- if a value is not found it will return Nothing, else it will return Some name

-- e.g. if the program is in error because x is found in multiple locations, 
-- this function returns Left "Bad anmls input: x found in multiple locations."
-- if x is not found anyway, it returns Right Nothing
-- else it returns Right "some name"

-- here, there is a pattern match in the function arguments for an empty list
whatisitTellMe :: String -> [(String, [String])]  -> Either String (Maybe String)
whatisitTellMe _ [] = Right Nothing
-- this pattern match is the standard one for a list with first element y and
-- remaining list ys (which can be [])
whatisitTellMe x (y:ys) 
  | length chk > 1 = Left "Bad anmls input: x found in multiple locations."
  | otherwise = Right (headMaybe chk)
   where 
      -- NOTE: there *is* a lookup function, but it searches by the key not the
      -- value
      chk = filterMap (isIn x) fst (y:ys)
      -- is z' in the list of values zs'?
      isIn z' (_, zs') = z' `elem` zs'

-- helpers for whatisitTellMe, broken out for more general use
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = foldr (\z zs -> if p z then f z : zs else zs) []

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (y:_) = Just y

-- B.5 Examples

-- is a falcon a marsupial? an eagle a cat?
notMarsupial, notCat :: String
notMarsupial = whatisit "falcon" False
notCat = whatisit "eagle" True

-- type error caught immediately
--badMarsupial :: String
--badMarsupial = whatisit 011000110110000101110100 False

bears :: Anmls
bears = Bear ["grizzly bear", "panda"]

isitbear :: String 
isitbear = whatisitImproved "grizzly bear" bears

-- compiler error!
-- I can't even create something for Fox because that is not one of the
-- variants of Anmls type, let alone try to use it in whatisitImproved
--foxes :: Anmls
--foxes = Fox ["red fox"]
--isitfox = whatisitImproved "red fox" foxes

-- doesn't save us from typos though
mistypedX :: String
mistypedX = whatisitImproved "grizzly" bears

mongeese :: Anmls
mongeese = Mongoose ["meerkat", "kusimanse", "mongoose"]

-- and this is one big reason why the design here was bad.
-- you have to manually ensure the input x makes sense to check in the Anmls
-- list.
mistypedName :: String
mistypedName = whatisitImproved "grizzly bear" mongeese

anmls :: [(String, [String])]
anmls = [("bear", ["grizzly bear", "panda"]), ("mongoose", ["meerkat", "kusimanse", "mongoose"])]

isitbear' :: Either String (Maybe String)
isitbear' = whatisitTellMe "grizzly bear" anmls 

-- still not immune to typos of course
-- to improve this a little, whatisitTellMe could take a custom data type for
-- the keys, as with whatisitImproved

-- however, the use or Either and Maybe mean that any
-- functions using the results from this one will have an
-- easier time of identifying failures without causing
-- unexpected problems down the line or just crashing the
-- program
mistypedBear :: Either String (Maybe String)
mistypedBear = whatisitTellMe "sumkinnaburr" anmls

-- at least this kind of error is better handled

badAnmls :: [(String, [String])]
badAnmls = [("bear", ["grizzly bear", "panda"]), ("mongoose", ["panda", "meerkat", "kusimanse", "mongoose"])]

badPanda :: Either String (Maybe String)
badPanda = whatisitTellMe "panda" badAnmls
