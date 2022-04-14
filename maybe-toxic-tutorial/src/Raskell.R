# RASKELL
# The R version of a split tutorial translating silly things from Haskell to R
# and back, in hopes of creating a mental bridge between the two

# Run the provided examples in your IDE.

# INTRO
# The first difference you're likely to see between Haskell and R is the former
# is a compiled language, whereas the latter is a scripting language. This
# affects the tooling and actual execution of code, but that is not what this
# tutorial will cover.
 
# Instead, the focus here is on similarities and differences between Haskell
# and R in code syntax and logic. Each function written here will have a
# corresponding function in the Raskell.hs file. Names will be the same where
# possible, but sometimes we'll want multiple functions in R to do what a
# single function does in Haskell.

# The solutions presented here will try to be code you might actually want to
# write, but sometimes I will write inadvisable code to make a point of
# comparison.

# The .R and .hs files are intended to be viewed side-by-side. I am a big fan of window tiling!

# Pre-requisites

# These examples will touch on the basics, e.g. defining functions and types,
# but only very briefly so as to compare the two languages. Since this is
# intended for R programmers learning Haskell, the assumption is that you are
# comfortable with R and have at hand some basic Haskell resource, like the
# books "Learn you a Haskell" or "Haskell from First Principles".

# First section overview:

# The first part will emphasize writing R and Haskell code that looks very
# similar, without worrying too much about whether the code is actually what
# you want to write in a real application. We're just trying to establish
# answers to the question "How do I do this thing in {other language}?" This
# means that in many cases the logic of the R and Haskell code will differ
# substantially, since the languages have different strengths.

# Lists will play a big role here, as they are common to both languages.

# Second section overview:

# Since the languages are substantially different, solutions to a common
# problem will have very different approaches. For example, lists in R are
# often not what you want --- rectangular structures and vectors in particular
# are better for many tasks.

# This section will try to solve at least one problem, attempting to take a
# 'good' approach for each language.


##########
# PART ONE
##########

# A. IMPORTING LIBRARIES

# * import entire packages, e.g. dplyr
# * can import any package that has been installed
# * importing makes available all functions of the package
# * conflicts are handled by allowing the last thing loaded to take precedence
# * all installed libraries available by qualifying package_name::function_name

library(dplyr)

# B. FUNCTIONS AND ARGUMENTS

# * Functions can take any number of arguments, and ... allows for capturing
# and passing unspecified arguments to inner functions.
# * Lexical scoping: Function arguments are available by name within the
# function scope. Objects not passed as arguments are looked for in the calling
# scope, one level up. Objects defined within the function scope can be
# referenced by name
# * Functions can have side-effects, roughly meaning they change some object in
# the calling scope, e.g. the global environment.
# * Some functional programming: Functions can be passed as arguments,
# functions can return functions, etc.
# * All arguments must be passed to a function or given defaults. Supplying a
# partial set of arguments has no meaning and produces an error.
# * No pattern matching in function definitions

# B.1
# prepend an element to a list
# NOTE: in R, list elements can have any type (class). in Haskell they must all
# have the same type.
# With a basic check that xs is a list. You will not know about failures until
# you actually run this on data.

prepend <- function(xs, x) {
  stopifnot(is.list(xs))

  ys <- list(x)

  if (length(xs) != 0) {
    ys <- append(ys, xs)
  }

  ys
}

# B.2
# NOTE: Partial application in which one argument is fixed requires defining a
# new function, or using a utility like purrr::partial.  letters is a built-in
# R list of all lowercase letters
# again we add a check (evaluated when the program actually runs) to ensure
# we're appending only character to the letters vector

# Aside: In R the "character" class is a vector of strings. In haskell
# 'character' or Char type is a single element of a String, which is synonymous
# with [Char] a list of Char. This is confusing. This is confusing. In haskell,
# "ab" is a String and is the same as ['a', 'b']. Note the use of single vs.
# double quotations, which in R doesn't matter. In R, list("a", "b") is analogous to ["a", "b"].

prependToLetters  <- function(x) {
  stopifnot(is.character(x))

  prepend(as.list(letters), x)
}


# B.2 Examples

zfirst <- prependToLetters("z")

# throws error when run
bad <- prependToLetters(1)


# B.3
# prepend elements of ys to a list xs  with a Reduce operation
# using purrr::reduce, but base::Reduce would be fine as well
# first argument is the accumulator, the thing passed from one iterations to
# the next

# same as append(ys, xs)
myConcat <- function(xs, ys) {
  stopifnot(is.list(ys) && is.list(xs))

  # base::rev reverses the list, needed since reduce starts grabbing elements
  # at the back of the list
  purrr::reduce(rev(ys), prepend, .init = xs)
}

# B.3 Examples
oneToFive <- myConcat(as.list(4:5), as.list(1:3))

# B.4 
# map (or lapply) take a function and apply it to each element in a list
# as above, there is no guarantee in R that the function argument types are
# appropriate for the elements of the list, or even that the list has elements
# of all the same type.

# this wrapper function takes a list and two functions as an argument, and maps
# the composition of those functions in a single pass over the list

mapCompose  <- function(f, g, xs) {
  # the most basic check you can do for argument validity. it would be more
  # complex to guarantee the arguments of f and g are appropriate for what we
  # are trying to do.
  # this amount of extra code is needed to get a decent error message. if this
  # function were buried deep in your program the effort would be well worth
  # it.

  check  <- !c(is.function(f), is.function(g), is.vector(xs))
  names(check)  <- rlang::fn_fmls_syms()

  if (any(check)) {
    bad  <- names(check)[check]
    msg  <- sprintf("Arguments %s are not of the correct type.",
                    paste(bad, collapse = ", "))

    rlang::abort(msg)
  }

  # always returns a list
  purrr::map(xs, ~ f(g(.x)))
}

# B.4 Examples

# everything working as it should here

shiftOne <- function(x) {
  x + 1
}

reciprocal  <- function(x) {
  1/x
}

good <- mapCompose(reciprocal, shiftOne, as.list(0:10))

# works for both vectors and lists.
good <- mapCompose(reciprocal, shiftOne, 0:10)

# oops
bad <- mapCompose(reciprocal, shiftOne, letters[0:10])

# you know you've done this before. at least we get a helpful message here,
# since we wrote one.
bad_order  <- mapCompose(letters[1:10], reciprocal, shiftOne)

# B.5 
# Case handling and pattern matching basics
# basic if and switch

whatisit  <- function(x, isitcat = FALSE) {
  yes_fun  <- function(n) sprintf("This is a %s", n)
  no_fun  <- function(n) sprintf("This is not a %s", n)

  anmls  <- list("cat" = c("bobcat", "highland tiger"),
                 "marsupial" = c("tazmanian devil", "possum", "monito del monte")
  )

  if (isitcat) {
    ifelse(x %in% anmls$cat, yes_fun("cat"), no_fun("cat"))
  } else {
    ifelse(x %in% anmls$marsupial, yes_fun("marsupial"), no_fun("marsupial"))
  }
  
}

# ok so we want to make it more general
whatisit_general  <- function(x, anmls, anml_type) {
  yes_fun  <- function(n) sprintf("This is a %s", n)
  no_fun  <- function(n) sprintf("This is not a %s", n)

  nm <- names(anmls)

  if (anml_type %in% nm) {
    ifelse(x %in% anmls[[anml_type]], yes_fun(anml_type), no_fun(anml_type))
  } else {
    rlang::abort(c(sprintf("Invalid anml_type: %s", anml_type),
                   "i" = sprintf("For this anmls, should be one of: %s", paste(nm, collapse = ", "))
                   )
    )
  }
}

# maybe in some application a lookup table is basically what we need
# sticking with lists here
whatisit_tellme  <- function(x, anmls) {
  res  <- purrr::map_lgl(anmls, ~ x %in% .x)
  nm <- names(anmls)

  if (!any(res)) {
    NA_character_
  } else if (sum(res) > 1) {
    rlang::abort(sprintf("Bad anmls input: %s found in more than one element of anmls",
                         anml_type))
  } else {
    nm[res]
  }

}

# B.5 Example

# ok
not_marsupial  <- whatisit("falcon")
not_cat  <- whatisit("eagle", isitcat = TRUE)

# hmm, did we intend for this to run and produce an answer?
# UTF-8 encoding for "cat"
bad  <- whatisit(011000110110000101110100, isitcat = TRUE)

# slightly better
anmls <- list("mongoose" = c("meerkat", "kusimanse", "mongoose"),
              "bear" = c("grizzly bear", "panda"))

# ok
isitbear  <- whatisit_general("grizzly bear", anmls, "bear")

# typos mess everything up
# no error on this first one to alert you!
mistyped_x  <- whatisit_general("grizzly", anmls, "bear")
mistyped_name  <- whatisit_general("grizzly bear", anmls, "ber")

# our lookup function
isitbear  <- whatisit_tellme("grizzly bear", anmls)
mistyped_bear  <- whatisit_tellme("grizzly", anmls)

bad_anmls  <- anmls
bad_anmls$mongoose  <- c(anmls$mongoose, "panda")

bad_panda  <- whatisit_tellme("panda", bad_anmls)

# B.6
# More case handling
# Use switch to match patterns, or if-else statements.
# switch has a very limited pattern matching functionality: only numbers or
# strings can be matched

# In this example, we do two kinds of matching. Input function f will evaluate
# some data and return a model name. That model name will then be matched 
# with `switch` to determine which model should be run on the data.

# f will be wrapped in purrr::safely, to catch errors. if f fails with some
# error, we'll process that error in some way based on the third input to this
# function, a switch saying whether we want to run some default case.

run_model  <- function(data_checker, xs, with_default = FALSE) {

  # basic argument checking
  stopifnot(is.list(xs) && is.function(data_checker))

  safe_checker  <- purrr::safely(data_checker)

  chk <- safe_checker(xs)

  # dummy output for default model case
  default_model  <- 1:10

  # did the check fail with some error?
  if (!is.null(chk$error)) {
    if (!with_default) {
      rlang::abort("Data check failed.", parent = chk$error)
    }

    # some dummy default output
    default_model
  } else {
    switch(chk$result,
           # dummy 'expensive' model operation with redundant calculations.
           "expensive" = length(rev(xs)),
           # 'check' model. notice the return types might differ
           "cheap" = xs[1],
           # baseline case is to run the default model
           default_model
    )
  }

}
