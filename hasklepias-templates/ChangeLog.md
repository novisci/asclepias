# Changelog for hasklepias-templates

## 0.2.0

* Restructures templates source code to use `hs`/`adoc`
style of documentation (like `hasklepias-example`)
instead of `md`/`lhs`/`markdown-unlit`.

## 0.1.1

* Streamlines the utilities for creating test cases for `Definition` builder functions.
The general case is handled by `makeTestCase`,
which takes a `TestName`,
a tuple of the builder arguments,
a tuple of the `Definition` inputs,
and a value for the `Definition` return type to create a `TestCase`.
A more specific function is `makeTestCaseOfIndexAndEvents`,
which specializes `makeTestCase` to taking a pair of `a`s
(to be converted to an `Interval a`)
and a list of `Events a` in place a tuple of the `Definition` inputs.
Note that if the builder only takes a single argument to create a `Definition`
(as in `buildIsEnrolled`),
the argument needs to be wrapped in the `OneTuple` type
in order for the constraints on the test evaluator functions to be satisfied.
`OneTuple` is an instance of `Applicative`,
thus one can use `pure` to create the `OneTuple`.
* Adds the `makeTestGroup` function which takes a name,
a builder function,
and a list of `TestCase`s to create the `TestTree` needed
to run the tests in the `Tasty` framework.

## 0.1.0

* Split from `core`.
