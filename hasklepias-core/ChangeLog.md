# Changelog for hasklepias-core

## 0.24.0

* Changes underlying type of `Criteria` to a list instead of a NonEmpty list.
Use of NonEmpty is an artifact of an older way of evaluating criteria
that's no longer necessary.
The old way processing cohorts didn't have the SubjectHasNoIndex status,
so a user always need to provide at least one criterion
which would handle the case of not having an index,
or else all the subjects we could included.
Now if no criteria are provided,
all units will either have a `SubjectHasNoIndex` or `Included` status.
* Changes the underlying type of a `Criterion` from a `Feature n Status` to
simply `(Text, Status)`.
In this way, the `Cohort` module no longer depends on the `Feature` module.
One can now create `Criterion` without needing to define a `Feature`.
This change better delineates the abstraction
between the `Cohort` and `Feature` modules.
For those cases where a `Feature` will be used from a `Criterion`,
a `From` instance is provided that casts a `Feature n Status` to a `Criterion`.
For example:

```haskell
x :: Feature "foo" Status
x = pure Include

y :: Criterion
y = into @Criterion x -- MkCriterion ("foo", Include)
```

* The `FeatureN` type has been removed from the `Feature` module,
as they only place that type was used was in the `Cohort.Criteria` module.
* Adds `From` instances to get to an `IndexSet i`
from an `i`, `Maybe i`, and `[i]`.

## 0.23.0

* Moves specification of test suite into `src` directly.
Also, swaps out `HSpec` framework for `tasty` framework.
* Removes `FeatureEvents` module. Functions were either removed entirely; 
moved (and possibly renamed) to `event-data-theory`; or moved to `Haskelpias.Misc`
* The evaluation logic of mapping a `Population` into a `Cohort` is changed completely.
Previously, a cohort's specification functions where run on all subjects in sequence:
get all the indices for all subjects,
get all the criteria for all indices, and so on.
Now, there is a `makeSubjectEvaluator` function that
can apply all of a cohort's logic to a single subject,
then the `makeCohortEvaluator` maps
over subjects in a population to get the cohort output.
This approach makes it much easier to reason about the code
and to parallelize subject processing
if we want to do that at some point in the future.
* Adds the `CohortEvalOptions` type which holds
options that can be passed to the evaluators that modify the evaluation logic,
such as:
  * `EvaluateFeatures` which determines whether
  and how a `CohortSpec`'s `runFeatures` function is evaluated.
  * `SubjectSample` which determines which subjects will be evaluated.
  This is can be used to filter to certain subjects, for example.
* The `AttritionInfo` type now includes both the number of subjects processed
as well as the number of observational units processed.
* The `Index` type has been removed altogether.
Now, an "index" is simply any type that is an instance of `Ord`.
This includes `Interval`s, for example.
* The `AssessmentInterval` module is moved out of `Cohort` module
to the root of the package.
* Adds `Arbitrary` instances for `Population` and `Subject`

## 0.22.7

* Moves app building modules into `hasklepias-appBuilder` and out of `hasklepias-core`.
* Adds ability to output from a cohort application to `stdout`, a file, or a S3 location. Note that this ability is not yet in place for a prefilter application.

## 0.22.6

* Moves application IO related functions and types from `Hasklepias.Misc` to `Hasklepias.AppUtilities`.
* Creates the `Hasklepias.MakeFilterApp` module, which exposes the `makeFilterApp` function, which like `makeCohortApp` returns an application that can be executed in a `main` function. The `makeFilterApp` takes two arguments: 1) a string for the name of the application (e.g. the project ID) and 2) a predicate function of type `Event a -> Bool`. The application takes event data formatted as [`ndjson`](http://ndjson.org/) (i.e. one event per line). The application returns the event data filtered to all those subjects who have at least one event satisfying the given predicate. Each subject's data must be grouped in contiguous chunks of lines; otherwise, the application may not behave as expected and will not warn or raise an error. Lines that fail to parse as an `Event` do not satisfy the predicate, but are not dropped from the output. In other words, all of a subject's data is returned in the same order as the input, provided that at least one line successfully parses into an `Event` and satisfies the predicate.

## 0.22.5

* Adds `asum` to reexports module.
* Reexports a few lens-related functions.

## 0.22.4

* Updates `interval-algebra` dependency to 1.1.0.

## 0.22.3

* Adds the `baselineFinishedBy` method to the `Baseline` class. This function creates a `BaselineInterval` of the given duration that is `FinishedBy` the `Index` interval. In other words, the baseline _includes_ the index. Adds the `makeBaselineFinishedByndex` function to create an `AssessmentmentInterval` constructed from `baselineFinishedBy`.
* Adds `makeIndexSet` function, which maps a list of indices to an `IndexSet`. In the case the list is empty, the result is `MkIndexSet Nothing`.

## 0.22.2

* Adds `Index i a` as an argument to the `runCriteria` function of a `CohortSpec`, so that `Criteria` can depend on an index -- an obvious need in retrospect.

## 0.22.1

* Adds functionality to the application resulting from `makeCohortApp` to read data from `stdin`, a local file, or an S3 location.

## 0.22.0

* Changes the type for `CohortSpec` to the type below, which makes the dependency of a cohort on one or more indices explicit. An `IndexSet` is `Maybe (Set Interval i a)`, where `Nothing` indicates that a subject does not have any indices. In the case that there are one or more indices, each index corresponds to exactly one observational unit (`ObsUnit`). After `runIndices` is evaluated, the `runCriteria` function is run on each observational unit. Then for each `ObsUnit` with a `CohortStatus` of `Included`, `runFeatures` is evaluated to create the final output type `d0`.

```haskell
data CohortSpec d1 d0 i a = MkCohortSpec
  { runIndices  :: d1 -> IndexSet i a
  , runCriteria :: d1 -> Criteria
  , runFeatures :: Index i a -> d1 -> d0
  }
```

* Adds the `ObsID` type as the id type for `ObsUnit`. This is the pair `(SubjectID, Natural)`, where the `SubjectID` is the subject from which the `ObsUnit` originated and the `Natural` is the index of the index. For example, the first observational unit for subject "a" would have the id of `MkObsID ("a", 1)`; the second `MkObsID ("a", 2)`; and so on.
* Moves attrition related types out of `Cohort.Core` into `Cohort.Attrition`.
* Adds `SubjectHasNoIndex` as a variant to `CohortStatus` to represent cases where a subject does not have any indices.
* Adds the `CohortApp` type, which `makeCohortApp` now returns. In this way, the `CohortApp` can tested using a golden test. The application can be run to `IO ()` using the `runApp` function.  

## 0.21.0

* Updates `interval-algebra` dependency to `1.0.0`.
* Reorganizes project into multiple sub-projects. E.g., `core`, `cohort-collector`, `edm`, and `stype` are now each a separate Haskell package their own `.cabal` file.
* Adds the `DuplicateRecordFields` language extension to the `Cohort.Output` module, so that field names in both row-wise and column-wise output formats are shared. Namely, `rowAttributes` --> `attributes` and `colAttributes` --> `attributes` and `colData` --> `cohortData` and `rowData` --> `cohortData`.

## 0.20.0

* Adds the `Hasklepias.CohortCollection` module which is exposed as the `collector` application. This application can be used to combine cohorts that were derived from different input data (e.g. different partitions of data). However, cohorts must be derived from the same specification using the same shape (e.g. `rowWise` or `colWise`). The application can be installed from the asclepias repository using `cabal install`. It is also available to download from `download.novisci.com/hasklepias/collector-0.20.0-linux.tar.gz` (currently only available for linux (not sure of architecture)). The following gives an example of using the application on local files:

```sh
$ cat collector-test/tests/manifestrw.txt 
testrw1.json
testrw2.json
testrw3.json
$ collector -f collector-test/tests/manifestrw.txt -d collector-test/tests
{"example":[{"totalProcessed":7,"attritionInfo":[{"attritionCount":2,"attritionLevel":{"contents":[1,"dummy"],"tag":"ExcludedBy"}},{"attritionCount":5,"attritionLevel":{"tag":"Included"}}]},{"contents":{"rowAttributes":[{"name":"myVar1","attrs":{"getPurpose":{"getTags":[],"getRole":["Outcome"]},"getDerivation":"","getLongLabel":"another label","getShortLabel":"somelabel"},"type":"Count"},{"name":"myVar2","attrs":{"getPurpose":{"getTags":[],"getRole":[]},"getDerivation":"","getLongLabel":"","getShortLabel":""},"type":"Bool"}],"rowData":[["a",[5,true]],["b",[5,true]],["c",[10,false]],["d",[99,true]],["f",[86,true]]]},"tag":"RW"}]}
```

## 0.19.0

* Overhauls the way that cohorts are written to JSON, mostly in the `Cohort.Output` module. The important bit is that intermediate types were added that can hold both row-wise and column-wise cohort data as list of `Value`s (`Data.Aeson` internal representation of JSON). These intermediate types were made `Semigroup` instances, which means that cohorts can be *combined*. Note that you should only combine cohorts (and set of cohorts) evaluated from the same set of cohort specifications. Since all types of the the underlying data are masked by `Value`, you technically can combine any values of these intermediate types, but the results would be generally be nonsensical. Use this feature of combining cohorts to (e.g.) collect the same cohort data processed on different partitions of population data. While users should not need to worry about these details, here is the semigroup behavior for two cohorts (i.e. `cohort1 <> cohort2`):
  * In both row-wise and column-wise formats, attrition information is added as one would expect.
  * In both row-wise and column-wise formats, feature attributes are kept from the first cohort (again, you should only combine two compatible cohorts).
  * In row-wise format, row data is stacked.
  * In column-wise format, data is stacked by column.
  * If you try to combine a row-wise cohort with a column-wise cohort, the cohort in the second position is dropped altogether.
* Adds a `CohortSet` type (which is technically not a `Set` but a `Map`) as a container for `Cohort`s, in this way each cohort can be named. Correspondingly, adds a `CohortSetSpec` type which is a `Map` of `CohortSpec`s.
* Adds the constructor function `makeCohortSpecs` for easily making a `CohortSetSpec` from a list.
* Adds the `evalCohortSet` function which evaluates a `CohortSetSpec` from `Population` into a `CohortSet`.
* Moves the `F` type synonym for `Feature` to the `Feature` module.
* Removes any the usage of `Data.List.NonEmpty.fromList` in `Cohort.Output` which was throwing an error when the input data is empty.
* Adds the empty file `exampleData/emptyData.jsonl` for testing the app on empty data.

## 0.18.2

* Mostly internal work to clean up `AttributionInfo` values. Now counts for all the criteria are included in the information, including 0 counts. Also added a total processed field. Adds a `Semigroup` instance for `AttributionInfo` so that attrition information can be later combined -- this will be useful, for example, when combining the same cohort across partitions.
* Adds `FromJSON` instance for `AttributionInfo` (and its components), so that attrition info can be read back into a Haskell program.

## 0.18.1

* Fixes incorrect type on `buildNofConceptsBinaryConcurBaseline` which returned `Bool` instead of `Binary`.

## 0.18.0

* Refactors the `Features.Compose` module a bit. Adds constructors for composing `Definition`. For example, `D1C :: (a2 -> a1 -> a)  -> Definition (F n1 b -> F n02 a2) -> Definition (F n1 b -> F n01 a1) -> Definition (F n1 b -> F n0 a )`. Such constructors allow one to build definitions from other definitions, provided the input types are the same. Note: there is *not* a single interface to these constructors like is provided by the `define` and `defineA` functions. The poorly designed `Eval` typeclass is now replaced with a single `eval` function which simply pattern matches on the various shapes of `Definition`s. I fully expect this module to get another refactor in the future, but it is shaping up OK.
* Examples are updated according to the changes `define`/`eval`.
* Adds the `buildNofXOrNofYWithGapBool` template, which can be used for the common feature or 2-outpatient events with at least some gap between them or 1 inpatient event.

## 0.17.1

* Fixes silly mistake with git.

## 0.17.0

* Adds the `Cohort.AssessmentIntervals` modules, which provides types and safe constructors for intervals during which features can be evaluated. The module currently provides the `BaselineInterval` type, with constructors `baseline` and `baselineBefore`. These two constructors guarantee that the resulting `BaselineInterval` will `meet` or `precede` (respectively) the provided `Index`. Use `baseline` if you want a `BaselineInterval` that ends at the beginning of the `Index`; use `baselineBefore` if you need space between the end of the baseline interval and `Index`. Similarly, there is a `FollowupInterval` type, with constructors `followup`, `followupMetBy`, and `followupAfter`. Note that the `followup` function always returns a `FollowupInterval` such that `end index < end (followup duration index)` for any provided `duration`. The `baseline` and `followup` functions were not named with their associated relation to `Index` (meets and startedBy, resp.), since they are most likely the most common use case. The `AssessmentInterval` type is a sum type with (currently) two variants: one containing a `BaselineInterval` and the other a `FollowupInterval`. The following functions create `AssesmentmentIntervals` using the corresponding function:
  * `makeBaselineFromIndex`: `baseline`
  * `makeBaselineBeforeIndex`: `baselineBefore`
  * `makeFollowupFromIndex`: `followup`
  * `makeFollowupMetByIndex`: `followupMetBy`
  * `makeFollowupAfterIndex`: `followupAfter`
* Modifies the continuous enrollment template to take a function `Index i a -> AssessmentInterval a` as an argument to enforce that functions that create valid assessment interval are used. Updates `ExampleCohort1` accordingly.
* Updates `interval-algebra` dependency to 0.10.2, and updates functions as needed.
* Adds the `EventData.Predicate` module which exposes `Predicate`s on `Event`s. For example, `isEnrollmentEvent` has the type `Predicate (Event a)` (so `getPredicate isEnrollmentEvent :: (Event a -> Bool)`). This module also includes two utilities for composing `Predicate`s: `(&&&)` and `(|||)` for conjunction and disjunction respectively. For example, running `isEnrollmentEvent ||| isBirthYearEvent` would return `True` if an event either has the Enrollment domain or has the Demographic domain with BirthYear as its field. You can also form Predicates on the interval part of an Event. Something like `Predicate (\x -> before index x) &&& isBirthYearEvent` is a predicate that returns `True` when an event is before `index` and the event contains a `BirthYear` fact
* Adds the `EventData.Accessors` module and moves functions such as `viewBirthYears` from `Hasklepias.FeatureEvents` to this new module.
* Adds an initial framework for feature definition builders (i.e templates for functions that define features). These are found in the `Hasklepias.Templates.Features` module. Adds a basic set of builders including:
  * `buildIsEnrolled`: identifies whether there is an event concurring with index
  * `buildContinuousEnrollment`: identifies whether a set of events meets continuous enrollment criteria given an allowable gap between enrollment intervals
  * `buildNofX`: a template that finds whether there are N events of some predicate X
  * `buildNofXBool`: `buildNofX` specialized to return `Bool`
  * `buildNofXBinary`: `buildNofX` specialized to return `Binary`
  * and several more

## 0.16.2

* Reexports `ToJSON` typeclass so users can export data as needed.

## 0.16.1

* Updates `FromJSON` instance for `Domain`, so that a JSON event with `"domain" = "Enrollment"` is deserialized into `Event` whose `Domain` is `Enrollment`.

## 0.16.0

* Adds a basic framework for `Feature` definition templates. Initially, this includes two templates for enrollment related features:
  * `defIsEnrolled` is a definition that maps an `Index` and a container of `Event`s to a `Status` (i.e. `Include` or `Exclude`). This template takes no arguments.
  * `defContinuousEnrollment` is a definition that maps an `Index`, a container of `Event`s, and a `Status` to a `Status`. This template takes two arguments: a function that creates the interval from the index during which enrollment is assessed and an allowable gap between any enrollment intervals. The input `Status` is used so that continuous enrollment may depend on other statuses. For example, you may want to have continuous enrollment depend on being enrolled.
* Adds a framework for testing the templates. E.g. templates can be tested using `cabal test templates`.
* Updates the `ExampleCohort1` to use the `defIsEnrolled` and `defContinuousEnrollment` templates.
* Makes the `Index i a` type an instance of `Intervallic`, so you can use methods like `begin`, `end`, and interval algebra functions directly on an `Index` without having to unpack the interval first.
* Adds stripped down `Enrollment` Domain. This does type is not faithful to the EDM, as it does not include the event data model's plan fact.
* Adds the `isEnrollment` predicate function for identifying `Domain`s that are Enrollment. This can be used with `filterByDomain` to filter a container of `Event`s to those that are enrollment events.

## 0.15.2

* Updates `viewBirthYears` utility to filter a list of events to those with `BirthYear` demographic facts. In this way, one doesn't need to prefilter the input list by, e.g., a concept.
* Adds `viewStates` and `viewGenders` utilities for extracting a list of Demographic `State`s and `Gender` (resp.) values from a collection of events. Note that these functions (like `viewBirthYears`) return a *List*, as the source data may contain be 0 or more values for a given subject. You probably only want one value for a given demographic, so you may need a function like `headMay` if you want the first element of the list (if it exists). Note too that the API of these accessor functions for `facts` in a `Context` need a careful design review and may be changed in the future.
* Adds `yearFromDay`, `monthFromDay`, and `dayOfMonthFromDay` utilities to get the year, month, and day of month, respectively from a `Day`.

## 0.15.1

* Tinkers with package version dependencies in `.cabal` file.

## 0.15.0

* Adds `Occurrence` type which is a simply a pair a reason and an `EventTime`. That is, an `Occurrence` captures what and when something occurred. Adds the `CensoredOccurrence` type which is similar to an `Occurrence`, except that the reason is of type `CensoringReason cr or`, where `data CensoringReason cr or = AdminCensor | C cr | O or`. The time of a `CensoredOccurrence` is a `MaybeCensored (EventTime a)` (not simply can `EventTime`). See the examples in `examples/ExampleFeatures4` for usage.
* A number of the utility functions in the `Hasklepias.FeatureEvents` module are generalized to operate on data structures other than lists of `Event`s.
* Exports type synonyms `F n a` and `Def d` for `Feature n a` and `Definiton d` to save a bit of typing.
* Adds `Hasklepias.Misc` module as a location to collect miscellaneous types and functions for the time-being, until better locations are found or created.
* Adds `examples/ExampleFeatures4.hs` which is an extensive example of assessing exposure protocols and censored outcomes.
* Moves the `Cohort` module to the top-level. Moves the `FeatureEvents` module within the `Hasklepias` module.

## 0.14.0

* Adds the ability to modify the output shape of cohorts. The `makeCohortApp` now takes a "`shape`" function as an argument of type `Cohort d -> CohortShape shape`. Currently, two functions of this type are provided: `rowWise` and `colWise`. The `rowWise` functions presents the output feature data in a row-wise format where each subject's data is its own array; whereas `colWise` presents the feature data where all the data of a given feature are in a single array.
* Updates `interval-algebra` dependency to 0.9.0. Updates code accordingly.
* Reorganizes modules to simplify imports.

## 0.13.2

* Adds `Binary` Stype data type.
* Adds `ToJSON` instance for most Stype types.
* Adds `Featureset` module and type, which is simply a list of `Featureable`s. This is just a step towards being able to define the shape of JSON output.

## 0.13.1

* Adds the `Featureable` type, which allows users to put features into a heterogenous list. A drawback of `Featureable` is that two `Featurable` values cannot be tested for equality, so testing will need to occur before a `Feature n d` is packed into a `Featureable` (by the `packFeature` function) or after the `Featureable` is encoded to JSON. See `examples/ExampleCohort1.hs` for example usage.
* Adds a `"type"` field to JSON output `Feature`s, which is a string representing the type `d` in a `Feature n d`. E.g., for a `Feature "x" Bool`, the result would be : `{..., "type": "Bool", ...}`.
* Adds the `Role` and `Purpose` types, which now are included as part of the `Attributes` type. The `Role`s mostly align with `stype`'s `valid_roles`, with the exception that `"identifier"`, `"index"` and `"censoring"` are not included and `"other"` corresponds to `Unspecified`.

## 0.13.0

* Adds a rudimentary `Attributes` type and `HasAttributes` typeclass for adding attributes to `Feature`s. This interfaces will likely change in the future, but for now users have the ability to add information like labels to a `Feature`. In fact, `Feature`s which are encoded to JSON are *required* to have attributes.
* Adds rudimentary `Stype` module in order to (ultimately) interface with the [R `stype` package](https://docs.novisci.com/stype/). For now, this module simply creates a few of the types, some of which are *not* an appropriate implementation. For example, the `Nominal` type is simply `newtype Nominal a = Nominal a`. Essentially, it's just a way to label something as nominal at this point.

## 0.12.0

* Converts `AttritionStatus` from a List to a NonEmpty container.
* Adds the `MakeApp` module with a single function `makeCohortApp` which takes a list of cohort specifications and returns an application (an `IO ()` function). Currently, the application is bare bones, printing the resulting cohorts (one per specification) to `stdout` and any parsing errors to `stderr`. For example usage, see the code in `exampleApp`.

## 0.11.1

* Modifies a `Context` so that its `_facts` are no longer `Maybe Domain` and now just `Domain`.

## 0.11.0

* Refactors the `FeatureCompose` module. `FeatureSpec` and `FeatureDefinition` types are dropped, and now there is a single `Definition` type with two related typeclasses: `Define` (with function `define`) and `DefineA` (with function `defineA`). Both of these typeclasses can lift functions to functions of either Features or FeatureData. For example `define` can take a function `c -> b -> a`, and, depending on the type annotation gives back a definition `Definition (FeatureData c -> FeatureData b -> FeatureData a)` or `Definition (Feature name2 c -> Feature name1 b -> Feature name0 a)`. The `defineA` works similarly for a function of type `c -> b -> f a`, where `f` is either `FeatureData` or `Feature`. The `eval` function takes any `Definition` and an appropriate argument to give back the desired return type. For example, to evaluate `def` of type `Definition (FeatureData c -> FeatureData b -> FeatureData a)`, you would call `eval def (x, y)`, where `(x, y) :: FeatureData c, FeatureData b)`.  At this time, one can define `Definition`s with up to 3 inputs.
* For now, the `Attributes` component of `Feature`s is dropped. This will be rethought and added back in at later time.

## 0.10.0

* Refactors `FeatureSpec`s and `Feature`s to have a `Symbol` as its name, rather than `Text`.
* Updates `parseEventLines` and related functions to keep parse errors. Similarly for `parseSubjectLines` which keeps the error message as well as the line number.

## 0.9.0

* Refactors cohort building types and functions. Still aways to go, but the basic ideas are there now.
* Adds the `ExampleCohort1` module to demonstrate the updates on calendar based cohorts.
* Adds several new functions and modules to `Reexports` including the `Test.Tasty` and `Test.Tasty.HUnit` testing modules for testing cohort building.

## 0.8.3

* Modifies `FromJSON` instance for events to use `parseEvent` as well as create a moment from the provided `begin` in the case that `end` is missing.
* Updates `FromJSON` instance for events to parse the facts object. Currently the only implemented domain is demographics.
* Adds `FromJSON` instances for `Population`, so now data from multiple subjects can be marshaled into Hasklepias programs.

## 0.8.2

* Adds preliminary `Subject`, `Population`, `ObsUnit`, and `Cohort` types to the `Hasklepias` module, along with the `makeObsUnitFeatures` which takes a function that maps a `Subject` into a `ObsUnit`. Currently, this function only supports 1-1 mapping between subjects and observational units. There is also the `makeCohort` function which maps a `Population` to a `Cohort`. The types get preliminary `FromJSON` (for `Subject` and `Population`) and `ToJSON` (for `ObsUnit` and `Cohort`) instances as well. Example 1 demonstrates use of these two functions.

## 0.8.1

* Adds an initial pass at the `Domain` type; simply including the `Demographics` type for now.

## 0.8.0

* Reorganizes modules into a more "vertical" structure that reflects the decoupling of the various components that make up hasklepias:
  * `EventData`: types and functions related to the event data model
  * `FeatureCompose`: types and functions for composing new features
  * `FeatureEvents`: various utilities for composing features from events specifically
  * `Hasklepias`: at this point, just reexporting the above modules and other Haskell functions

## 0.7.3

* Refactors the `eval*` function for Features so that there is a single `eval` not two.

## 0.7.2

* Adds `Criteria` module which provides specialized functions and types for working with boolean `Feature`s which are meant to be used to determine whether a subject is included or excluded from cohorts.

## 0.7.1

* Fixes bug in `FeatureData` `Monad` instance, so you don't get infinite recursion.

## 0.7.0

* Modifies the way that `Feature`s are defined and evaluated. For one, the dependency between `Events` and `Feature`s is eliminated, thus decoupling the defining of Features from the input data type. There are now 4 functions for defining features:
`define0`, `define1`, `define2`, and `define2d`, and 3 functions for evaluating `eval0`, `eval1`, and `eval2` (and corresponding `evalSpec*`). These functions will hopefully be combined into a single interface at later time. See the function signatures for the types of functions passed to a `define*` function. Examples will be forthcoming as the API stabilizes.

## 0.6.1

* Adds `derving Eq` to `Feature` type.

## 0.6.0

* Adds `PolyKinds` extension to `Feature` module to enable poly-kind inputs to `FeatureDefinition`s. Adds a related `Defineable` typeclass with `define` and `eval` functions as a common interface for defining new definitions and evaluating them.
* Removes `defineEF` and `applyEF` function (and other similar functions). The functionality is now handled by the `Defineable` class.

## 0.5.1

* Adds `Show`, `Functor`, and `Generic` to Reexports.
* Updates `interval-algebra` to `0.8.2`.

## 0.5.0

* Changes what was the `Feature` type into `FeatureData`. The `Feature` type becomes a container for `FeatureData` with a name and attributes.
* Adds the `FeatureSpec` type which contains `FeatureDefinition`s plus a name and attributes. The name and attributes are mapped directly into the resulting `Feature` when a `FeatureSpec` is evaluated, while the `FeatureDefinition` is evaluated into `FeatureData`. The `evalEFFeature`, `evalFEFFeature`, and `evalFFFFeature` are provided for evaluating a `FeatureSpec` according the corresponding `FeatureDefinition`.
* Adds additional functions to reexports.
* Adds `witherable` dependency to use a more general `filter` function.

## 0.4.4

* Adds the `FFF` option to `FeatureDefinition` to define `(Feature f -> Feature e -> Feature d)` along with corresponding `defineFFF` and `applyFFF`.
* Adds `zipWith`, `id`, and `Integer` to re-exports.

## 0.4.3

* Exports `Feature` constructor.
* Adds `defineFEF2` function for creating a feature definition where the provided function returns a `Feature d` rather than just a `d`.
* Generalizes `allPairs` from type `[a] -> [a] -> [(a, a)]` to `[a] -> [b] -> [(a, b)]`.
* Reexports a few functions and types from `Data.Time.Calendar`. Also reexports `const` from `Data.Function`.

## 0.4.2

* Updates `interval-algebra` to 0.8.0.

## 0.4.1

* Modifies the example in `example/ExampleFeatures3` to use the pipe `|>` operator.
* Adds the `hasAllConcepts` function to the `HasConcepts` class.
* Adds a `Reexports` module with the goal to re-export everything one might need from other Haskell libraries to build a cohort.
* Removes a number of unneeded/unused functions from the `Functions` module.
* Adds the `Safe` language extension to modules where possible.

## 0.4.0

* Adds the `FeatureDefinition` to represent common patterns for building `Feature`s:

```haskell
data FeatureDefinition e a d =
    EF  (Events a -> Feature d)
  | FEF (Feature e -> Events a -> Feature d)
```

* Provides an initial set of functions designed to make defining `Feature`s easier, namely `defineEF` and `defineFEF`. These functions construct `FeatureDefinition`s of using `EF` and `FEF` constructors, respectively. The example features in `examples/ExampleFeatures1` demonstrate their use.
* Adds the `allPairs` function to form all pairs of elements of two lists.
* Adds the `splitByConcepts` to split a container of events into a pair such that first element contains
events have any of the first argument's concepts, and similarly for the second element.
* Demonstrates how `allPairs` and `splitByConcepts` might be used in the `examples/ExampleFeatures3` module.
* Adds a rudimentary `ToJSON` instance for `Feature`s so that data can be encoded and output from the software. This is pretty rough; e.g. encoding an `Interval Int` feature produces: `"{\"end\":10,\"begin\":0}"`.
* Removes the `Transformations` module and `transformToMeetingSequence` function. The same functionality is available by using the `formMeetingSequence` function from `interval-algebra`. See `examples/ExampleFeatures2` for the updated example.
* Adds the `toConceptEventOf` function which creates a `ConceptEvent` but takes the `intersection` of `Concepts` in the first argument and concepts in the context of the `Event` in the second argument to form the new `ConceptEvent`. This is a way to keep only those concepts you need in the event.

## 0.3.0

* Updates code as needed to work with interval-algebra v0.6.2. In particular, the `Event a` is now a synonym for `PairedInterval Context a`, hence any methods that work on the `PairedInterval` also work for the `Event` type.
* Adds the `ConceptEvent a` type which is a synonym for `PairedInterval Concept a`; i.e, this is an event without facts or a source.
* Adds the `toConceptEvent` function for dropping from an `Event a` to a `ConceptEvent a`, and `mkConceptEvent` function for directly making a `ConceptEvent` from concepts and an interval.
* Adds generators for lists of arbitrary events. The generator for `Concepts` is limited at this point; it simply takes a subsample of the first 10 letters of the alphabet. Currently, only generators for `Event Int` are provided by the `generateEventsInt`. For example, in the `repl` `generateEventsInt 2` produces two randomly generated events:

```haskell
*Hasklepias> generateEventsInt 2
[{(-33, -16), Context {getConcepts = fromList ["G","I"], getFacts = Nothing, getSource = Nothing}},{(12, 13), Context {getConcepts = fromList ["A","C","D","E","G","I"], getFacts = Nothing, getSource = Nothing}}]
```

* Adds the `transformToMeetingSequence` function which takes a set of concepts and a list of possibly non-disjoint `ConceptEvents`s and returns a list of `ConceptEvents`, where each consecutive event meets the next. Moreover, only those concepts selected (in the first argument) are kept in the output list of events. In the case that none of the events have the chosen concepts during an interval, an `ConceptEvent` with an empty set of concept is returned. A few examples might make this more clear.

```haskell
*Hasklepias> :set -XOverloadedStrings
*Hasklepias> x <- fmap (map toConceptEvent) (generateEventsInt 1)
*Hasklepias> x
[{(3, 4), fromList ["B","C"]}]
*Hasklepias> transformToMeetingSequence (map packConcept ["A"]) x
[{(3, 4), fromList []}]
*Hasklepias> transformToMeetingSequence (map packConcept ["B"]) x
[{(3, 4), fromList ["B"]}]
*Hasklepias> x <- fmap (map toConceptEvent) (generateEventsInt 10)
*Hasklepias> x
[{(-44, 7), fromList ["C","D","E","F","H","J"]},{(-30, -29), fromList ["A","B","F","G","H","I","J"]},{(-25, 5), fromList ["C","D","E","I"]},{(-20, -19), fromList ["A","C","E","G","I","J"]},{(-17, -16), fromList ["B","D","F","J"]},{(-6, -5), fromList ["E","F","H","J"]},{(2, 21), fromList ["A","F","J"]},{(18, 19), fromList ["D","F","G","H","I"]},{(19, 20), fromList ["B","C","D","E","F","H"]},{(30, 31), fromList ["B","C","D","H","J"]}]
*Hasklepias> transformToMeetingSequence (map packConcept ["B", "I"]) x
[{(-44, -30), fromList []},{(-30, -29), fromList ["B","I"]},{(-29, -25), fromList []},{(-25, -17), fromList ["I"]},{(-17, -16), fromList ["B","I"]},{(-16, 5), fromList ["I"]},{(5, 18), fromList []},{(18, 19), fromList ["I"]},{(19, 20), fromList ["B"]},{(20, 30), fromList []},{(30, 31), fromList ["B"]}]
```

* Adds an example of `transformToMeetingSequence` could be used to derive a feature that is the list of durations that a subject was both hospitalized and on antibiotics at the same time in the `examples/ExampleFeatures2` module.
