# Changelog for hasklepias-appBuilder

## 0.2.0

* Exposes CLI options for `EvaluateFeatures` and `SubjectSample`
to control cohort evaluation from a cohort app.
* Adds help text to the cohort app,
including information on the git commit from which the application was built.
* Fixes a bug where options were overwritten in `runApp`.

## 0.1.0

* Moves app building modules into `hasklepias-appBuilder` and out of `hasklepias-core`.
* Changes `co-log` dependency to `co-log-core`.
* Adds shared testing routines
