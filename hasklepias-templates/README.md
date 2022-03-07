# Hasklepias Templates

TODO: link to docs


### Add the new module to `other-modules`

In `hasklepias-templates.cabal`, add the new module to the `other-modules section, as in:

```cabal
  other-modules:
      ...
      Templates.Features.BuildMyNewFeature
      ...
```

### Add the builder's tests to the `Tests` module

Import the tests of your new feature builder in the `Templates.Tests` module:

```haskell
import Templates.Features.BuildMyNewFeature  ( buildMyNewFeatureTests )
```

and add the tests to the test group:

```haskell
templateTests :: TestTree
templateTests = 
   testGroup 
      "Tests of feature building templates" 
      [ ...
      , buildMyNewFeatureTests ]
```

### Run tests

Check the package builds and all the tests pass using:

```sh
cabal test hasklepias-templates
```
