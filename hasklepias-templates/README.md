# Hasklepias Templates

TODO: link to docs

## Creating a new builder

### Create a symbolic link for `.md`

The `./scripts/create-template-link.sh` provides a utility for creating the appropriate link. For example, the following creates `BuildNofX.lhs` as a symbolic link `BuildNofX.md`.

```sh
./scripts/create-template-link.sh BuildNofX.md
```

### Add the new module to `other-modules`

In `hasklepias-templates.cabal`, add the new module to the `other-modules section, as in:

```
  other-modules:
      ...
      Templates.Features.BuildMyNewFeature
      ...

```

### Reexport the new module in the `Features` module

In the `Templates.Features` module, import your new module:

```haskell
import    Templates.Features.BuildMyNewFeature
```

as well as re-export your new module at the top of the `Features` module:

```haskell
module Templates.Features (
     ...
   , module Templates.Features.BuildMyNewFeature
   , ...
) where
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

## Technical Notes

The `hasklepias-templates` package relies on the [markdown-unlit](https://github.com/sol/markdown-unlit) preprocessor in order to write the templates as markdown (`.md`) files while still having the `cabal` tools just work. In particular, `markdown-unlit` extracts code blocks marked with `haskell`.