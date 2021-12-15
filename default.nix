# default.nix
# See: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#scaffolding

let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in 
# { withCoverage ? false }:
pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };

  # see: https://github.com/input-output-hk/haskell.nix/issues/1314
  modules = [{
    # reinstallableLibGhc = true;
    options.nonReinstallablePkgs =
      pkgs.lib.mkOption { apply = pkgs.lib.remove "time"; };
  }];
  # modules = pkgs.lib.optional withCoverage [{
  #   packages..pkg.components.library.doCoverage = true;
  # }];

  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107"; # Not required for `stack.yaml` based projects.
}