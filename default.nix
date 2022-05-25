let
  # Read in the Niv sources
  sources = import ./nix/sources.nix { };
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.cabalProject {
  name = "asclepias";
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "asclepias";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107";

  # should also be set in cabal.project to matching index?
  index-state = "2022-05-21T09:30:28Z";

  # easiest way to configure is with module
  modules = [
    {
      # https://github.com/input-output-hk/cardano-node/blob/0f6a20d15b69f599eafc3d46535474ba67c52693/nix/haskell.nix#L165
      # Do not ignore version bounds
      packages.hasklepias-main.doExactConfig = false;
    }
    {
      packages.hasklepias-main.components.foreignlibs.time.doExactConfig = true;
      packages.hasklepias-core.components.library.doCoverage = false;
      packages.hasklepias-core.components.library.doCheck = false;

      #packages.monarch.components.library.doCoverage = true;
      #packages.monarch.components.library.doCheck = true;

      packages.stype.components.library.doCoverage = true;
      packages.stype.components.library.doCheck = true;

      # trying to avoid --dependency=time=time-1.9.3

    }
  ];

}
