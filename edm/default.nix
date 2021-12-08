{ compiler ? "8107"  }:
let 

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          interval-algebra = 
            pkgs.haskell.lib.dontCheck
             ( haskellPackagesNew.callPackage ../nix/interval-algebra.nix { });

          aeson = 
            pkgs.haskell.lib.dontCheck
             ( haskellPackagesNew.callPackage ../nix/aeson.nix { });
          
          OneTuple = 
            pkgs.haskell.lib.dontCheck
             ( haskellPackagesNew.callPackage ../nix/OneTuple.nix { });

          hashable = 
            pkgs.haskell.lib.dontCheck
             ( haskellPackagesNew.callPackage ../nix/hashable.nix { });

          integer-simple = 
            pkgs.haskell.lib.dontCheck
             ( haskellPackagesNew.callPackage ../nix/integer-simple.nix { });
        };
      };
    };
  };
  
  pkgs = import <nixpkgs> { inherit config; };

in
  { edm = pkgs.haskellPackages.callPackage ./edm.nix { };
  }
