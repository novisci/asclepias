{ compiler ? "8107" }:
let 
  pkgs = import <nixpkgs> { };
in
  { project1 = pkgs.haskellPackages.callPackage ./cohort-collector.nix { };
  }
