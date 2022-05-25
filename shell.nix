(import ./default.nix).shellFor {
  tools = {
    cabal = "3.6.2.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
