with import <nixpkgs> {};
pkgs.haskell.packages.ghc883.callCabal2nix "monad-control-identity" ./. {}
