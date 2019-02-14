{ compiler ? "ghc844" }:

let
  pkgs = import <nixpkgs> { };

  haskellPackages = pkgs.haskell.packages.${compiler};

  freer-simple = haskellPackages.callPackage ./deps/freer-simple.nix { };

  drv = haskellPackages.callPackage ./packages.nix {
    inherit freer-simple;
  };
in
  drv
