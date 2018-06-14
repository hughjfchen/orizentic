{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./capabilities.nix { }
