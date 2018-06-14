{ pkgs ? import <nixos> {}
, compiler ? "ghc802"
, stack ? pkgs.stack
, zlib ? pkgs.zlib
}:
let
  package = import ./default.nix { inherit pkgs compiler; };
in pkgs.stdenv.mkDerivation rec {
  name = "health-env";

  buildInputs = [ stack
                  pkgs.haskell.compiler.${compiler}
                  pkgs.haskellPackages.hasktags
                  pkgs.haskellPackages.hlint
                  zlib
                ];

  LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
}

