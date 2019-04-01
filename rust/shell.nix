let
    pkgs = import <nixpkgs-18.09> {};
    unstable = import <nixpkgs> {};
    frameworks = pkgs.darwin.apple_sdk.frameworks;
    rust = import ./nixpkgs/rust-1.33.nix {
      mkDerivation = pkgs.stdenv.mkDerivation;
      fetchurl = pkgs.fetchurl;
      stdenv = pkgs.stdenv;
    };

in pkgs.stdenv.mkDerivation {
    name = "orizentic";

    buildInputs = [ rust
                    unstable.carnix
                    frameworks.Security
                    frameworks.CoreFoundation
                    frameworks.CoreServices
                  ];
}
