{ pkgs ? import <nixpkgs-18.09> {}
, unstable ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, licenses ? pkgs.lib.licenses
, maintainers ? pkgs.stdenv.maintainers }:
let
    rust = import ./nixpkgs/rust-1.33.nix {
      mkDerivation = pkgs.stdenv.mkDerivation;
      fetchurl = pkgs.fetchurl;
      stdenv = pkgs.stdenv;
    };

    buildRustCrate = unstable.buildRustCrate.override {
        crateOverrides = pkgs.defaultCrateOverrides // {
            buildRustCrate = attrs: { rustc = rust.rustc; };
        };
    };

    cratesIO = import ./crates-io.nix {
        lib = stdenv.lib;
        buildRustCrate = buildRustCrate;
        buildRustCrateHelpers = unstable.buildRustCrateHelpers;
    };
    cargo = import ./Cargo.nix {
        lib = stdenv.lib;
        buildPlatform = stdenv.buildPlatform;
        buildRustCrate = buildRustCrate;
        buildRustCrateHelpers = unstable.buildRustCrateHelpers;
        cratesIO = cratesIO;
        fetchgit = pkgs.fetchgit;
    };
    frameworks = pkgs.darwin.apple_sdk.frameworks;
    security = if pkgs.stdenv.buildPlatform.system == "x86_64-darwin"
        then [ frameworks.Security ]
        else [];

    orizentic = (cargo.orizentic {}).override {
        crateOverrides = pkgs.defaultCrateOverrides // {
            orizentic = attrs: { buildInputs = [ pkgs.openssl ] ++ security;
                               };
        };
    };

in pkgs.symlinkJoin rec {
    name = "orizentic-${version}";
    version = "2.0.1";
    paths = [ orizentic ];

    meta = with stdenv.lib; {
        description = "A library for inerfacing with a JWT auth token database and a command line tool for managing it";
        homepage = "https://github.com/luminescent-dreams/orizentic";
        license = licenses.bsd3;
        maintainers = [ "savanni@luminescent-dreams.com" ];
    };
}
