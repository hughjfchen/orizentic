{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, rustPlatform ? pkgs.rustPlatform
, licenses ? pkgs.lib.licenses
, maintainers ? pkgs.stdenv.maintainers }:
let
    cratesIO = import ./crates-io.nix {
        lib = stdenv.lib;
        buildRustCrate = pkgs.buildRustCrate;
        buildRustCrateHelpers = pkgs.buildRustCrateHelpers;
    };
    cargo = import ./Cargo.nix {
        lib = stdenv.lib;
        buildPlatform = stdenv.buildPlatform;
        buildRustCrate = pkgs.buildRustCrate;
        buildRustCrateHelpers = pkgs.buildRustCrateHelpers;
        cratesIO = cratesIO;
        fetchgit = pkgs.fetchgit;
    };
    frameworks = pkgs.darwin.apple_sdk.frameworks;
    security = if pkgs.stdenv.buildPlatform.system == "x86_64-darwin"
        then [ frameworks.Security ]
        else [];

    orizentic = (cargo.orizentic {}).override {
        crateOverrides = pkgs.defaultCrateOverrides // {
            orizentic = attrs: { buildInputs = [ pkgs.openssl ] ++ security; };
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
