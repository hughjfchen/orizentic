let
    pkgs = import <nixpkgs-18.09> {};
    unstable = import <nixpkgs> {};
    frameworks = pkgs.darwin.apple_sdk.frameworks;
in pkgs.stdenv.mkDerivation {
    name = "orizentic";

    buildInputs = [ pkgs.rustc
                    pkgs.cargo
                    pkgs.rustfmt
                    unstable.carnix
                    frameworks.Security
                    frameworks.CoreFoundation
                    frameworks.CoreServices
                  ];
}
