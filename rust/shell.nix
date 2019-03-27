let
    pkgs = import <nixpkgs-18.09> {};
    unstable = import <nixpkgs> {};
    frameworks = pkgs.darwin.apple_sdk.frameworks;
in pkgs.stdenv.mkDerivation {
    name = "orizentic";

    buildInputs = [ unstable.rustc
                    unstable.cargo
                    unstable.rustfmt
                    unstable.carnix
                    frameworks.Security
                    frameworks.CoreFoundation
                    frameworks.CoreServices
                  ];
}
