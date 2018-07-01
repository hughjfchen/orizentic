let
    pkgs = import <stable> {};
in pkgs.stdenv.mkDerivation {
    name = "orizentic";

    buildInputs = [ pkgs.rustc
                    pkgs.cargo
                  ];

    shellHook = ''
        export PS1="[$name] \[$txtgrn\]\u@\h\[$txtwht\]:\[$bldpur\]\w \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty \[$bldylw\]\$aws_env\[$txtrst\]\$ "
    '';
}
