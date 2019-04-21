{ lib, buildRustCrate, buildRustCrateHelpers }:
with buildRustCrateHelpers;
let inherit (lib.lists) fold;
    inherit (lib.attrsets) recursiveUpdate;
in
rec {

# ansi_term-0.11.0

  crates.ansi_term."0.11.0" = deps: { features?(features_.ansi_term."0.11.0" deps {}) }: buildRustCrate {
    crateName = "ansi_term";
    version = "0.11.0";
    authors = [ "ogham@bsago.me" "Ryan Scheel (Havvy) <ryan.havvy@gmail.com>" "Josh Triplett <josh@joshtriplett.org>" ];
    sha256 = "08fk0p2xvkqpmz3zlrwnf6l8sj2vngw464rvzspzp31sbgxbwm4v";
    dependencies = (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."ansi_term"."0.11.0"."winapi"}" deps)
    ]) else []);
  };
  features_.ansi_term."0.11.0" = deps: f: updateFeatures f (rec {
    ansi_term."0.11.0".default = (f.ansi_term."0.11.0".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.ansi_term."0.11.0".winapi}"."consoleapi" = true; }
      { "${deps.ansi_term."0.11.0".winapi}"."errhandlingapi" = true; }
      { "${deps.ansi_term."0.11.0".winapi}"."processenv" = true; }
      { "${deps.ansi_term."0.11.0".winapi}".default = true; }
    ];
  }) [
    (features_.winapi."${deps."ansi_term"."0.11.0"."winapi"}" deps)
  ];


# end
# atty-0.2.10

  crates.atty."0.2.10" = deps: { features?(features_.atty."0.2.10" deps {}) }: buildRustCrate {
    crateName = "atty";
    version = "0.2.10";
    authors = [ "softprops <d.tangren@gmail.com>" ];
    sha256 = "1h26lssj8rwaz0xhwwm5a645r49yly211amfmd243m3m0jl49i2c";
    dependencies = (if kernel == "redox" then mapFeatures features ([
      (crates."termion"."${deps."atty"."0.2.10"."termion"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."atty"."0.2.10"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."atty"."0.2.10"."winapi"}" deps)
    ]) else []);
  };
  features_.atty."0.2.10" = deps: f: updateFeatures f (rec {
    atty."0.2.10".default = (f.atty."0.2.10".default or true);
    libc."${deps.atty."0.2.10".libc}".default = (f.libc."${deps.atty."0.2.10".libc}".default or false);
    termion."${deps.atty."0.2.10".termion}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.atty."0.2.10".winapi}"."consoleapi" = true; }
      { "${deps.atty."0.2.10".winapi}"."minwinbase" = true; }
      { "${deps.atty."0.2.10".winapi}"."minwindef" = true; }
      { "${deps.atty."0.2.10".winapi}"."processenv" = true; }
      { "${deps.atty."0.2.10".winapi}"."winbase" = true; }
      { "${deps.atty."0.2.10".winapi}".default = true; }
    ];
  }) [
    (features_.termion."${deps."atty"."0.2.10"."termion"}" deps)
    (features_.libc."${deps."atty"."0.2.10"."libc"}" deps)
    (features_.winapi."${deps."atty"."0.2.10"."winapi"}" deps)
  ];


# end
# base64-0.9.2

  crates.base64."0.9.2" = deps: { features?(features_.base64."0.9.2" deps {}) }: buildRustCrate {
    crateName = "base64";
    version = "0.9.2";
    authors = [ "Alice Maz <alice@alicemaz.com>" "Marshall Pierce <marshall@mpierce.org>" ];
    sha256 = "0g4xxl8jhwjhvr69qlxdmbzd521xcn5j67lhkr20nh7ajvl6k0l1";
    dependencies = mapFeatures features ([
      (crates."byteorder"."${deps."base64"."0.9.2"."byteorder"}" deps)
      (crates."safemem"."${deps."base64"."0.9.2"."safemem"}" deps)
    ]);
  };
  features_.base64."0.9.2" = deps: f: updateFeatures f (rec {
    base64."0.9.2".default = (f.base64."0.9.2".default or true);
    byteorder."${deps.base64."0.9.2".byteorder}".default = true;
    safemem."${deps.base64."0.9.2".safemem}".default = true;
  }) [
    (features_.byteorder."${deps."base64"."0.9.2"."byteorder"}" deps)
    (features_.safemem."${deps."base64"."0.9.2"."safemem"}" deps)
  ];


# end
# bitflags-1.0.3

  crates.bitflags."1.0.3" = deps: { features?(features_.bitflags."1.0.3" deps {}) }: buildRustCrate {
    crateName = "bitflags";
    version = "1.0.3";
    authors = [ "The Rust Project Developers" ];
    sha256 = "162p4w4h1ad76awq6b5yivmls3d50m9cl27d8g588lsps6g8s5rw";
    features = mkFeatures (features."bitflags"."1.0.3" or {});
  };
  features_.bitflags."1.0.3" = deps: f: updateFeatures f (rec {
    bitflags."1.0.3".default = (f.bitflags."1.0.3".default or true);
  }) [];


# end
# byteorder-1.2.3

  crates.byteorder."1.2.3" = deps: { features?(features_.byteorder."1.2.3" deps {}) }: buildRustCrate {
    crateName = "byteorder";
    version = "1.2.3";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "1xghv5f5rydzsam8lnfqhfk090i8a1knb77ikbs0ik44bvrw2ij3";
    features = mkFeatures (features."byteorder"."1.2.3" or {});
  };
  features_.byteorder."1.2.3" = deps: f: updateFeatures f (rec {
    byteorder = fold recursiveUpdate {} [
      { "1.2.3".default = (f.byteorder."1.2.3".default or true); }
      { "1.2.3".std =
        (f.byteorder."1.2.3".std or false) ||
        (f.byteorder."1.2.3".default or false) ||
        (byteorder."1.2.3"."default" or false); }
    ];
  }) [];


# end
# cc-1.0.31

  crates.cc."1.0.31" = deps: { features?(features_.cc."1.0.31" deps {}) }: buildRustCrate {
    crateName = "cc";
    version = "1.0.31";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "1a576gp1gp69v8kprwgk8ryxs3sc9v9g06cd3ggxm9jrzrcc4x5n";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."cc"."1.0.31" or {});
  };
  features_.cc."1.0.31" = deps: f: updateFeatures f (rec {
    cc = fold recursiveUpdate {} [
      { "1.0.31".default = (f.cc."1.0.31".default or true); }
      { "1.0.31".rayon =
        (f.cc."1.0.31".rayon or false) ||
        (f.cc."1.0.31".parallel or false) ||
        (cc."1.0.31"."parallel" or false); }
    ];
  }) [];


# end
# cfg-if-0.1.4

  crates.cfg_if."0.1.4" = deps: { features?(features_.cfg_if."0.1.4" deps {}) }: buildRustCrate {
    crateName = "cfg-if";
    version = "0.1.4";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "0n5baxk53dvqjymzwynq55wb805b24390qx1n16zi8fjzq90j7k4";
  };
  features_.cfg_if."0.1.4" = deps: f: updateFeatures f (rec {
    cfg_if."0.1.4".default = (f.cfg_if."0.1.4".default or true);
  }) [];


# end
# chrono-0.4.4

  crates.chrono."0.4.4" = deps: { features?(features_.chrono."0.4.4" deps {}) }: buildRustCrate {
    crateName = "chrono";
    version = "0.4.4";
    authors = [ "Kang Seonghoon <public+rust@mearie.org>" "Brandon W Maister <quodlibetor@gmail.com>" ];
    sha256 = "169h5rlrb9df3yvnzihjw39cjipvz90qgf9211pivms2s9xnpqpr";
    dependencies = mapFeatures features ([
      (crates."num_integer"."${deps."chrono"."0.4.4"."num_integer"}" deps)
      (crates."num_traits"."${deps."chrono"."0.4.4"."num_traits"}" deps)
    ]
      ++ (if features.chrono."0.4.4".serde or false then [ (crates.serde."${deps."chrono"."0.4.4".serde}" deps) ] else [])
      ++ (if features.chrono."0.4.4".time or false then [ (crates.time."${deps."chrono"."0.4.4".time}" deps) ] else []));
    features = mkFeatures (features."chrono"."0.4.4" or {});
  };
  features_.chrono."0.4.4" = deps: f: updateFeatures f (rec {
    chrono = fold recursiveUpdate {} [
      { "0.4.4".clock =
        (f.chrono."0.4.4".clock or false) ||
        (f.chrono."0.4.4".default or false) ||
        (chrono."0.4.4"."default" or false); }
      { "0.4.4".default = (f.chrono."0.4.4".default or true); }
      { "0.4.4".time =
        (f.chrono."0.4.4".time or false) ||
        (f.chrono."0.4.4".clock or false) ||
        (chrono."0.4.4"."clock" or false); }
    ];
    num_integer."${deps.chrono."0.4.4".num_integer}".default = (f.num_integer."${deps.chrono."0.4.4".num_integer}".default or false);
    num_traits."${deps.chrono."0.4.4".num_traits}".default = (f.num_traits."${deps.chrono."0.4.4".num_traits}".default or false);
    serde."${deps.chrono."0.4.4".serde}".default = true;
    time."${deps.chrono."0.4.4".time}".default = true;
  }) [
    (features_.num_integer."${deps."chrono"."0.4.4"."num_integer"}" deps)
    (features_.num_traits."${deps."chrono"."0.4.4"."num_traits"}" deps)
    (features_.serde."${deps."chrono"."0.4.4"."serde"}" deps)
    (features_.time."${deps."chrono"."0.4.4"."time"}" deps)
  ];


# end
# clap-2.32.0

  crates.clap."2.32.0" = deps: { features?(features_.clap."2.32.0" deps {}) }: buildRustCrate {
    crateName = "clap";
    version = "2.32.0";
    authors = [ "Kevin K. <kbknapp@gmail.com>" ];
    sha256 = "1hdjf0janvpjkwrjdjx1mm2aayzr54k72w6mriyr0n5anjkcj1lx";
    dependencies = mapFeatures features ([
      (crates."bitflags"."${deps."clap"."2.32.0"."bitflags"}" deps)
      (crates."textwrap"."${deps."clap"."2.32.0"."textwrap"}" deps)
      (crates."unicode_width"."${deps."clap"."2.32.0"."unicode_width"}" deps)
    ]
      ++ (if features.clap."2.32.0".atty or false then [ (crates.atty."${deps."clap"."2.32.0".atty}" deps) ] else [])
      ++ (if features.clap."2.32.0".strsim or false then [ (crates.strsim."${deps."clap"."2.32.0".strsim}" deps) ] else [])
      ++ (if features.clap."2.32.0".vec_map or false then [ (crates.vec_map."${deps."clap"."2.32.0".vec_map}" deps) ] else []))
      ++ (if !(kernel == "windows") then mapFeatures features ([
    ]
      ++ (if features.clap."2.32.0".ansi_term or false then [ (crates.ansi_term."${deps."clap"."2.32.0".ansi_term}" deps) ] else [])) else []);
    features = mkFeatures (features."clap"."2.32.0" or {});
  };
  features_.clap."2.32.0" = deps: f: updateFeatures f (rec {
    ansi_term."${deps.clap."2.32.0".ansi_term}".default = true;
    atty."${deps.clap."2.32.0".atty}".default = true;
    bitflags."${deps.clap."2.32.0".bitflags}".default = true;
    clap = fold recursiveUpdate {} [
      { "2.32.0".ansi_term =
        (f.clap."2.32.0".ansi_term or false) ||
        (f.clap."2.32.0".color or false) ||
        (clap."2.32.0"."color" or false); }
      { "2.32.0".atty =
        (f.clap."2.32.0".atty or false) ||
        (f.clap."2.32.0".color or false) ||
        (clap."2.32.0"."color" or false); }
      { "2.32.0".clippy =
        (f.clap."2.32.0".clippy or false) ||
        (f.clap."2.32.0".lints or false) ||
        (clap."2.32.0"."lints" or false); }
      { "2.32.0".color =
        (f.clap."2.32.0".color or false) ||
        (f.clap."2.32.0".default or false) ||
        (clap."2.32.0"."default" or false); }
      { "2.32.0".default = (f.clap."2.32.0".default or true); }
      { "2.32.0".strsim =
        (f.clap."2.32.0".strsim or false) ||
        (f.clap."2.32.0".suggestions or false) ||
        (clap."2.32.0"."suggestions" or false); }
      { "2.32.0".suggestions =
        (f.clap."2.32.0".suggestions or false) ||
        (f.clap."2.32.0".default or false) ||
        (clap."2.32.0"."default" or false); }
      { "2.32.0".term_size =
        (f.clap."2.32.0".term_size or false) ||
        (f.clap."2.32.0".wrap_help or false) ||
        (clap."2.32.0"."wrap_help" or false); }
      { "2.32.0".vec_map =
        (f.clap."2.32.0".vec_map or false) ||
        (f.clap."2.32.0".default or false) ||
        (clap."2.32.0"."default" or false); }
      { "2.32.0".yaml =
        (f.clap."2.32.0".yaml or false) ||
        (f.clap."2.32.0".doc or false) ||
        (clap."2.32.0"."doc" or false); }
      { "2.32.0".yaml-rust =
        (f.clap."2.32.0".yaml-rust or false) ||
        (f.clap."2.32.0".yaml or false) ||
        (clap."2.32.0"."yaml" or false); }
    ];
    strsim."${deps.clap."2.32.0".strsim}".default = true;
    textwrap = fold recursiveUpdate {} [
      { "${deps.clap."2.32.0".textwrap}"."term_size" =
        (f.textwrap."${deps.clap."2.32.0".textwrap}"."term_size" or false) ||
        (clap."2.32.0"."wrap_help" or false) ||
        (f."clap"."2.32.0"."wrap_help" or false); }
      { "${deps.clap."2.32.0".textwrap}".default = true; }
    ];
    unicode_width."${deps.clap."2.32.0".unicode_width}".default = true;
    vec_map."${deps.clap."2.32.0".vec_map}".default = true;
  }) [
    (features_.atty."${deps."clap"."2.32.0"."atty"}" deps)
    (features_.bitflags."${deps."clap"."2.32.0"."bitflags"}" deps)
    (features_.strsim."${deps."clap"."2.32.0"."strsim"}" deps)
    (features_.textwrap."${deps."clap"."2.32.0"."textwrap"}" deps)
    (features_.unicode_width."${deps."clap"."2.32.0"."unicode_width"}" deps)
    (features_.vec_map."${deps."clap"."2.32.0"."vec_map"}" deps)
    (features_.ansi_term."${deps."clap"."2.32.0"."ansi_term"}" deps)
  ];


# end
# dtoa-0.4.2

  crates.dtoa."0.4.2" = deps: { features?(features_.dtoa."0.4.2" deps {}) }: buildRustCrate {
    crateName = "dtoa";
    version = "0.4.2";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1bxsh6fags7nr36vlz07ik2a1rzyipc8x1y30kjk832hf2pzadmw";
  };
  features_.dtoa."0.4.2" = deps: f: updateFeatures f (rec {
    dtoa."0.4.2".default = (f.dtoa."0.4.2".default or true);
  }) [];


# end
# either-1.5.0

  crates.either."1.5.0" = deps: { features?(features_.either."1.5.0" deps {}) }: buildRustCrate {
    crateName = "either";
    version = "1.5.0";
    authors = [ "bluss" ];
    sha256 = "1f7kl2ln01y02m8fpd2zrdjiwqmgfvl9nxxrfry3k19d1gd2bsvz";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."either"."1.5.0" or {});
  };
  features_.either."1.5.0" = deps: f: updateFeatures f (rec {
    either = fold recursiveUpdate {} [
      { "1.5.0".default = (f.either."1.5.0".default or true); }
      { "1.5.0".use_std =
        (f.either."1.5.0".use_std or false) ||
        (f.either."1.5.0".default or false) ||
        (either."1.5.0"."default" or false); }
    ];
  }) [];


# end
# fuchsia-zircon-0.3.3

  crates.fuchsia_zircon."0.3.3" = deps: { features?(features_.fuchsia_zircon."0.3.3" deps {}) }: buildRustCrate {
    crateName = "fuchsia-zircon";
    version = "0.3.3";
    authors = [ "Raph Levien <raph@google.com>" ];
    sha256 = "0jrf4shb1699r4la8z358vri8318w4mdi6qzfqy30p2ymjlca4gk";
    dependencies = mapFeatures features ([
      (crates."bitflags"."${deps."fuchsia_zircon"."0.3.3"."bitflags"}" deps)
      (crates."fuchsia_zircon_sys"."${deps."fuchsia_zircon"."0.3.3"."fuchsia_zircon_sys"}" deps)
    ]);
  };
  features_.fuchsia_zircon."0.3.3" = deps: f: updateFeatures f (rec {
    bitflags."${deps.fuchsia_zircon."0.3.3".bitflags}".default = true;
    fuchsia_zircon."0.3.3".default = (f.fuchsia_zircon."0.3.3".default or true);
    fuchsia_zircon_sys."${deps.fuchsia_zircon."0.3.3".fuchsia_zircon_sys}".default = true;
  }) [
    (features_.bitflags."${deps."fuchsia_zircon"."0.3.3"."bitflags"}" deps)
    (features_.fuchsia_zircon_sys."${deps."fuchsia_zircon"."0.3.3"."fuchsia_zircon_sys"}" deps)
  ];


# end
# fuchsia-zircon-sys-0.3.3

  crates.fuchsia_zircon_sys."0.3.3" = deps: { features?(features_.fuchsia_zircon_sys."0.3.3" deps {}) }: buildRustCrate {
    crateName = "fuchsia-zircon-sys";
    version = "0.3.3";
    authors = [ "Raph Levien <raph@google.com>" ];
    sha256 = "08jp1zxrm9jbrr6l26bjal4dbm8bxfy57ickdgibsqxr1n9j3hf5";
  };
  features_.fuchsia_zircon_sys."0.3.3" = deps: f: updateFeatures f (rec {
    fuchsia_zircon_sys."0.3.3".default = (f.fuchsia_zircon_sys."0.3.3".default or true);
  }) [];


# end
# itertools-0.7.8

  crates.itertools."0.7.8" = deps: { features?(features_.itertools."0.7.8" deps {}) }: buildRustCrate {
    crateName = "itertools";
    version = "0.7.8";
    authors = [ "bluss" ];
    sha256 = "0ib30cd7d1icjxsa13mji1gry3grp72kx8p33yd84mphdbc3d357";
    dependencies = mapFeatures features ([
      (crates."either"."${deps."itertools"."0.7.8"."either"}" deps)
    ]);
    features = mkFeatures (features."itertools"."0.7.8" or {});
  };
  features_.itertools."0.7.8" = deps: f: updateFeatures f (rec {
    either."${deps.itertools."0.7.8".either}".default = (f.either."${deps.itertools."0.7.8".either}".default or false);
    itertools = fold recursiveUpdate {} [
      { "0.7.8".default = (f.itertools."0.7.8".default or true); }
      { "0.7.8".use_std =
        (f.itertools."0.7.8".use_std or false) ||
        (f.itertools."0.7.8".default or false) ||
        (itertools."0.7.8"."default" or false); }
    ];
  }) [
    (features_.either."${deps."itertools"."0.7.8"."either"}" deps)
  ];


# end
# itoa-0.4.1

  crates.itoa."0.4.1" = deps: { features?(features_.itoa."0.4.1" deps {}) }: buildRustCrate {
    crateName = "itoa";
    version = "0.4.1";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1jyrsmrm5q4r2ipmq5hvvkqg0mgnlbk44lm7gr0v9ymvbrh2gbij";
    features = mkFeatures (features."itoa"."0.4.1" or {});
  };
  features_.itoa."0.4.1" = deps: f: updateFeatures f (rec {
    itoa = fold recursiveUpdate {} [
      { "0.4.1".default = (f.itoa."0.4.1".default or true); }
      { "0.4.1".std =
        (f.itoa."0.4.1".std or false) ||
        (f.itoa."0.4.1".default or false) ||
        (itoa."0.4.1"."default" or false); }
    ];
  }) [];


# end
# jsonwebtoken-5.0.1

  crates.jsonwebtoken."5.0.1" = deps: { features?(features_.jsonwebtoken."5.0.1" deps {}) }: buildRustCrate {
    crateName = "jsonwebtoken";
    version = "5.0.1";
    authors = [ "Vincent Prouillet <prouillet.vincent@gmail.com>" ];
    sha256 = "09jpcc86zpwdm41g45p5l4na4s48cn6ggcsaq8clfmqv0754c6id";
    dependencies = mapFeatures features ([
      (crates."base64"."${deps."jsonwebtoken"."5.0.1"."base64"}" deps)
      (crates."chrono"."${deps."jsonwebtoken"."5.0.1"."chrono"}" deps)
      (crates."ring"."${deps."jsonwebtoken"."5.0.1"."ring"}" deps)
      (crates."serde"."${deps."jsonwebtoken"."5.0.1"."serde"}" deps)
      (crates."serde_derive"."${deps."jsonwebtoken"."5.0.1"."serde_derive"}" deps)
      (crates."serde_json"."${deps."jsonwebtoken"."5.0.1"."serde_json"}" deps)
      (crates."untrusted"."${deps."jsonwebtoken"."5.0.1"."untrusted"}" deps)
    ]);
  };
  features_.jsonwebtoken."5.0.1" = deps: f: updateFeatures f (rec {
    base64."${deps.jsonwebtoken."5.0.1".base64}".default = true;
    chrono."${deps.jsonwebtoken."5.0.1".chrono}".default = true;
    jsonwebtoken."5.0.1".default = (f.jsonwebtoken."5.0.1".default or true);
    ring = fold recursiveUpdate {} [
      { "${deps.jsonwebtoken."5.0.1".ring}"."dev_urandom_fallback" = true; }
      { "${deps.jsonwebtoken."5.0.1".ring}"."rsa_signing" = true; }
      { "${deps.jsonwebtoken."5.0.1".ring}".default = true; }
    ];
    serde."${deps.jsonwebtoken."5.0.1".serde}".default = true;
    serde_derive."${deps.jsonwebtoken."5.0.1".serde_derive}".default = true;
    serde_json."${deps.jsonwebtoken."5.0.1".serde_json}".default = true;
    untrusted."${deps.jsonwebtoken."5.0.1".untrusted}".default = true;
  }) [
    (features_.base64."${deps."jsonwebtoken"."5.0.1"."base64"}" deps)
    (features_.chrono."${deps."jsonwebtoken"."5.0.1"."chrono"}" deps)
    (features_.ring."${deps."jsonwebtoken"."5.0.1"."ring"}" deps)
    (features_.serde."${deps."jsonwebtoken"."5.0.1"."serde"}" deps)
    (features_.serde_derive."${deps."jsonwebtoken"."5.0.1"."serde_derive"}" deps)
    (features_.serde_json."${deps."jsonwebtoken"."5.0.1"."serde_json"}" deps)
    (features_.untrusted."${deps."jsonwebtoken"."5.0.1"."untrusted"}" deps)
  ];


# end
# lazy_static-1.3.0

  crates.lazy_static."1.3.0" = deps: { features?(features_.lazy_static."1.3.0" deps {}) }: buildRustCrate {
    crateName = "lazy_static";
    version = "1.3.0";
    authors = [ "Marvin Löbel <loebel.marvin@gmail.com>" ];
    sha256 = "1vv47va18ydk7dx5paz88g3jy1d3lwbx6qpxkbj8gyfv770i4b1y";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."lazy_static"."1.3.0" or {});
  };
  features_.lazy_static."1.3.0" = deps: f: updateFeatures f (rec {
    lazy_static = fold recursiveUpdate {} [
      { "1.3.0".default = (f.lazy_static."1.3.0".default or true); }
      { "1.3.0".spin =
        (f.lazy_static."1.3.0".spin or false) ||
        (f.lazy_static."1.3.0".spin_no_std or false) ||
        (lazy_static."1.3.0"."spin_no_std" or false); }
    ];
  }) [];


# end
# libc-0.2.42

  crates.libc."0.2.42" = deps: { features?(features_.libc."0.2.42" deps {}) }: buildRustCrate {
    crateName = "libc";
    version = "0.2.42";
    authors = [ "The Rust Project Developers" ];
    sha256 = "064v49hz1zpl081w8c4vwikrkhaxp06y4i9l7x7wx6bjpwp19pjx";
    features = mkFeatures (features."libc"."0.2.42" or {});
  };
  features_.libc."0.2.42" = deps: f: updateFeatures f (rec {
    libc = fold recursiveUpdate {} [
      { "0.2.42".default = (f.libc."0.2.42".default or true); }
      { "0.2.42".use_std =
        (f.libc."0.2.42".use_std or false) ||
        (f.libc."0.2.42".default or false) ||
        (libc."0.2.42"."default" or false); }
    ];
  }) [];


# end
# linked-hash-map-0.5.1

  crates.linked_hash_map."0.5.1" = deps: { features?(features_.linked_hash_map."0.5.1" deps {}) }: buildRustCrate {
    crateName = "linked-hash-map";
    version = "0.5.1";
    authors = [ "Stepan Koltsov <stepan.koltsov@gmail.com>" "Andrew Paseltiner <apaseltiner@gmail.com>" ];
    sha256 = "1f29c7j53z7w5v0g115yii9dmmbsahr93ak375g48vi75v3p4030";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."linked_hash_map"."0.5.1" or {});
  };
  features_.linked_hash_map."0.5.1" = deps: f: updateFeatures f (rec {
    linked_hash_map = fold recursiveUpdate {} [
      { "0.5.1".default = (f.linked_hash_map."0.5.1".default or true); }
      { "0.5.1".heapsize =
        (f.linked_hash_map."0.5.1".heapsize or false) ||
        (f.linked_hash_map."0.5.1".heapsize_impl or false) ||
        (linked_hash_map."0.5.1"."heapsize_impl" or false); }
      { "0.5.1".serde =
        (f.linked_hash_map."0.5.1".serde or false) ||
        (f.linked_hash_map."0.5.1".serde_impl or false) ||
        (linked_hash_map."0.5.1"."serde_impl" or false); }
      { "0.5.1".serde_test =
        (f.linked_hash_map."0.5.1".serde_test or false) ||
        (f.linked_hash_map."0.5.1".serde_impl or false) ||
        (linked_hash_map."0.5.1"."serde_impl" or false); }
    ];
  }) [];


# end
# num-integer-0.1.39

  crates.num_integer."0.1.39" = deps: { features?(features_.num_integer."0.1.39" deps {}) }: buildRustCrate {
    crateName = "num-integer";
    version = "0.1.39";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1f42ls46cghs13qfzgbd7syib2zc6m7hlmv1qlar6c9mdxapvvbg";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."num_traits"."${deps."num_integer"."0.1.39"."num_traits"}" deps)
    ]);
    features = mkFeatures (features."num_integer"."0.1.39" or {});
  };
  features_.num_integer."0.1.39" = deps: f: updateFeatures f (rec {
    num_integer = fold recursiveUpdate {} [
      { "0.1.39".default = (f.num_integer."0.1.39".default or true); }
      { "0.1.39".std =
        (f.num_integer."0.1.39".std or false) ||
        (f.num_integer."0.1.39".default or false) ||
        (num_integer."0.1.39"."default" or false); }
    ];
    num_traits = fold recursiveUpdate {} [
      { "${deps.num_integer."0.1.39".num_traits}"."i128" =
        (f.num_traits."${deps.num_integer."0.1.39".num_traits}"."i128" or false) ||
        (num_integer."0.1.39"."i128" or false) ||
        (f."num_integer"."0.1.39"."i128" or false); }
      { "${deps.num_integer."0.1.39".num_traits}"."std" =
        (f.num_traits."${deps.num_integer."0.1.39".num_traits}"."std" or false) ||
        (num_integer."0.1.39"."std" or false) ||
        (f."num_integer"."0.1.39"."std" or false); }
      { "${deps.num_integer."0.1.39".num_traits}".default = (f.num_traits."${deps.num_integer."0.1.39".num_traits}".default or false); }
    ];
  }) [
    (features_.num_traits."${deps."num_integer"."0.1.39"."num_traits"}" deps)
  ];


# end
# num-traits-0.2.5

  crates.num_traits."0.2.5" = deps: { features?(features_.num_traits."0.2.5" deps {}) }: buildRustCrate {
    crateName = "num-traits";
    version = "0.2.5";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0ql203ca6lzppksy4fsfnpz3kq96vwlwvyn3ahvnd9g6k9f5ncj0";
    build = "build.rs";
    features = mkFeatures (features."num_traits"."0.2.5" or {});
  };
  features_.num_traits."0.2.5" = deps: f: updateFeatures f (rec {
    num_traits = fold recursiveUpdate {} [
      { "0.2.5".default = (f.num_traits."0.2.5".default or true); }
      { "0.2.5".std =
        (f.num_traits."0.2.5".std or false) ||
        (f.num_traits."0.2.5".default or false) ||
        (num_traits."0.2.5"."default" or false); }
    ];
  }) [];


# end
# proc-macro2-0.4.6

  crates.proc_macro2."0.4.6" = deps: { features?(features_.proc_macro2."0.4.6" deps {}) }: buildRustCrate {
    crateName = "proc-macro2";
    version = "0.4.6";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "0lnxcavz5nqxypjyz6isffqiyxb3h0021nkn8djsl33a4qd4pxv6";
    dependencies = mapFeatures features ([
      (crates."unicode_xid"."${deps."proc_macro2"."0.4.6"."unicode_xid"}" deps)
    ]);
    features = mkFeatures (features."proc_macro2"."0.4.6" or {});
  };
  features_.proc_macro2."0.4.6" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "0.4.6".default = (f.proc_macro2."0.4.6".default or true); }
      { "0.4.6".proc-macro =
        (f.proc_macro2."0.4.6".proc-macro or false) ||
        (f.proc_macro2."0.4.6".default or false) ||
        (proc_macro2."0.4.6"."default" or false) ||
        (f.proc_macro2."0.4.6".nightly or false) ||
        (proc_macro2."0.4.6"."nightly" or false); }
    ];
    unicode_xid."${deps.proc_macro2."0.4.6".unicode_xid}".default = true;
  }) [
    (features_.unicode_xid."${deps."proc_macro2"."0.4.6"."unicode_xid"}" deps)
  ];


# end
# quote-0.6.3

  crates.quote."0.6.3" = deps: { features?(features_.quote."0.6.3" deps {}) }: buildRustCrate {
    crateName = "quote";
    version = "0.6.3";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1bqm3fjww2ivnshzxg54nnn1dxrx1cmvpjc5d50xjvjfg9xjb4b5";
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."quote"."0.6.3"."proc_macro2"}" deps)
    ]);
    features = mkFeatures (features."quote"."0.6.3" or {});
  };
  features_.quote."0.6.3" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "${deps.quote."0.6.3".proc_macro2}"."proc-macro" =
        (f.proc_macro2."${deps.quote."0.6.3".proc_macro2}"."proc-macro" or false) ||
        (quote."0.6.3"."proc-macro" or false) ||
        (f."quote"."0.6.3"."proc-macro" or false); }
      { "${deps.quote."0.6.3".proc_macro2}".default = (f.proc_macro2."${deps.quote."0.6.3".proc_macro2}".default or false); }
    ];
    quote = fold recursiveUpdate {} [
      { "0.6.3".default = (f.quote."0.6.3".default or true); }
      { "0.6.3".proc-macro =
        (f.quote."0.6.3".proc-macro or false) ||
        (f.quote."0.6.3".default or false) ||
        (quote."0.6.3"."default" or false); }
    ];
  }) [
    (features_.proc_macro2."${deps."quote"."0.6.3"."proc_macro2"}" deps)
  ];


# end
# rand-0.4.2

  crates.rand."0.4.2" = deps: { features?(features_.rand."0.4.2" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.4.2";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0h8pkg23wb67i8904sm76iyr1jlmhklb85vbpz9c9191a24xzkfm";
    dependencies = (if kernel == "fuchsia" then mapFeatures features ([
      (crates."fuchsia_zircon"."${deps."rand"."0.4.2"."fuchsia_zircon"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
    ]
      ++ (if features.rand."0.4.2".libc or false then [ (crates.libc."${deps."rand"."0.4.2".libc}" deps) ] else [])) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand"."0.4.2"."winapi"}" deps)
    ]) else []);
    features = mkFeatures (features."rand"."0.4.2" or {});
  };
  features_.rand."0.4.2" = deps: f: updateFeatures f (rec {
    fuchsia_zircon."${deps.rand."0.4.2".fuchsia_zircon}".default = true;
    libc."${deps.rand."0.4.2".libc}".default = true;
    rand = fold recursiveUpdate {} [
      { "0.4.2".default = (f.rand."0.4.2".default or true); }
      { "0.4.2".i128_support =
        (f.rand."0.4.2".i128_support or false) ||
        (f.rand."0.4.2".nightly or false) ||
        (rand."0.4.2"."nightly" or false); }
      { "0.4.2".libc =
        (f.rand."0.4.2".libc or false) ||
        (f.rand."0.4.2".std or false) ||
        (rand."0.4.2"."std" or false); }
      { "0.4.2".std =
        (f.rand."0.4.2".std or false) ||
        (f.rand."0.4.2".default or false) ||
        (rand."0.4.2"."default" or false); }
    ];
    winapi = fold recursiveUpdate {} [
      { "${deps.rand."0.4.2".winapi}"."minwindef" = true; }
      { "${deps.rand."0.4.2".winapi}"."ntsecapi" = true; }
      { "${deps.rand."0.4.2".winapi}"."profileapi" = true; }
      { "${deps.rand."0.4.2".winapi}"."winnt" = true; }
      { "${deps.rand."0.4.2".winapi}".default = true; }
    ];
  }) [
    (features_.fuchsia_zircon."${deps."rand"."0.4.2"."fuchsia_zircon"}" deps)
    (features_.libc."${deps."rand"."0.4.2"."libc"}" deps)
    (features_.winapi."${deps."rand"."0.4.2"."winapi"}" deps)
  ];


# end
# redox_syscall-0.1.40

  crates.redox_syscall."0.1.40" = deps: { features?(features_.redox_syscall."0.1.40" deps {}) }: buildRustCrate {
    crateName = "redox_syscall";
    version = "0.1.40";
    authors = [ "Jeremy Soller <jackpot51@gmail.com>" ];
    sha256 = "132rnhrq49l3z7gjrwj2zfadgw6q0355s6a7id7x7c0d7sk72611";
    libName = "syscall";
  };
  features_.redox_syscall."0.1.40" = deps: f: updateFeatures f (rec {
    redox_syscall."0.1.40".default = (f.redox_syscall."0.1.40".default or true);
  }) [];


# end
# redox_termios-0.1.1

  crates.redox_termios."0.1.1" = deps: { features?(features_.redox_termios."0.1.1" deps {}) }: buildRustCrate {
    crateName = "redox_termios";
    version = "0.1.1";
    authors = [ "Jeremy Soller <jackpot51@gmail.com>" ];
    sha256 = "04s6yyzjca552hdaqlvqhp3vw0zqbc304md5czyd3axh56iry8wh";
    libPath = "src/lib.rs";
    dependencies = mapFeatures features ([
      (crates."redox_syscall"."${deps."redox_termios"."0.1.1"."redox_syscall"}" deps)
    ]);
  };
  features_.redox_termios."0.1.1" = deps: f: updateFeatures f (rec {
    redox_syscall."${deps.redox_termios."0.1.1".redox_syscall}".default = true;
    redox_termios."0.1.1".default = (f.redox_termios."0.1.1".default or true);
  }) [
    (features_.redox_syscall."${deps."redox_termios"."0.1.1"."redox_syscall"}" deps)
  ];


# end
# ring-0.13.5

  crates.ring."0.13.5" = deps: { features?(features_.ring."0.13.5" deps {}) }: buildRustCrate {
    crateName = "ring";
    version = "0.13.5";
    authors = [ "Brian Smith <brian@briansmith.org>" ];
    sha256 = "0b071zwzwhgmj0xyr7wqc55f4nppgjikfh53nb9m799l096s86j4";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."untrusted"."${deps."ring"."0.13.5"."untrusted"}" deps)
    ])
      ++ (if kernel == "redox" || (kernel == "linux" || kernel == "darwin") && !(kernel == "darwin" || kernel == "ios") then mapFeatures features ([
      (crates."lazy_static"."${deps."ring"."0.13.5"."lazy_static"}" deps)
    ]) else [])
      ++ (if kernel == "linux" then mapFeatures features ([
      (crates."libc"."${deps."ring"."0.13.5"."libc"}" deps)
    ]) else []);

    buildDependencies = mapFeatures features ([
      (crates."cc"."${deps."ring"."0.13.5"."cc"}" deps)
    ]);
    features = mkFeatures (features."ring"."0.13.5" or {});
  };
  features_.ring."0.13.5" = deps: f: updateFeatures f (rec {
    cc."${deps.ring."0.13.5".cc}".default = true;
    lazy_static."${deps.ring."0.13.5".lazy_static}".default = true;
    libc."${deps.ring."0.13.5".libc}".default = true;
    ring = fold recursiveUpdate {} [
      { "0.13.5".default = (f.ring."0.13.5".default or true); }
      { "0.13.5".dev_urandom_fallback =
        (f.ring."0.13.5".dev_urandom_fallback or false) ||
        (f.ring."0.13.5".default or false) ||
        (ring."0.13.5"."default" or false); }
      { "0.13.5".use_heap =
        (f.ring."0.13.5".use_heap or false) ||
        (f.ring."0.13.5".default or false) ||
        (ring."0.13.5"."default" or false) ||
        (f.ring."0.13.5".rsa_signing or false) ||
        (ring."0.13.5"."rsa_signing" or false); }
    ];
    untrusted."${deps.ring."0.13.5".untrusted}".default = true;
  }) [
    (features_.untrusted."${deps."ring"."0.13.5"."untrusted"}" deps)
    (features_.cc."${deps."ring"."0.13.5"."cc"}" deps)
    (features_.lazy_static."${deps."ring"."0.13.5"."lazy_static"}" deps)
    (features_.libc."${deps."ring"."0.13.5"."libc"}" deps)
  ];


# end
# safemem-0.2.0

  crates.safemem."0.2.0" = deps: { features?(features_.safemem."0.2.0" deps {}) }: buildRustCrate {
    crateName = "safemem";
    version = "0.2.0";
    authors = [ "Austin Bonander <austin.bonander@gmail.com>" ];
    sha256 = "058m251q202n479ip1h6s91yw3plg66vsk5mpaflssn6rs5hijdm";
  };
  features_.safemem."0.2.0" = deps: f: updateFeatures f (rec {
    safemem."0.2.0".default = (f.safemem."0.2.0".default or true);
  }) [];


# end
# serde-1.0.69

  crates.serde."1.0.69" = deps: { features?(features_.serde."1.0.69" deps {}) }: buildRustCrate {
    crateName = "serde";
    version = "1.0.69";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "0ac4x90in10qx2r1nzgq485x5n3z0fpr3q87hgbzicrh70x1lzmv";
    build = "build.rs";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."serde"."1.0.69" or {});
  };
  features_.serde."1.0.69" = deps: f: updateFeatures f (rec {
    serde = fold recursiveUpdate {} [
      { "1.0.69".default = (f.serde."1.0.69".default or true); }
      { "1.0.69".serde_derive =
        (f.serde."1.0.69".serde_derive or false) ||
        (f.serde."1.0.69".derive or false) ||
        (serde."1.0.69"."derive" or false); }
      { "1.0.69".std =
        (f.serde."1.0.69".std or false) ||
        (f.serde."1.0.69".default or false) ||
        (serde."1.0.69"."default" or false); }
      { "1.0.69".unstable =
        (f.serde."1.0.69".unstable or false) ||
        (f.serde."1.0.69".alloc or false) ||
        (serde."1.0.69"."alloc" or false); }
    ];
  }) [];


# end
# serde_derive-1.0.69

  crates.serde_derive."1.0.69" = deps: { features?(features_.serde_derive."1.0.69" deps {}) }: buildRustCrate {
    crateName = "serde_derive";
    version = "1.0.69";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1c9rdq12r82nnbqa179jsllpxw8vizq7m7v4ja49cgsys9q0575q";
    procMacro = true;
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."serde_derive"."1.0.69"."proc_macro2"}" deps)
      (crates."quote"."${deps."serde_derive"."1.0.69"."quote"}" deps)
      (crates."syn"."${deps."serde_derive"."1.0.69"."syn"}" deps)
    ]);
    features = mkFeatures (features."serde_derive"."1.0.69" or {});
  };
  features_.serde_derive."1.0.69" = deps: f: updateFeatures f (rec {
    proc_macro2."${deps.serde_derive."1.0.69".proc_macro2}".default = true;
    quote."${deps.serde_derive."1.0.69".quote}".default = true;
    serde_derive."1.0.69".default = (f.serde_derive."1.0.69".default or true);
    syn = fold recursiveUpdate {} [
      { "${deps.serde_derive."1.0.69".syn}"."visit" = true; }
      { "${deps.serde_derive."1.0.69".syn}".default = true; }
    ];
  }) [
    (features_.proc_macro2."${deps."serde_derive"."1.0.69"."proc_macro2"}" deps)
    (features_.quote."${deps."serde_derive"."1.0.69"."quote"}" deps)
    (features_.syn."${deps."serde_derive"."1.0.69"."syn"}" deps)
  ];


# end
# serde_json-1.0.22

  crates.serde_json."1.0.22" = deps: { features?(features_.serde_json."1.0.22" deps {}) }: buildRustCrate {
    crateName = "serde_json";
    version = "1.0.22";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "18yd5q5xglmqjc1w133ac84rxzrlcg7qfwwzy91gy9r10c5nzn3c";
    dependencies = mapFeatures features ([
      (crates."dtoa"."${deps."serde_json"."1.0.22"."dtoa"}" deps)
      (crates."itoa"."${deps."serde_json"."1.0.22"."itoa"}" deps)
      (crates."serde"."${deps."serde_json"."1.0.22"."serde"}" deps)
    ]);
    features = mkFeatures (features."serde_json"."1.0.22" or {});
  };
  features_.serde_json."1.0.22" = deps: f: updateFeatures f (rec {
    dtoa."${deps.serde_json."1.0.22".dtoa}".default = true;
    itoa."${deps.serde_json."1.0.22".itoa}".default = true;
    serde."${deps.serde_json."1.0.22".serde}".default = true;
    serde_json = fold recursiveUpdate {} [
      { "1.0.22".default = (f.serde_json."1.0.22".default or true); }
      { "1.0.22".indexmap =
        (f.serde_json."1.0.22".indexmap or false) ||
        (f.serde_json."1.0.22".preserve_order or false) ||
        (serde_json."1.0.22"."preserve_order" or false); }
    ];
  }) [
    (features_.dtoa."${deps."serde_json"."1.0.22"."dtoa"}" deps)
    (features_.itoa."${deps."serde_json"."1.0.22"."itoa"}" deps)
    (features_.serde."${deps."serde_json"."1.0.22"."serde"}" deps)
  ];


# end
# strsim-0.7.0

  crates.strsim."0.7.0" = deps: { features?(features_.strsim."0.7.0" deps {}) }: buildRustCrate {
    crateName = "strsim";
    version = "0.7.0";
    authors = [ "Danny Guo <dannyguo91@gmail.com>" ];
    sha256 = "0fy0k5f2705z73mb3x9459bpcvrx4ky8jpr4zikcbiwan4bnm0iv";
  };
  features_.strsim."0.7.0" = deps: f: updateFeatures f (rec {
    strsim."0.7.0".default = (f.strsim."0.7.0".default or true);
  }) [];


# end
# syn-0.14.4

  crates.syn."0.14.4" = deps: { features?(features_.syn."0.14.4" deps {}) }: buildRustCrate {
    crateName = "syn";
    version = "0.14.4";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "0hh8ph4pr3cg8kld4b5j6d8lh2q4sl6dhwwka4v15ip5lzd4ra7d";
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."syn"."0.14.4"."proc_macro2"}" deps)
      (crates."unicode_xid"."${deps."syn"."0.14.4"."unicode_xid"}" deps)
    ]
      ++ (if features.syn."0.14.4".quote or false then [ (crates.quote."${deps."syn"."0.14.4".quote}" deps) ] else []));
    features = mkFeatures (features."syn"."0.14.4" or {});
  };
  features_.syn."0.14.4" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "${deps.syn."0.14.4".proc_macro2}"."proc-macro" =
        (f.proc_macro2."${deps.syn."0.14.4".proc_macro2}"."proc-macro" or false) ||
        (syn."0.14.4"."proc-macro" or false) ||
        (f."syn"."0.14.4"."proc-macro" or false); }
      { "${deps.syn."0.14.4".proc_macro2}".default = (f.proc_macro2."${deps.syn."0.14.4".proc_macro2}".default or false); }
    ];
    quote = fold recursiveUpdate {} [
      { "${deps.syn."0.14.4".quote}"."proc-macro" =
        (f.quote."${deps.syn."0.14.4".quote}"."proc-macro" or false) ||
        (syn."0.14.4"."proc-macro" or false) ||
        (f."syn"."0.14.4"."proc-macro" or false); }
      { "${deps.syn."0.14.4".quote}".default = (f.quote."${deps.syn."0.14.4".quote}".default or false); }
    ];
    syn = fold recursiveUpdate {} [
      { "0.14.4".clone-impls =
        (f.syn."0.14.4".clone-impls or false) ||
        (f.syn."0.14.4".default or false) ||
        (syn."0.14.4"."default" or false); }
      { "0.14.4".default = (f.syn."0.14.4".default or true); }
      { "0.14.4".derive =
        (f.syn."0.14.4".derive or false) ||
        (f.syn."0.14.4".default or false) ||
        (syn."0.14.4"."default" or false); }
      { "0.14.4".parsing =
        (f.syn."0.14.4".parsing or false) ||
        (f.syn."0.14.4".default or false) ||
        (syn."0.14.4"."default" or false); }
      { "0.14.4".printing =
        (f.syn."0.14.4".printing or false) ||
        (f.syn."0.14.4".default or false) ||
        (syn."0.14.4"."default" or false); }
      { "0.14.4".proc-macro =
        (f.syn."0.14.4".proc-macro or false) ||
        (f.syn."0.14.4".default or false) ||
        (syn."0.14.4"."default" or false); }
      { "0.14.4".quote =
        (f.syn."0.14.4".quote or false) ||
        (f.syn."0.14.4".printing or false) ||
        (syn."0.14.4"."printing" or false); }
    ];
    unicode_xid."${deps.syn."0.14.4".unicode_xid}".default = true;
  }) [
    (features_.proc_macro2."${deps."syn"."0.14.4"."proc_macro2"}" deps)
    (features_.quote."${deps."syn"."0.14.4"."quote"}" deps)
    (features_.unicode_xid."${deps."syn"."0.14.4"."unicode_xid"}" deps)
  ];


# end
# termion-1.5.1

  crates.termion."1.5.1" = deps: { features?(features_.termion."1.5.1" deps {}) }: buildRustCrate {
    crateName = "termion";
    version = "1.5.1";
    authors = [ "ticki <Ticki@users.noreply.github.com>" "gycos <alexandre.bury@gmail.com>" "IGI-111 <igi-111@protonmail.com>" ];
    sha256 = "02gq4vd8iws1f3gjrgrgpajsk2bk43nds5acbbb4s8dvrdvr8nf1";
    dependencies = (if !(kernel == "redox") then mapFeatures features ([
      (crates."libc"."${deps."termion"."1.5.1"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."termion"."1.5.1"."redox_syscall"}" deps)
      (crates."redox_termios"."${deps."termion"."1.5.1"."redox_termios"}" deps)
    ]) else []);
  };
  features_.termion."1.5.1" = deps: f: updateFeatures f (rec {
    libc."${deps.termion."1.5.1".libc}".default = true;
    redox_syscall."${deps.termion."1.5.1".redox_syscall}".default = true;
    redox_termios."${deps.termion."1.5.1".redox_termios}".default = true;
    termion."1.5.1".default = (f.termion."1.5.1".default or true);
  }) [
    (features_.libc."${deps."termion"."1.5.1"."libc"}" deps)
    (features_.redox_syscall."${deps."termion"."1.5.1"."redox_syscall"}" deps)
    (features_.redox_termios."${deps."termion"."1.5.1"."redox_termios"}" deps)
  ];


# end
# textwrap-0.10.0

  crates.textwrap."0.10.0" = deps: { features?(features_.textwrap."0.10.0" deps {}) }: buildRustCrate {
    crateName = "textwrap";
    version = "0.10.0";
    authors = [ "Martin Geisler <martin@geisler.net>" ];
    sha256 = "1s8d5cna12smhgj0x2y1xphklyk2an1yzbadnj89p1vy5vnjpsas";
    dependencies = mapFeatures features ([
      (crates."unicode_width"."${deps."textwrap"."0.10.0"."unicode_width"}" deps)
    ]);
  };
  features_.textwrap."0.10.0" = deps: f: updateFeatures f (rec {
    textwrap."0.10.0".default = (f.textwrap."0.10.0".default or true);
    unicode_width."${deps.textwrap."0.10.0".unicode_width}".default = true;
  }) [
    (features_.unicode_width."${deps."textwrap"."0.10.0"."unicode_width"}" deps)
  ];


# end
# time-0.1.40

  crates.time."0.1.40" = deps: { features?(features_.time."0.1.40" deps {}) }: buildRustCrate {
    crateName = "time";
    version = "0.1.40";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0wgnbjamljz6bqxsd5axc4p2mmhkqfrryj4gf2yswjaxiw5dd01m";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."time"."0.1.40"."libc"}" deps)
    ])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."time"."0.1.40"."redox_syscall"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."time"."0.1.40"."winapi"}" deps)
    ]) else []);
  };
  features_.time."0.1.40" = deps: f: updateFeatures f (rec {
    libc."${deps.time."0.1.40".libc}".default = true;
    redox_syscall."${deps.time."0.1.40".redox_syscall}".default = true;
    time."0.1.40".default = (f.time."0.1.40".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.time."0.1.40".winapi}"."minwinbase" = true; }
      { "${deps.time."0.1.40".winapi}"."minwindef" = true; }
      { "${deps.time."0.1.40".winapi}"."ntdef" = true; }
      { "${deps.time."0.1.40".winapi}"."profileapi" = true; }
      { "${deps.time."0.1.40".winapi}"."std" = true; }
      { "${deps.time."0.1.40".winapi}"."sysinfoapi" = true; }
      { "${deps.time."0.1.40".winapi}"."timezoneapi" = true; }
      { "${deps.time."0.1.40".winapi}".default = true; }
    ];
  }) [
    (features_.libc."${deps."time"."0.1.40"."libc"}" deps)
    (features_.redox_syscall."${deps."time"."0.1.40"."redox_syscall"}" deps)
    (features_.winapi."${deps."time"."0.1.40"."winapi"}" deps)
  ];


# end
# unicode-width-0.1.5

  crates.unicode_width."0.1.5" = deps: { features?(features_.unicode_width."0.1.5" deps {}) }: buildRustCrate {
    crateName = "unicode-width";
    version = "0.1.5";
    authors = [ "kwantam <kwantam@gmail.com>" ];
    sha256 = "0886lc2aymwgy0lhavwn6s48ik3c61ykzzd3za6prgnw51j7bi4w";
    features = mkFeatures (features."unicode_width"."0.1.5" or {});
  };
  features_.unicode_width."0.1.5" = deps: f: updateFeatures f (rec {
    unicode_width."0.1.5".default = (f.unicode_width."0.1.5".default or true);
  }) [];


# end
# unicode-xid-0.1.0

  crates.unicode_xid."0.1.0" = deps: { features?(features_.unicode_xid."0.1.0" deps {}) }: buildRustCrate {
    crateName = "unicode-xid";
    version = "0.1.0";
    authors = [ "erick.tryzelaar <erick.tryzelaar@gmail.com>" "kwantam <kwantam@gmail.com>" ];
    sha256 = "05wdmwlfzxhq3nhsxn6wx4q8dhxzzfb9szsz6wiw092m1rjj01zj";
    features = mkFeatures (features."unicode_xid"."0.1.0" or {});
  };
  features_.unicode_xid."0.1.0" = deps: f: updateFeatures f (rec {
    unicode_xid."0.1.0".default = (f.unicode_xid."0.1.0".default or true);
  }) [];


# end
# untrusted-0.6.2

  crates.untrusted."0.6.2" = deps: { features?(features_.untrusted."0.6.2" deps {}) }: buildRustCrate {
    crateName = "untrusted";
    version = "0.6.2";
    authors = [ "Brian Smith <brian@briansmith.org>" ];
    sha256 = "189ir1h2xgb290bhjchwczr9ygia1f3ipsydf6pwnnb95lb8fihg";
    libPath = "src/untrusted.rs";
  };
  features_.untrusted."0.6.2" = deps: f: updateFeatures f (rec {
    untrusted."0.6.2".default = (f.untrusted."0.6.2".default or true);
  }) [];


# end
# uuid-0.6.5

  crates.uuid."0.6.5" = deps: { features?(features_.uuid."0.6.5" deps {}) }: buildRustCrate {
    crateName = "uuid";
    version = "0.6.5";
    authors = [ "Ashley Mannix<ashleymannix@live.com.au>" "Christopher Armstrong" "Dylan DPC<dylan.dpc@gmail.com>" "Hunar Roop Kahlon<hunar.roop@gmail.com>" ];
    sha256 = "1jy15m4yxxwma0jsy070garhbgfprky23i77rawjkk75vqhnnhlf";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."uuid"."0.6.5"."cfg_if"}" deps)
    ]
      ++ (if features.uuid."0.6.5".rand or false then [ (crates.rand."${deps."uuid"."0.6.5".rand}" deps) ] else []));
    features = mkFeatures (features."uuid"."0.6.5" or {});
  };
  features_.uuid."0.6.5" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.uuid."0.6.5".cfg_if}".default = true;
    rand."${deps.uuid."0.6.5".rand}".default = true;
    uuid = fold recursiveUpdate {} [
      { "0.6.5".byteorder =
        (f.uuid."0.6.5".byteorder or false) ||
        (f.uuid."0.6.5".u128 or false) ||
        (uuid."0.6.5"."u128" or false); }
      { "0.6.5".default = (f.uuid."0.6.5".default or true); }
      { "0.6.5".md5 =
        (f.uuid."0.6.5".md5 or false) ||
        (f.uuid."0.6.5".v3 or false) ||
        (uuid."0.6.5"."v3" or false); }
      { "0.6.5".nightly =
        (f.uuid."0.6.5".nightly or false) ||
        (f.uuid."0.6.5".const_fn or false) ||
        (uuid."0.6.5"."const_fn" or false); }
      { "0.6.5".rand =
        (f.uuid."0.6.5".rand or false) ||
        (f.uuid."0.6.5".v3 or false) ||
        (uuid."0.6.5"."v3" or false) ||
        (f.uuid."0.6.5".v4 or false) ||
        (uuid."0.6.5"."v4" or false) ||
        (f.uuid."0.6.5".v5 or false) ||
        (uuid."0.6.5"."v5" or false); }
      { "0.6.5".sha1 =
        (f.uuid."0.6.5".sha1 or false) ||
        (f.uuid."0.6.5".v5 or false) ||
        (uuid."0.6.5"."v5" or false); }
      { "0.6.5".std =
        (f.uuid."0.6.5".std or false) ||
        (f.uuid."0.6.5".default or false) ||
        (uuid."0.6.5"."default" or false) ||
        (f.uuid."0.6.5".use_std or false) ||
        (uuid."0.6.5"."use_std" or false); }
    ];
  }) [
    (features_.cfg_if."${deps."uuid"."0.6.5"."cfg_if"}" deps)
    (features_.rand."${deps."uuid"."0.6.5"."rand"}" deps)
  ];


# end
# vec_map-0.8.1

  crates.vec_map."0.8.1" = deps: { features?(features_.vec_map."0.8.1" deps {}) }: buildRustCrate {
    crateName = "vec_map";
    version = "0.8.1";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" "Jorge Aparicio <japaricious@gmail.com>" "Alexis Beingessner <a.beingessner@gmail.com>" "Brian Anderson <>" "tbu- <>" "Manish Goregaokar <>" "Aaron Turon <aturon@mozilla.com>" "Adolfo Ochagavía <>" "Niko Matsakis <>" "Steven Fackler <>" "Chase Southwood <csouth3@illinois.edu>" "Eduard Burtescu <>" "Florian Wilkens <>" "Félix Raimundo <>" "Tibor Benke <>" "Markus Siemens <markus@m-siemens.de>" "Josh Branchaud <jbranchaud@gmail.com>" "Huon Wilson <dbau.pp@gmail.com>" "Corey Farwell <coref@rwell.org>" "Aaron Liblong <>" "Nick Cameron <nrc@ncameron.org>" "Patrick Walton <pcwalton@mimiga.net>" "Felix S Klock II <>" "Andrew Paseltiner <apaseltiner@gmail.com>" "Sean McArthur <sean.monstar@gmail.com>" "Vadim Petrochenkov <>" ];
    sha256 = "1jj2nrg8h3l53d43rwkpkikq5a5x15ms4rf1rw92hp5lrqhi8mpi";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."vec_map"."0.8.1" or {});
  };
  features_.vec_map."0.8.1" = deps: f: updateFeatures f (rec {
    vec_map = fold recursiveUpdate {} [
      { "0.8.1".default = (f.vec_map."0.8.1".default or true); }
      { "0.8.1".serde =
        (f.vec_map."0.8.1".serde or false) ||
        (f.vec_map."0.8.1".eders or false) ||
        (vec_map."0.8.1"."eders" or false); }
    ];
  }) [];


# end
# version_check-0.1.5

  crates.version_check."0.1.5" = deps: { features?(features_.version_check."0.1.5" deps {}) }: buildRustCrate {
    crateName = "version_check";
    version = "0.1.5";
    authors = [ "Sergio Benitez <sb@sergio.bz>" ];
    sha256 = "1yrx9xblmwbafw2firxyqbj8f771kkzfd24n3q7xgwiqyhi0y8qd";
  };
  features_.version_check."0.1.5" = deps: f: updateFeatures f (rec {
    version_check."0.1.5".default = (f.version_check."0.1.5".default or true);
  }) [];


# end
# winapi-0.3.5

  crates.winapi."0.3.5" = deps: { features?(features_.winapi."0.3.5" deps {}) }: buildRustCrate {
    crateName = "winapi";
    version = "0.3.5";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0cfdsxa5yf832r5i2z7dhdvnryyvhfp3nb32gpcaq502zgjdm3w6";
    build = "build.rs";
    dependencies = (if kernel == "i686-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_i686_pc_windows_gnu"."${deps."winapi"."0.3.5"."winapi_i686_pc_windows_gnu"}" deps)
    ]) else [])
      ++ (if kernel == "x86_64-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_x86_64_pc_windows_gnu"."${deps."winapi"."0.3.5"."winapi_x86_64_pc_windows_gnu"}" deps)
    ]) else []);
    features = mkFeatures (features."winapi"."0.3.5" or {});
  };
  features_.winapi."0.3.5" = deps: f: updateFeatures f (rec {
    winapi."0.3.5".default = (f.winapi."0.3.5".default or true);
    winapi_i686_pc_windows_gnu."${deps.winapi."0.3.5".winapi_i686_pc_windows_gnu}".default = true;
    winapi_x86_64_pc_windows_gnu."${deps.winapi."0.3.5".winapi_x86_64_pc_windows_gnu}".default = true;
  }) [
    (features_.winapi_i686_pc_windows_gnu."${deps."winapi"."0.3.5"."winapi_i686_pc_windows_gnu"}" deps)
    (features_.winapi_x86_64_pc_windows_gnu."${deps."winapi"."0.3.5"."winapi_x86_64_pc_windows_gnu"}" deps)
  ];


# end
# winapi-i686-pc-windows-gnu-0.4.0

  crates.winapi_i686_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_i686_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-i686-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "05ihkij18r4gamjpxj4gra24514can762imjzlmak5wlzidplzrp";
    build = "build.rs";
  };
  features_.winapi_i686_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_i686_pc_windows_gnu."0.4.0".default = (f.winapi_i686_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
# winapi-x86_64-pc-windows-gnu-0.4.0

  crates.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_x86_64_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-x86_64-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0n1ylmlsb8yg1v583i4xy0qmqg42275flvbc51hdqjjfjcl9vlbj";
    build = "build.rs";
  };
  features_.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_x86_64_pc_windows_gnu."0.4.0".default = (f.winapi_x86_64_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
# yaml-rust-0.4.0

  crates.yaml_rust."0.4.0" = deps: { features?(features_.yaml_rust."0.4.0" deps {}) }: buildRustCrate {
    crateName = "yaml-rust";
    version = "0.4.0";
    authors = [ "Yuheng Chen <yuhengchen@sensetime.com>" ];
    sha256 = "1mqv1jagn9hfym28ypp2dq6hw9kcyilzil9ydlpls0ivb8d9i3h8";
    dependencies = mapFeatures features ([
      (crates."linked_hash_map"."${deps."yaml_rust"."0.4.0"."linked_hash_map"}" deps)
    ]);
  };
  features_.yaml_rust."0.4.0" = deps: f: updateFeatures f (rec {
    linked_hash_map."${deps.yaml_rust."0.4.0".linked_hash_map}".default = true;
    yaml_rust."0.4.0".default = (f.yaml_rust."0.4.0".default or true);
  }) [
    (features_.linked_hash_map."${deps."yaml_rust"."0.4.0"."linked_hash_map"}" deps)
  ];


# end
}
