{ system
, config
, pkgs
, baseDirectory
}:
let

  nixGitIgnore = import (pkgs.fetchFromGitHub {
    owner = "siers";
    repo = "nix-gitignore";
    rev = "686b057f6c24857c8862c0ef15a6852caab809c7";
    sha256 = "1hv8jl7ppv0f8lnfx2qi2jmzc7b5yiy12yvd4waq9xmxhip1k7rb";
  }) { inherit (pkgs) lib runCommand; };

  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "cc7196bff3fdb5957aabfe22c3fa88267047fe88";
    sha256 = "1xfl7rnmmcm8qdlsfn3xjv91my6lirs5ysy01bmyblsl10y2z9iw";
  }) { pkgs = pkgs // { nix-gitignore = nixGitIgnore; }; };

  node_modules = pkgs.stdenv.mkDerivation {
    name = "node_modules";
    srcs = builtins.filterSource (path: type: baseNameOf path == "package.json") baseDirectory;
    buildInputs = [ pkgs.yarn 
                  ];
    buildPhase = ''
    yarn
    '';
    installPhase = ''
    mkdir -p $out
    cp yarn.lock $out/
    cp -r node_modules $out/
    '';
  };
in
pkgs.stdenv.mkDerivation {
    name = "deps";
    srcs = builtins.filterSource (path: type: pkgs.lib.elem (baseNameOf path) ["package.json" "spago.dhall" "packages.dhall"]) baseDirectory;
    buildInputs = [ 
                    pkgs.coreutils
                    pkgs.python2
                    pkgs.nodejs-10_x
                    pkgs.nodePackages_10_x.node-gyp
                    pkgs.yarn
                    pkgs.yarn2nix
                    easyPS.purs
                    easyPS.psc-package
                    easyPS.spago
                    easyPS.spago2nix
                    pkgs.which
                    pkgs.git
                  ];
    buildPhase = ''
    cp -r ${node_modules}/node_modules .
    cat ${node_modules}/yarn.lock > yarn.lock
    ls -la
    yarn2nix > yarn.nix
    echo "spago"
    export HOME=$(pwd)
    # spago init
    # spago list-packages -f transitive -j
    # spago -v sources

    # spago2nix generate
    '';
    installPhase = ''
    ls -la
    mkdir -p $out
    cp yarn.lock $out/
    cp -r node_modules $out/
    cp yarn.nix $out/
    '';
}