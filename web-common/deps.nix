{ system
, config
, pkgs
, nixGitIgnore
, easyPS
, baseDirectory
}:
let

  deps = pkgs.stdenv.mkDerivation {
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
      yarn
      yarn2nix > yarn.nix
      export HOME=$(pwd)
      # spago init
      # spago list-packages -f transitive -j
      # spago -v sources
      # spago2nix generate
      '';
      installPhase = ''
      mkdir -p $out
      cp yarn.lock $out/
      cp -r node_modules $out/
      cp yarn.nix $out/
      '';
  };
in
    pkgs.writeScript "update-client-deps" ''
      #!${pkgs.runtimeShell}

      set -eou pipefail

      export PATH=${pkgs.stdenv.lib.makeBinPath [
        pkgs.coreutils
        pkgs.git
        pkgs.findutils
        pkgs.nodejs-10_x
        easyPS.purs
        easyPS.psc-package
        easyPS.spago
        easyPS.spago2nix
      ]}
      if [ ! -f package.json ]
      then
          echo "package.json not found. Please run this script from the client directory." >&2
          exit 1
      fi

      echo Remove old JavaScript Dependencies
      rm -Rf node_modules : true

      echo Installing JavaScript Dependencies
      mkdir node_modules
      cp -R ${deps}/node_modules/* ./node_modules
      chown -R `whoami` node_modules
      chmod -R +w node_modules

      cat ${deps}/yarn.lock > yarn.lock
      cat ${deps}/yarn.nix > yarn.nix

      echo Generate nix files
      spago2nix generate

      echo Done
    ''