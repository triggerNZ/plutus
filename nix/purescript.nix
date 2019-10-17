{ stdenv
, pkgs
, fetchurl
, psSrc
, easyPS
, yarn2nix
, nodejsHeaders
, src
, webCommonPath
, packages
, spagoPackages
, name
, packageJSON
, yarnLock
, yarnNix
}:

with pkgs;

let
  # node-sass is terrible and we have to get it its binaries otherwise it will try to build them
  nodeSassBinLinux = fetchurl {
    url = "https://github.com/sass/node-sass/releases/download/v4.11.0/linux-x64-64_binding.node";
    sha256 = "0dl91l414na44h090cgghd06q0j2whlj9h98im2qb9823glq7xff";
  };
  nodeSassBinDarwin = fetchurl {
    url = "https://github.com/sass/node-sass/releases/download/v4.11.0/darwin-x64-64_binding.node";
    sha256 = "1p5gz1694vxar81hbrrbdmmr2wjw3ksfvfgwh0kzzgjkc2dpk5pa";
  };
  webCommon = pkgs.copyPathToStore webCommonPath;

  packagesJson = "${src}/packages.json";

in yarn2nix.mkYarnPackage {
  inherit name src packageJSON yarnLock yarnNix;
  nodejs = nodejs-10_x;

  pkgConfig = {
    "libxmljs" = {
      buildInputs = [ nodejs-10_x nodePackages_10_x.node-gyp python2 ];
      postInstall = ''
        node-gyp --tarball ${nodejsHeaders} rebuild
      '';
    };
  };

  buildInputs = [ cacert webCommon ];

  nativeBuildInputs = [ git easyPS.purs easyPS.spago easyPS.psc-package nodePackages_10_x.node-gyp nodejs-10_x python2 ];

  buildPhase = ''
    export HOME=$NIX_BUILD_TOP
    export SASS_BINARY_PATH=${if stdenv.isDarwin then nodeSassBinDarwin else nodeSassBinLinux}

    # mkYarnPackage moves everything into deps/${name}
    shopt -s extglob
    mkdir deps
    mv !(deps) deps/
    cd deps

    # move everything into its correct place
    cp -R ${psSrc} generated
    cp -R ${webCommon} ../web-common
    sh ${spagoPackages.installSpagoStyle}

    # Compile everything.
    # This should just be `spago --no-psa build`, but there's a bug in spago that means it would go to the internet.
    # When that changes, update.
    purs compile \
      'src/**/*.purs' \
      'test/**/*.purs' \
      'generated/**/*.purs' \
      '../web-common/**/*.purs' \
      '.spago/*/*/src/**/*.purs'

    # Build the frontend.
    yarn --offline webpack
  '';

  doCheck = true;

  checkPhase = ''
    node -e 'require("./output/Test.Main").main()'
  '';

  distPhase = ''
    true
  '';

  installPhase = ''
    mv dist $out
  '';
}
