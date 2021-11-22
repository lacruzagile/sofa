let sources = import ../nix/sources.nix;
in { pkgs ? import sources.nixpkgs { config = { }; } }:

let

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  pursOutput = spagoPkgs.mkBuildProjectOutput {
    inherit src;
    purs = pkgs.purescript;
  };

in

pkgs.mkYarnPackage {
  name = "sofa";
  inherit src;
  packageJSON = ./package.json;
  yarnNix = ./yarn.nix;
  yarnLock = ./yarn.lock;

  nativeBuildInputs = [ pkgs.nodejs-12_x pkgs.purescript pkgs.spago ];

  buildPhase = ''
    cd deps/sofa
    ln -s ${pursOutput}/output .
    spago --global-cache skip bundle-app --no-install --no-build \
      --main Main --to sofa.js

    # Work around for https://github.com/purescript-contrib/purescript-affjax/issues/161
    sed -i 's/&& module.require /\&\& false /' sofa.js

    node node_modules/.bin/parcel build \
       --no-cache --no-source-maps --public-url ./ \
       sofa.html
  '';

  installPhase = ''
    mkdir $out
    ln -sv ${pursOutput}/output $out/purs_output
    cp -rv node_modules $out
    cp -rv dist $out
    rmdir $out/dist/v1alpha1
  '';

  distPhase = "true";

  meta = with pkgs.stdenv.lib; {
    description = "The Sinch SOFA SPA";
  };
}
