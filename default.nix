let sources = import ../nix/sources.nix;
in { pkgs ? import sources.nixpkgs { config = { }; } }:

let

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  pursOutput = spagoPkgs.mkBuildProjectOutput {
    inherit src;
    purs = pkgs.purescript;
  };

in pkgs.runCommand "sofa" { nativeBuildInputs = [ pkgs.purescript ]; } ''
  mkdir $out
  cp ${src}/sofa.{css,html} ${src}/picnic.min.css $out
  purs bundle -o $out/sofa.js -m Main ${pursOutput}/output/**/*.js
  echo 'PS["Main"].main();' >> $out/sofa.js
''
