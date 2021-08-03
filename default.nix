let sources = import ../nix/sources.nix;
in { pkgs ? import sources.nixpkgs { config = { }; } }:

let spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

in spagoPkgs.mkBuildProjectBundle {
  name = "smart-spec-web";
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  purs = pkgs.purescript;
}
