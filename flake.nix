{
  description = "A prototype front-end of Smart § Spec";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShellNoCC {
          name = "nix-shell-for-sofa";
          buildInputs = with pkgs; [
            dhall-lsp-server
            nodePackages.purescript-language-server
            purescript
            spago
            yarn
          ];
        };
      });
}
