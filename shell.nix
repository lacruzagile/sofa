{ pkgs ? import ../nix/packages.nix { } }:

pkgs.mkShellNoCC {
  name = "nix-shell-for-purssmartspec";
  buildInputs = with pkgs; [
    dhall-lsp-server
    nodePackages.purescript-language-server
    nodejs
    purescript
    purty
    spago
    spago2nix
    yarn
    yarn2nix
  ];
}
