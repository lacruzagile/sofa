{ pkgs ? import ../nix/packages.nix }:

pkgs.mkShell {
  name = "nix-shell-for-purssmartspec";
  buildInputs = with pkgs; [
    dhall-lsp-server
    nodePackages.purescript-language-server
    nodejs
    purescript
    purty
    spago
  ];
}
