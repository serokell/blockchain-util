{ pkgs ? import ./nixpkgs.nix }:

let
  stack4nix = fetchGit {
    url = https://github.com/serokell/stack4nix;
    rev = "dee5f58317a0f067c6adfceed4d710e53d56cac5";
  };

  buildStackProject = import stack4nix { inherit pkgs; };
in

buildStackProject ./.
