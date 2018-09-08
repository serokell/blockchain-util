{ pkgs ? import ./nixpkgs.nix }:

let
  stack4nix = fetchGit {
    url = https://github.com/serokell/stack4nix;
    rev = "26a46991270749ecc57d5842d391435b314dc26f";
  };

  buildStackProject = import stack4nix { inherit pkgs; };
in

buildStackProject ./.
