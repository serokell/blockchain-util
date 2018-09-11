let
  nixpkgs = fetchGit {
    url = https://github.com/serokell/nixpkgs;
    rev = "4089219f345ce7bd0c3bd579d9190e526ea50b52";
  };
in

import nixpkgs {}
