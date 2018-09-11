let
  nixpkgs = fetchGit {
    url = https://github.com/serokell/nixpkgs;
    ref = "20180911.173158";
    rev = "4089219f345ce7bd0c3bd579d9190e526ea50b52";
  };
in

import nixpkgs {}
