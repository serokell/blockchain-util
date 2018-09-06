let
  overlay = import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git"}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
  };
in

with nixpkgs;

buildStackApplication {
  packages = [ "snowdrop-block" "snowdrop-core" "snowdrop-execution" "snowdrop-util" ];
  src = lib.cleanSource ./.;
  ghc = pkgs.haskell.compiler.ghc822;
  
  #overrides = final: previous: with haskell.lib; {
  #  snowdrop = haskell.lib.doCheck (overrideCabal previous.snowdrop (super: with final; {
  #    buildDepends = (super.buildDepends or []) ++ [ hspec tasty-hedgehog QuickCheck ];
  #    buildTools = [ cpphs ];
  #  }));
  #};
}
