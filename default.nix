let
  nixpkgs = fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz";
  serokell-overlay = fetchGit "ssh://git@github.com/serokell/serokell-overlay";
in

with import nixpkgs {
  config.allowUnfree = true;
  overlays = [ (import "${serokell-overlay}/pkgs") ];
};

let
  closure = (stackClosure haskell.compiler.ghc822 ./.);
in

{ inherit (closure) snowdrop-block snowdrop-core snowdrop-execution snowdrop-util; }
