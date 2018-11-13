{ pkgs ? import ./closure.nix, shell ? false }: with pkgs;

stackToNix {
  root = constGitIgnore "blockchain-util-src" ./. [
    "*.nix"
    "/.buildkite"
    "/config.yaml"
    "/demo.sh"
    "/docs"
    "/paper"
    "/readme.md"
    "Makefile"
    "start.sh"
    "test-cases"
  ];
  inherit shell;
}
