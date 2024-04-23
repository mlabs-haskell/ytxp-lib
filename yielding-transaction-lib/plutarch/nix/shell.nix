{ pkgs }:
let
  inputShell = pkgs.mkShell {
    packages = with pkgs; [
      nixpkgs-fmt
      haskellPackages.apply-refact
    ];
  };
in
{
  withHoogle = true;
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
  };
  inputsFrom = [ inputShell ];
  # TODO (alberto 2024-04-23): this can't work in the current repository becase
  # we already have other pre-commit hooks installed
  # shellHook = ''
  #   ${config.pre-commit.installationScript}
  # '';
}
