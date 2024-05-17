{ flake, pkgs }:
let
  inputShell = pkgs.mkShell {
    buildInputs = flake.pre-commit.settings.enabledPackages ++ [ pkgs.haskellPackages.apply-refact ];
    shellHook = flake.pre-commit.installationScript;
  };
in
{
  withHoogle = true;
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    ghcid = "latest";
  };
  inputsFrom = [ inputShell ];
}
