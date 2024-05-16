{ flake, pkgs }:
let
  inputShell = pkgs.mkShell {
    buildInputs = flake.pre-commit.settings.enabledPackages;
    shellHook = flake.pre-commit.installationScript;
  };
in
{
  withHoogle = true;
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
  };
  inputsFrom = [ inputShell ];
}
