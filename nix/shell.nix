{ flake, pkgs }:
let
  inputShell = pkgs.mkShell {
    buildInputs = flake.pre-commit.settings.enabledPackages ++ [ pkgs.haskellPackages.apply-refact ];
    shellHook = flake.pre-commit.installationScript;
  };
in
{
  withHoogle = true;
  additional = ps: with ps; [
    ytxp-plutarch
    ytxp-sdk
    plutarch
    plutus-core
    plutus-tx
    plutus-ledger-api
  ];
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    ghcid = "latest";
  };
  inputsFrom = [ inputShell ];
}
