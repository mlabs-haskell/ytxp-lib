{ pkgs }:
let
  inputShell = pkgs.mkShell {
    packages = [
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
}
