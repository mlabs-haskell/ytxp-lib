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
}
