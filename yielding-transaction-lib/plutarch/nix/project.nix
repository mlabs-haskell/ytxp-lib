{ pkgs, lib, inputs }:
pkgs.haskell-nix.cabalProject'
{
  sha256map = { "https://github.com/Plutonomicon/plutarch-plutus"."7efbb9fc9f358cfbfd89ff245d85632a17ff3fb3" = "sha256-S7UQrcBO+6G77O4OAAMfQNrChuGzfTr9ARovjAcP7eg="; };
  compiler-nix-name = "ghc963";
  src = lib.cleanSource ./../ytxp-plutarch;
  shell = {
    withHoogle = true;
    tools = {
      cabal = "latest";
      hlint = "latest";
      haskell-language-server = "latest";
    };
  };
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
    "https://github.com/Plutonomicon/plutarch-plutus.git" = inputs.plutarch;
  };
}
