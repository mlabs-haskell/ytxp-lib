final: _prev: {
  ytxp-plutarch = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc965";
    src = final.lib.cleanSource ./..;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
    sha256map = {
      "https://github.com/j-mueller/sc-tools"."188933df37fdcc11e6b91f83a7acdbeac4a09c2f" = "sha256-Lv3jjXdlwGxG05rckpgolp6ruWkO76E/xpEdpBVyBEA=";
      "https://github.com/input-output-hk/xsy-liqwid-libs"."d63f0d52bc1c5d55c7056fdb6f01a08d49eced95" = "sha256-VWWE07vuZIJDt55EjICA/dUo2jXLL68PDJ46zs9Obbo=";
      "https://github.com/mlabs-haskell/ply"."7fb83df1397eba057d00fd03f0af04a61512d9ef" = "sha256-G9zKpDgYhcFAejwI8lZ0pQnHm+WOlQNXZ1146+FwmQQ=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."4e741f5a22a1cb6c6f64aca1797986189ebb93b9" = "sha256-TuKtwLD8q41f10SOsFozmxstsUwPLF34yyWgmiwyt2Q=";
      "https://github.com/Plutonomicon/plutarch-plutus"."7913e2d883530f569b16c02878989d3394bab727" = "sha256-Vg0U0QHolNHhBH2EaMHmFxeMt+Mv+thbY58PuVpWRlQ=";
    };
  };
}
