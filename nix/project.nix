final: _prev: {
  ytxp-plutarch = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc96";
    src = final.lib.cleanSource ./../ytxp-plutarch;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
    sha256map = {
      "https://github.com/Plutonomicon/plutarch-plutus"."7efbb9fc9f358cfbfd89ff245d85632a17ff3fb3" = "sha256-S7UQrcBO+6G77O4OAAMfQNrChuGzfTr9ARovjAcP7eg=";
      "https://github.com/input-output-hk/xsy-liqwid-libs"."01dcaf60ee5386f0975aed908d6f92ab0e9b34e0" = "sha256-RK3yytUxIFbceU+m6MxvXyYRrWvDnhx7xmO3UcOzgKc=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."0c6b15be554cd3020a67e7c7507cecbf93e72dad" = "sha256-3hkqrH4+dghrMxEq/sCs2EnhoZQD5BxZ2tNjl34BayQ=";
    };
  };
}
