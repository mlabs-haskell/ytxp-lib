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
      "https://github.com/mlabs-haskell/ytxp-sdk"."d4e52d53eab5d4c8b62efd72a35f3d4dc5b6bc43" = "sha256-6hO304XvHJRC2tw0hRcRR13QWGcO6z7E+HvkLKo9r2Q=";
    };
  };
}
