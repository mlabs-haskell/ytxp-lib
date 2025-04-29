final: _prev: {
  ytxp-plutarch = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc96";
    src = final.lib.cleanSource ./..;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
    sha256map = {
      "https://github.com/mlabs-haskell/ply"."1e87c0cd8ae64f6913fb0ec31ddb6c70edde6678" = "sha256-XofLt0+OyE3IisBh6faHqsJFTPEHODSnioHOhntdPDQ=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."c589c23c366edc883196b0f4814cf79e3ab61130" = "sha256-6dZf+Lyf2bR2MQHXj/MI/7PRl25CkTKf7m4YkTazlok=";
      "https://github.com/Plutonomicon/plutarch-plutus"."93881d63bfe02ea12329fe3d5521935bb7dce18c" = "sha256-WSESLdKZttm2EBNv5SY6sZXT+ks87rY3y0gatf1m3Lo=";
    };
  };
}
