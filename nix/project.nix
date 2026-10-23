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
      "https://github.com/mlabs-haskell/plutus-test"."2c54dd73c2f5c8f48102efc39dceb8756db3d664" = "sha256-cgBJzs3ZFZT/JnBr6z/rYmMXWebyNWbvo4e4Qj4CniQ=";
      "https://github.com/mlabs-haskell/ply"."98a7f0e236e9b0a8b4bc2f8dcba2a6eaacd24ae1" = "sha256-GY8UcL34ooHkyvBiZoqRhdI1PcrVr2XdpMmbus6GuEI=";
      "https://github.com/Plutonomicon/plutarch-plutus"."7237f3982e47f447d27ba0a07f91503566e01d80" = "sha256-Wm/cPFbRAI4NJqD/z0B4IsiViMUhrMG2REuARRitwAs=";
    };
  };

  ytxp-sdk = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc96";
    src = final.lib.cleanSource ./..;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
  };
}
