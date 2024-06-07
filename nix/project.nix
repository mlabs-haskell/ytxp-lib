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
      "https://github.com/Plutonomicon/plutarch-plutus"."fcdd2209433d8b8979e820dc4fa9aad5f202216d" = "sha256-gQwaYGIds5owHivXi+ktH7CGeBqoLBykVxyHZZiDUM4=";
      "https://github.com/input-output-hk/xsy-liqwid-libs"."f46485d8796db37a5b3e7fe90a830355504f4edb" = "sha256-2t843Fmn/jMIPtcgpC3drumzqHu3qWK5geFTCndjJp0=";
      "https://github.com/mlabs-haskell/ply"."a0aa36863372a3375e0ec73dd0ccfa49046d7855" = "sha256-dSXC40u8taswmygFDuGDLkdRj/ebsnk/hsJM5JAmWE4=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."d53079f5f6c9e1da5f64dd8b95738b64dfe119d0" = "sha256-JyXNI4sz+NEXhk5rC/MbQwgBT+zaFYpAXDZo81yfxx0=";
    };
  };
}
