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
      "https://github.com/j-mueller/sc-tools"."e2e3add9d7823b97326fd42d55550bc1b4f9cdbb" = "sha256-dx83vFVvhWvcQgwYJTAmCaiCpDfwddyfAN7vCX/jYOg=";
      "https://github.com/input-output-hk/xsy-liqwid-libs"."f46485d8796db37a5b3e7fe90a830355504f4edb" = "sha256-2t843Fmn/jMIPtcgpC3drumzqHu3qWK5geFTCndjJp0=";
      "https://github.com/mlabs-haskell/ply"."a0aa36863372a3375e0ec73dd0ccfa49046d7855" = "sha256-dSXC40u8taswmygFDuGDLkdRj/ebsnk/hsJM5JAmWE4=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."4e741f5a22a1cb6c6f64aca1797986189ebb93b9" = "sha256-TuKtwLD8q41f10SOsFozmxstsUwPLF34yyWgmiwyt2Q=";
      "https://github.com/Plutonomicon/plutarch-plutus"."fcdd2209433d8b8979e820dc4fa9aad5f202216d" = "sha256-gQwaYGIds5owHivXi+ktH7CGeBqoLBykVxyHZZiDUM4=";
    };
  };
}
