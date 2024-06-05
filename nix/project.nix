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
      "https://github.com/input-output-hk/xsy-liqwid-libs"."a799bcca72bcd133cc25a7b6841acb48b3885138" = "sha256-ibAtsyejc3SOUWKoyxSQ5r3jg9eOfyEHkpO+qTrwa2U=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."988c3ba18aa2c6bc40c302a947356191273cc105" = "sha256-JyXNI4sz+NEXhk5rC/MbQwgBT+zaFYpAXDZo81yfxx0=";
      "https://github.com/albertodvp/sc-tools"."91d197203f1255b6e33ef3cb11513b672250f48b" = "sha256-KeP5FjOygJxjPAasCdbv0tKabnxlZ4g/VkO/MNPx2YI=";

    };
  };
}
