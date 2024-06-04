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
      "https://github.com/mlabs-haskell/ytxp-sdk"."d4e52d53eab5d4c8b62efd72a35f3d4dc5b6bc43" = "sha256-6hO304XvHJRC2tw0hRcRR13QWGcO6z7E+HvkLKo9r2Q=";
      "https://github.com/albertodvp/sc-tools"."91d197203f1255b6e33ef3cb11513b672250f48b" = "sha256-KeP5FjOygJxjPAasCdbv0tKabnxlZ4g/VkO/MNPx2YI=";

    };
  };
}
