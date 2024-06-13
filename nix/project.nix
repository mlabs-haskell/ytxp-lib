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
      "https://github.com/input-output-hk/xsy-liqwid-libs"."2425031da690546dd64669975598a99d1279eb16" = "sha256-JQ0Ua6jTgppmfKuWufWSTMg7w/yRpQUwVi8zJ6kx9AI=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."7c586bfb9d35b095bb71db6d0709fc677c676349" = "sha256-TB6WJuj6bJ5VCv4sk/kGiU2JEmLC2eLeTzCpnrJp1ZY=";
      "https://github.com/albertodvp/sc-tools"."91d197203f1255b6e33ef3cb11513b672250f48b" = "sha256-KeP5FjOygJxjPAasCdbv0tKabnxlZ4g/VkO/MNPx2YI=";
    };
  };
}
