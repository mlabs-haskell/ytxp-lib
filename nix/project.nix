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
      "https://github.com/mlabs-haskell/plutus-test"."648adbd910e7e9ae0efb082bc936eb304c610ef0" = "sha256-YVHoY4Pg+v0S+2isuEpZ5Krg9o3+wQcWX9zo6DtgDb8=";
      "https://github.com/mlabs-haskell/ply"."b24c919a0cddc3a3a2ace4c88f8a185db01bca04" = "sha256-wXklqWiB/zB+97A3WcdPJfFFSqiPr+4X7EZXBYesUck=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."52d8e56f7eb347b76e60eeac273121534e8b0021" = "sha256-lcrbvqJD3J+yxafVoIQsp4hFUbqc1eWj5s2YTrSUEnw=";
      "https://github.com/Plutonomicon/plutarch-plutus"."f84a46287b06f36abf8d2d63bec7ff75d32f3e91" = "sha256-gKBk9D6DHSEudq7P9+07yXWcgM/QX7NFp0tJXBodopM=";
    };
  };
}
