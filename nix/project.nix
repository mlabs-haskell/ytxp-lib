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
      "https://github.com/mlabs-haskell/ply"."1e87c0cd8ae64f6913fb0ec31ddb6c70edde6678" = "sha256-XofLt0+OyE3IisBh6faHqsJFTPEHODSnioHOhntdPDQ=";
      "https://github.com/mlabs-haskell/ytxp-sdk"."8aff5092a058e2b5eaa3da6dc8ec2a89bfa39e66" = "sha256-R3+1PdSmykWor87NH+THMRI5gqi2FoR6CI+j4qk4JGk=";
      "https://github.com/Plutonomicon/plutarch-plutus"."f84a46287b06f36abf8d2d63bec7ff75d32f3e91" = "sha256-gKBk9D6DHSEudq7P9+07yXWcgM/QX7NFp0tJXBodopM=";
    };
  };
}
