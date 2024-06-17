{ inputs, ... }:
{
  perSystem = { lib, config, ... }:
    let
      overlays = [
        inputs.haskellNix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
        (import ./project.nix)
        (_: _: {
          inherit inputs;
          flake = config;
        })
      ];
      pkgs = config._module.args.pkgs.extend (lib.composeManyExtensions overlays);
      flake = pkgs.ytxp-plutarch.flake { };
      combine-haddock = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/Plutonomicon/plutarch-plutus/f78b70eca8841a4a009cb6791a59c999cbc68745/nix/combine-haddock.nix";
        sha256 = "sha256-Th7HnBErgCdwwdszZ8gQz94V87gqEbzHAqN7QhRROMc=";
      };
    in
    {
      packages = with flake.packages; {
        inherit "ytxp-plutarch:lib:ytxp-plutarch" "ytxp-plutarch:test:ytxp-lib-test" "ytxp-plutarch:exe:export";
        docs = import combine-haddock { inherit pkgs lib; } {
          cabalProject = pkgs.ytxp-plutarch;
          targetPackages = [
            "ytxp-plutarch"
          ];
          prologue = ''
            = YTxP Documentation
            Documentation of the onchain implementation of the YTxP library
          '';
        };
        serve-docs = pkgs.writeShellApplication {
          name = "serve-docs";
          runtimeInputs = [ ];
          text = "${lib.getExe pkgs.python3} -m http.server -d ${config.packages.docs}/share/doc 8284";
        };
      };
      inherit (flake) devShells;
    };

}
