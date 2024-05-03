{ inputs, ... }:
{
  perSystem = { system, lib, config, ... }:
    let
      overlays = [
        inputs.haskellNix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
        (_final: _prev: {
          ytxp-plutarch = import ./project.nix {
            inherit pkgs lib inputs;
          };
        })
      ];
      pkgs = import inputs.nixpkgs {
        inherit system overlays; inherit (inputs.haskellNix) config;
      };
      flake = pkgs.ytxp-plutarch.flake { };
    in
    {
      packages = {
        ytxp-plutarch-lib = flake.packages."ytxp-plutarch:lib:ytxp-plutarch";
        ytxp-plutarch-test-serialization = flake.packages."ytxp-plutarch:test:serialization";
        ytxp-plutarch-write-config = flake.packages."ytxp-plutarch:exe:write-config";
      };
      devShells = flake.devShells;
    };
}
