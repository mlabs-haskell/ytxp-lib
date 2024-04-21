{ inputs, ... }:
{
  perSystem = { self', system, lib, config, pkgs, ... }:
    let
      overlays = [
        inputs.haskellNix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
        (final: prev: {
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
        ytxp-plutarch-cli = flake.packages."ytxp-plutarch:exe:ytxp-plutarch-cli";
        # TODO
        # ytxp-plutarch-config = nixpkgs pkgs.stdenv.mkDerivation {
        #   name = "ytxp-plutarch-config";
        #   src = ./.;
        #   buildPhase = ''${config.packages.ytxp-plutarch-cli}/bin/ytxp-plutarch-cli compile'';
        #   installPhase = "cp ytxp-config.json $out";
        # };
      };
      devShells = flake.devShells;
    };
}
