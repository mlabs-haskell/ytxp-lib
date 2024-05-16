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
          # TODO this is not doing the expected this
          inherit (inputs.nixpkgs-unstable) cljfmt;
          flake = config;
        })
      ];
      pkgs = config._module.args.pkgs.extend (lib.composeManyExtensions overlays);
      flake = pkgs.ytxp-plutarch.flake { };
    in
    {
      packages = with flake.packages; {
        inherit "ytxp-plutarch:lib:ytxp-plutarch" "ytxp-plutarch:test:serialization" "ytxp-plutarch:exe:write-config";
      };
      inherit (flake) devShells;
    };
}
