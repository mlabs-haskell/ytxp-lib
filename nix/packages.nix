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
    in
    {
      packages = with flake.packages; {
        inherit "ytxp-plutarch:lib:ytxp-plutarch" "ytxp-plutarch:test:serialization" "ytxp-plutarch:exe:write-config";
        docs = import combine-haddock { inherit pkgs lib; } {
          cabalProject = pkgs.ytxp-plutarch;
          targetPackages = [
            "ytxp-plutarch"
          ];
          prologue = ''
            = Plutarch Documentation
            Documentation of Plutarch /and/ Documentation of Plutus libraries.
          '';
        };
        serve-docs = pkgs.writeShellApplication {
          name = "serve-docs";
          runtimeInputs = [ ];
          text = "${lib.getExe pkgs.python3} -m http.server -d ${config.packages.doc}/share/doc 8284";
        };
      };
      inherit (flake) devShells;
    };

}
