{ inputs, ... }:
{
  perSystem = { config, system, pkgs, inputs', ... }:
    let
      hsFlake = inputs.flake-lang.lib."${system}".haskellPlutusFlake {
        src = ./.;

        name = "ytxp-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-extra"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-ytxp-plutarch = hsFlake.devShell;

      packages = {
        # WARN(bladyjoker): We have to pick the hsFlake.packages like this otherwise flake-parts goes into `infinite recursion`.
        ytxp-plutarch-lib = hsFlake.packages."ytxp-plutarch:lib:ytxp-plutarch";

        ytxp-plutarch-cli = hsFlake.packages."ytxp-plutarch:exe:ytxp-plutarch-cli";

        ytxp-plutarch-config = pkgs.stdenv.mkDerivation {
          name = "ytxp-plutarch-config";
          src = ./.;
          buildPhase = ''${config.packages.ytxp-plutarch-cli}/bin/ytxp-plutarch-cli compile'';
          installPhase = "cp ytxp-config.json $out";
        };
      };

      inherit (hsFlake) checks;
    };
}
