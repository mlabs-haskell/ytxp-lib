{ inputs, ... }:
{
  imports = [
    inputs.flake-root.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { config
    , pkgs
    , ...
    }:
    {
      treefmt.config = {
        inherit (config.flake-root) projectRootFile;
        package = pkgs.treefmt;
        flakeFormatter = true;
        flakeCheck = true;
        programs = {
          cabal-fmt.enable = true;
          deadnix.enable = true;
          fourmolu.enable = true;
          hlint.enable = true;
          typos.enable = true;
        };
      };
    };
}
