{ inputs, self, ... }:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];
  perSystem = { pkgs, ... }: {
    pre-commit = {
      pkgs = pkgs.extend (_: _: {
        cljfmt = null; # git-hooks.nix requires this (even if unused) but our nixpkgs isn't recent enough
      });
      settings = {
        default_stages = [ "commit" "push" ];
        rootSrc = self.outPath;
        hooks = {
          cabal-fmt.enable = true;
          deadnix.enable = true;
          fourmolu.enable = true;
          hlint.enable = true;
          nixpkgs-fmt.enable = true;
          typos =
            {
              settings.configPath = ".typos.toml";
              enable = false;
            };
        };
      };
    };
  };
}
