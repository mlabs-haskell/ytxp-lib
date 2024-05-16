{ inputs, self, ... }:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];
  perSystem = _: {
    pre-commit = {
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
              enable = true;
            };
        };
      };
    };
  };
}
