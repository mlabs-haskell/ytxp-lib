{ inputs, self, ... }:
{
  imports = [
    inputs.git-hooks.flakeModule
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
          typos.enable = true;
          markdownlint.enable = true;
          check-json.enable = true;
        };
      };
    };
  };
}
