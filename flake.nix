{
  description = "Yielding Transaction Pattern Library";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    git-hooks.url = "github:cachix/git-hooks.nix";
    combine-haddock = {
      # Pinning to a specific commit for security to avoid potential malicious changes
      url = "https://raw.githubusercontent.com/Plutonomicon/plutarch-plutus/f78b70eca8841a4a009cb6791a59c999cbc68745/nix/combine-haddock.nix";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        ./nix/pre-commit.nix
        ./nix/packages.nix
        ./nix/checks.nix
      ];
    };
}
