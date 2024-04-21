{
  description = "Yielding Transaction Pattern Library";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    nixpkgs.follows = "iohk-nix/nixpkgs";
    plutarch.url = "github:Plutonomicon/plutarch-plutus";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, haskellNix, flake-parts, iohk-nix, CHaP, plutarch, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        ./nix/packages.nix
      ];
    };
}
