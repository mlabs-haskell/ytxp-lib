{
  description = "Yielding Transaction Pattern Library";
  inputs = {
    # NOTE(bladyjoker): Trying to reuse as many inputs from lbf and flake-lang as possible to reduce the flake.lock size which impacts performance significantly.
    nixpkgs.follows = "lbf/nixpkgs";

    # LambdaBuffers for sharing types
    lbf.url = "github:mlabs-haskell/lambda-buffers";

    # flake-lang.nix for monorepo setup
    flake-lang.follows = "lbf/flake-lang";

    # flake-parts for Flake modules
    flake-parts.follows = "lbf/flake-parts";

    # Code quality automation
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";

    # Cardano transaction library
    haskell-nix.follows = "lbf/flake-lang/haskell-nix";
    iohk-nix.follows = "lbf/flake-lang/iohk-nix";

    # Plutarch eDSL
    plutarch.follows = "lbf/plutarch";

    plutip.url = "github:mlabs-haskell/plutip/1bf0b547cd3689c727586abb8385c008fb2a3d1c";
    ogmios.url = "github:mlabs-haskell/ogmios-nixos/78e829e9ebd50c5891024dcd1004c2ac51facd80";

    # Liqwid Libs
    liqwid-libs.url = "github:Liqwid-Labs/liqwid-libs";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        #./pre-commit.nix
        #./api/build.nix
        ./ytxp-plutarch/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];

    };

}
